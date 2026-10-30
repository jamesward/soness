package com.jamesward.soness

import com.jamesward.zio_bedrock.BedrockMiddleware
import com.jamesward.zio_decisions.Decisions
import zio.*
import zio.json.*
import zio.logging.{ConsoleLoggerConfig, FileLoggerConfig, LogFilter, LogFormat, makeConsoleErrLogger, makeFileLogger}

import java.nio.file.{Path, Paths}

/**
 * soness's command line: `--debug` logs at debug level (every ACP message, prompt, tool call, Bedrock and Jev exchange,
 * in full), to stderr and to a file of its own ([[Logging.debugFile]]).
 */
final case class Options(debug: Boolean = false) derives CanEqual

object Options:
  val usage = "usage: soness [--debug]"

  def parse(args: List[String]): Either[String, Options] =
    args.find(_ != "--debug").fold(Right(Options(debug = args.nonEmpty)))(other => Left(s"unknown argument: $other\n$usage"))

/**
 * Logging. ACP has no logging channel and stdout carries the protocol, so the log goes to stderr, which ACP clients
 * capture (IntelliJ IDEA writes it to `idea.log`), and with `--debug` to a file as well. At debug level the log holds
 * prompts, tool input and output, and model output: everything the session sees.
 */
object Logging:

  /**
   * Where a `--debug` process's log goes: `SONESS_LOG_FILE` when it's set (the evals collect every run in one log), else
   * a file named for the process and when it started (`soness-2026-10-19T16-30-36.log`) in [[logsDir]].
   */
  def debugFile(name: String): Path =
    sys.env.get("SONESS_LOG_FILE").filter(_.trim.nonEmpty).map(Paths.get(_).toAbsolutePath).getOrElse:
      val at = java.time.LocalDateTime.now.truncatedTo(java.time.temporal.ChronoUnit.SECONDS).toString.replace(':', '-')
      logsDir.resolve(s"$name-$at.log")

  /**
   * `target/logs` of the soness build this process runs from (the `target` directory above its classes or jars, in a
   * directory with a `build.sbt`), which the log explorer reads; `~/.cache/soness/logs` for one that isn't in a build.
   */
  lazy val logsDir: Path =
    val source = scala.util.Try(Paths.get(classOf[Options].getProtectionDomain.getCodeSource.getLocation.toURI)).toOption
    Iterator.iterate(source.orNull)(p => if p == null then null else p.getParent).takeWhile(_ != null)
      .find(p => p.getFileName != null && p.getFileName.toString == "target" && java.nio.file.Files.exists(p.resolveSibling("build.sbt")))
      .map(_.resolve("logs"))
      .getOrElse(Paths.get(sys.props("user.home"), ".cache", "soness", "logs"))

  private val currentFile = java.util.concurrent.atomic.AtomicReference(Option.empty[Path])

  /** This process's debug log, once `--debug` has set it up: what self-improvement takes as evidence of its work. */
  def current: Option[Path] = currentFile.get

  /**
   * The loggers for `options`, replacing ZIO's default (stdout) logger; at debug level, to `name`'s [[debugFile]] too
   * (and, unless `quietConsole`, to stderr at debug level).
   */
  def layer(options: Options, name: String = "soness", quietConsole: Boolean = false): ZLayer[Any, Nothing, Unit] =
    val filter = LogFilter.LogLevelByNameConfig(if options.debug then LogLevel.Debug else LogLevel.Info)
    val format = LogFormat.default + (LogFormat.space + LogFormat.allAnnotations)
    // `quietConsole`: stderr at info level whatever the file's (a command-line run, whose debug detail is in its file)
    val console = makeConsoleErrLogger(ConsoleLoggerConfig(format,
      if quietConsole then LogFilter.LogLevelByNameConfig(LogLevel.Info) else filter))
    val path = Option.when(options.debug)(debugFile(name))
    currentFile.set(path)
    val file = path.map(p => makeFileLogger(FileLoggerConfig(p, format, filter)))
    val loggers = Runtime.removeDefaultLoggers ++ ZLayer.scoped:
      ZIO.foreachDiscard(path)(p => ZIO.attemptBlocking(java.nio.file.Files.createDirectories(p.getParent)).ignore) *>
        ZIO.foreachDiscard(console :: file.toList)(_.flatMap(logger => ZIO.withLoggerScoped(withoutNoise(logger))))
    // said once the loggers are in place: before, ZIO's default logger would write it to stdout, the protocol's
    loggers >>> ZLayer.fromZIO(ZIO.foreachDiscard(path)(p => ZIO.logInfo(s"debug log: $p")))

  /**
   * Leaves out zio-http's debug line for a pooled connection it closes after its idle timeout (50 s without use): routine
   * connection reuse, logged under the trace of whoever opened the connection, so its logger name can't filter it.
   */
  private def withoutNoise(logger: ZLogger[String, Any]): ZLogger[String, Any] =
    new ZLogger[String, Any]:
      def apply(
        trace: Trace,
        fiberId: FiberId,
        logLevel: LogLevel,
        message: () => String,
        cause: Cause[Any],
        context: FiberRefs,
        spans: List[LogSpan],
        annotations: Map[String, String],
      ): Any =
        val text = message()
        if !text.startsWith("ReadTimeoutException on channel") then
          logger(trace, fiberId, logLevel, () => text, cause, context, spans, annotations)

  /** The loggers for the process's arguments; bad arguments are reported on stderr and end the process. */
  def fromArgs(name: String): ZLayer[ZIOAppArgs, Nothing, Unit] =
    ZLayer.fromZIO(ZIOAppArgs.getArgs).flatMap: args =>
      Options.parse(args.get.toList) match
        case Right(options) => layer(options, name)
        case Left(message)  => ZLayer.fromZIO(ZIO.succeed(java.lang.System.err.println(message)) *> ZIO.die(IllegalArgumentException(message)))

  /** Each Bedrock exchange of `model`, request and response in full, at debug level. */
  def bedrock(model: String): BedrockMiddleware =
    BedrockMiddleware.observe: (request, exit, latencyMs) =>
      exit match
        case Exit.Success(response) =>
          ZIO.logDebug(s"Bedrock $model exchange in ${latencyMs}ms: request=${request.body.toJson} response=${response.body.toJson}")
        case Exit.Failure(cause) =>
          ZIO.logDebugCause(s"Bedrock $model exchange failed in ${latencyMs}ms: request=${request.body.toJson}", cause)

  /** Each Jev exchange, request and response in full, at debug level. */
  val jev: Decisions.ExchangeObserver =
    Decisions.ExchangeObserver.fromFunction: observation =>
      observation.outcome match
        case Decisions.ExchangeOutcome.Success(response) =>
          ZIO.logDebug(s"Jev exchange in ${observation.latencyMs}ms: request=${observation.request.toJson} response=${response.toJson}")
        case Decisions.ExchangeOutcome.Failure(cause) =>
          ZIO.logDebugCause(s"Jev exchange failed in ${observation.latencyMs}ms: request=${observation.request.toJson}", cause)
