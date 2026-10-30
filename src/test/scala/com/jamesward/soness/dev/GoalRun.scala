package com.jamesward.soness.dev

import com.jamesward.soness.*
import com.jamesward.zioacp.{AcpAgent, AcpClient, AgentConnection, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * A goal, then soness improving itself from how it went: soness works on `goal` in a project, the run's transcript and
 * telemetry are read back from this process's debug log, and soness then works on its own project (and its source
 * dependencies) to do such goals better, in rounds judged by self-improvement's gates ([[Improve.Local]]). A round aims
 * at the goal's accomplishment, alignment with soness's DESIGN.md, token efficiency and time efficiency (parallelism).
 *
 * No editor is involved: soness runs in this process, as an ACP agent its client side drives in memory (no client
 * file system or terminal: soness's own tools and the project's `.mcp.json` servers). Its permission requests are
 * refused unless `--allow`. The changes take effect once soness is staged again: rerun the goal to measure them.
 *
 * Run it outside sbt (the gates and the improvement's own build go through the project's sbt server, which a
 * `runMain` would hold): `./sbta Test/stage`, then
 * `target/out/jvm/scala-3.10.0/soness/test-stage/bin/soness-goal [--project <dir>] [--rounds <n>] [--no-improve]
 * [--eval] [--allow] <goal…>`. Each run's record is in `target/goal-runs/<stamp>/` of soness's project.
 */
object GoalRun extends ZIOAppDefault:

  // the debug detail (every exchange and call) in the run's log file; stderr shows the progress
  override val bootstrap = Logging.layer(Options(debug = true), "soness-goal", quietConsole = true)
  override def gracefulShutdownTimeout: Duration = 10.seconds

  final case class Args(goal: String, project: Path, soness: Path, rounds: Int, improve: Boolean, eval: Boolean,
      allow: Boolean) derives CanEqual

  val Usage: String =
    "usage: soness-goal [--project <dir>] [--soness <dir>] [--rounds <n>] [--no-improve] [--eval] [--allow] <goal…>"

  /** The arguments: the goal is every word that isn't an option; the project defaults to `cwd`, soness to `home`. */
  def parse(args: List[String], cwd: Path, home: Path): Either[String, Args] =
    def go(rest: List[String], acc: Args, words: Vector[String]): Either[String, Args] = rest match
      case Nil => Either.cond(words.nonEmpty, acc.copy(goal = words.mkString(" ")), s"no goal given\n$Usage")
      case "--project" :: dir :: more => go(more, acc.copy(project = cwd.resolve(dir).toAbsolutePath.normalize), words)
      case "--soness" :: dir :: more  => go(more, acc.copy(soness = cwd.resolve(dir).toAbsolutePath.normalize), words)
      case "--rounds" :: n :: more    =>
        n.toIntOption.filter(r => r >= 1 && r <= 20).toRight(s"--rounds is a number from 1 to 20, not $n")
          .flatMap(r => go(more, acc.copy(rounds = r), words))
      case "--no-improve" :: more => go(more, acc.copy(improve = false), words)
      case "--eval" :: more       => go(more, acc.copy(eval = true), words)
      case "--allow" :: more      => go(more, acc.copy(allow = true), words)
      case option :: _ if option.startsWith("--") => Left(s"unknown option $option\n$Usage")
      case word :: more => go(more, acc, words :+ word)
    go(args, Args("", cwd.toAbsolutePath.normalize, home.toAbsolutePath.normalize, 1, improve = true, eval = false,
      allow = false), Vector.empty)

  // ---------- telemetry ----------

  /** What a run cost, and how its time went: from its transcript and its turn's metrics. */
  final case class Telemetry(
    wallMs: Long,
    jevCalls: Int,
    jevMs: Long,
    jevInputTokens: Long,
    llmCalls: Int,
    llmMs: Long,
    llmInputTokens: Long,
    llmOutputTokens: Long,
    toolCalls: Int,
    toolMs: Long,
    /** The time at least one tool call ran. */
    toolBusyMs: Long,
    /** The most tool calls running at once. */
    maxConcurrent: Int,
    failedCalls: Int,
    repeatedCalls: Int,
  ) derives JsonCodec:
    /** Tool time per busy time: 1.0 is one call at a time. */
    def parallelism: Double = if toolBusyMs == 0 then 1.0 else toolMs.toDouble / toolBusyMs
    /** The time no tool ran: deciding, writing, waiting. */
    def idleMs: Long = (wallMs - toolBusyMs).max(0)

    def describe: String =
      f"""- wall time ${wallMs / 1000.0}%.1fs: tools busy ${toolBusyMs / 1000.0}%.1fs, no tool running ${idleMs / 1000.0}%.1fs
         |- decision model: $jevCalls requests, ${jevMs / 1000.0}%.1fs, $jevInputTokens input tokens
         |- LLM: $llmCalls requests, ${llmMs / 1000.0}%.1fs, $llmInputTokens input and $llmOutputTokens output tokens
         |- tools: $toolCalls calls, ${toolMs / 1000.0}%.1fs in all; at most $maxConcurrent at once, parallelism $parallelism%.2f
         |- $failedCalls failed calls, $repeatedCalls repeats of an identical call""".stripMargin

  def telemetry(turn: Transcript.Turn, metrics: Option[TurnMetrics]): Telemetry =
    val spans = turn.calls.flatMap(c => c.ms.map(ms => c.startedAt.toEpochMilli -> (c.startedAt.toEpochMilli + ms)))
    // the union of the calls' spans, and the most that overlap
    val busy = spans.sortBy(_._1).foldLeft((0L, Option.empty[(Long, Long)])):
      case ((sum, None), span)                          => (sum, Some(span))
      case ((sum, Some((s, e))), (s2, e2)) if s2 <= e   => (sum, Some((s, e.max(e2))))
      case ((sum, Some((s, e))), span)                  => (sum + (e - s), Some(span))
    val busyMs = busy._1 + busy._2.fold(0L)((s, e) => e - s)
    val edges = spans.flatMap((s, e) => List(s -> 1, e -> -1)).sortBy((t, d) => (t, d))
    val most = edges.scanLeft(0)(_ + _._2).maxOption.getOrElse(0)
    val repeats = turn.calls.groupBy(c => (c.tool, c.arguments)).values.map(_.size - 1).sum
    Telemetry(
      wallMs = metrics.map(_.latencyMs).orElse(turn.wallMs).getOrElse(0L),
      jevCalls = metrics.map(_.jevCalls).getOrElse(turn.jevCalls),
      jevMs = metrics.map(_.jevLatencyMs).filter(_ > 0).getOrElse(turn.jevMs),
      jevInputTokens = metrics.map(_.jevInputTokens).getOrElse(0L),
      llmCalls = metrics.map(_.llmCalls).getOrElse(turn.llmCalls),
      llmMs = metrics.map(_.llmLatencyMs).filter(_ > 0).getOrElse(turn.llmMs),
      llmInputTokens = metrics.map(_.llmInputTokens).getOrElse(0L),
      llmOutputTokens = metrics.map(_.llmOutputTokens).getOrElse(0L),
      toolCalls = turn.calls.size,
      toolMs = spans.map((s, e) => e - s).sum,
      toolBusyMs = busyMs,
      maxConcurrent = most,
      failedCalls = turn.calls.count(_.failed),
      repeatedCalls = repeats,
    )

  // ---------- the goal ----------

  /** How the goal went: the session's answer and ending, its metrics, transcript and telemetry, and Jev's judgment. */
  final case class Outcome(session: String, answer: String, ending: String, metrics: Option[TurnMetrics],
      turn: Option[Transcript.Turn], telemetry: Option[Telemetry], accomplished: Double)

  /** A round's request: improve soness from how the goal went. */
  def improvementPrompt(args: Args, outcome: Outcome, design: String, log: Option[Path], n: Int, history: List[String]): String =
    val transcript = outcome.turn.fold("(no transcript: the debug log had no turn of the session)")(Transcript.render(_, 20000))
    val evidence = log.fold("")(l => s"\nThe whole debug log is at $l: transcript_turns, transcript_turn and transcript_call show " +
      s"any of it in full${outcome.turn.fold("")(t => s" (the goal's turn is ${t.id})")}.")
    s"""Improve soness (this project, the agent harness) from one real run of it.
       |
       |soness was given this goal, in ${args.project}:
       |${args.goal}
       |
       |How it went: it ended ${outcome.ending}; the decision model judges the goal accomplished with p ${f"${outcome.accomplished}%.2f"}.
       |Its answer: ${Transcript.cut(outcome.answer, 2000)}
       |
       |Telemetry:
       |${outcome.telemetry.fold("(none)")(_.describe)}
       |$evidence
       |
       |Find what in soness's code (or a source dependency's: zio-decisions' orchestration, zio-acp, zio-bedrock) made
       |this run go as it did, and make the one change with the most effect on these aims, most important first:
       |1. the goal accomplished: if it wasn't (or only in part), what in the harness kept it from it;
       |2. alignment with DESIGN.md (below): the decision model orchestrates and is used abundantly; LLMs only fill in
       |   tool parameters and summarize; tool calls don't block or poll; everything that can run in parallel does; tool
       |   output is memoized and searched rather than run again;
       |3. token efficiency: fewer and smaller decision-model and LLM requests for the same work (what's in their
       |   prompts and contexts that they don't need, requests that repeat);
       |4. time efficiency: independent calls run at the same time, nothing waits idle, no repeated or wasted calls.
       |Change it, add or update a test that fails without the change, and run the relevant tests until they pass. Keep
       |the change small and focused on that one cause. Don't redo the goal's work, and don't change the goal's project.
       |${if history.isEmpty then "" else "Earlier rounds: " + history.mkString("; ") + "."}
       |
       |This is round $n of a self-improvement run, on ${args.soness} itself: edit its files (and, where the cause is in
       |one, its source dependencies' sibling checkouts) directly. What this project's files then are is judged after
       |this reply by its gates (a code change with a test, the offline suite${if args.eval then ", the quick eval" else ""}).
       |In your reply say: what in the run you picked, the cause in the code, the change (files), and the tests run.
       |
       |DESIGN.md:
       |$design
       |
       |The goal's transcript:
       |$transcript""".stripMargin

  // ---------- running soness in this process ----------

  /** The client side of soness's ACP agent: each session's messages collected and its progress logged; permissions refused unless `allow`. */
  private final class Driver(text: Ref[Map[SessionId, StringBuilder]], allow: Boolean) extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] = n.update match
      case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) =>
        text.update(m => m.updated(n.sessionId, m.getOrElse(n.sessionId, new StringBuilder).append(t)))
      case SessionUpdate.ToolCallStarted(call) => ZIO.logInfo(s"goal run: ${call.title}")
      case _ => ZIO.unit
    def requestPermission(r: RequestPermissionRequest) =
      val chosen = r.options.find(_.kind == PermissionOptionKind.AllowOnce).filter(_ => allow)
      ZIO.logWarning(s"goal run: ${if chosen.isDefined then "allowed" else "refused"} ${r.toolCall.title.getOrElse(r.toolCall.toolCallId.value)}")
        .as(RequestPermissionResponse(chosen.fold(RequestPermissionOutcome.Cancelled)(o => RequestPermissionOutcome.Selected(o.optionId))))

  /** soness's agent in this process, and the client side's collected text. */
  def connect(catalog: HarnessCatalog, allow: Boolean): ZIO[Client & Scope, Throwable, (AgentConnection, Ref[Map[SessionId, StringBuilder]])] =
    for
      http   <- ZIO.environment[Client]
      parent <- ZIO.scope
      pipe   <- Transport.pipe
      text   <- Ref.make(Map.empty[SessionId, StringBuilder])
      _      <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent)).forkScoped
      agent  <- AcpClient.connect(pipe._2, Driver(text, allow))
      _      <- agent.initialize(InitializeRequest(ProtocolVersion.V1)).mapError(e => RuntimeException(e.message))
    yield (agent, text)

  /** `prompt` in a new session in `dir`: its id, answer, ending and metrics. */
  def attempt(agent: AgentConnection, text: Ref[Map[SessionId, StringBuilder]], dir: Path, prompt: String)
      : Task[(String, String, String, Option[TurnMetrics])] =
    for
      session  <- agent.newSession(NewSessionRequest(dir.toString, Nil)).mapError(e => RuntimeException(e.message)).map(_.sessionId)
      response <- agent.prompt(PromptRequest(session, List(ContentBlock.text(prompt)))).mapError(e => RuntimeException(e.message))
      answer   <- text.get.map(_.get(session).fold("")(_.toString.trim))
      metrics   = TurnMetrics.fromMeta(response.meta)
    yield (session.value, answer, metrics.map(_.ending).getOrElse(response.stopReason.toString.toLowerCase), metrics)

  /** The session's last turn in the debug log `log`. */
  def transcript(log: Option[Path], session: String): UIO[Option[Transcript.Turn]] =
    ZIO.foreach(log): l =>
      ZIO.attemptBlocking(Files.readAllLines(l, UTF_8).asScala.iterator).map(lines => Transcript.turns(lines).filter(_.session == session).lastOption)
    .map(_.flatten).catchAll(e => ZIO.logWarning(s"couldn't read the transcript: ${e.getMessage}").as(None))

  /** The goal, then the improvement rounds; every step's record in `out`. A summary of it all. */
  def runWith(args: Args, catalog: HarnessCatalog, steps: Improve.Steps, log: Option[Path], out: Path,
      read: (Option[Path], String) => UIO[Option[Transcript.Turn]] = transcript): ZIO[Client & Scope, Throwable, String] =
    def write(name: String, text: String) = ZIO.attemptBlocking { Files.createDirectories(out); Files.writeString(out.resolve(name), text, UTF_8) }
    for
      (agent, text) <- connect(catalog, args.allow)
      _             <- ZIO.logInfo(s"goal run: working on the goal in ${args.project}")
      goal          <- attempt(agent, text, args.project, args.goal)
      (session, answer, ending, metrics) = goal
      _             <- ZIO.sleep(500.millis) // the log's last lines written
      turn          <- read(log, session)
      tel            = turn.map(telemetry(_, metrics))
      harness       <- ZIO.fromOption(catalog.harness(catalog.default)).orElseFail(IllegalStateException("no harness"))
      accomplished  <- harness.judge(s"A coding agent was given this goal in ${args.project}:\n${args.goal}\n\nIt ended $ending. " +
                         s"Its report:\n${answer.take(6000)}\n\n${turn.fold("")(t => Transcript.diagnose(t).mkString("What its transcript shows:\n- ", "\n- ", ""))}",
                         "Did the agent accomplish the goal?",
                         "the report shows the goal done: the change made and checked, the question answered from what the agent found",
                         "the goal isn't done, or only in part, or the report says what's still missing or that it couldn't")
                         .catchAll(e => ZIO.logWarning(s"couldn't judge the goal: ${e.getMessage}").as(0.0))
      outcome        = Outcome(session, answer, ending, metrics, turn, tel, accomplished)
      _             <- write("goal.json", Json.Obj("goal" -> Json.Str(args.goal), "project" -> Json.Str(args.project.toString),
                         "session" -> Json.Str(session), "ending" -> Json.Str(ending), "accomplished" -> Json.Num(accomplished),
                         "answer" -> Json.Str(answer), "log" -> log.fold(Json.Null)(l => Json.Str(l.toString)),
                         "metrics" -> metrics.fold(Json.Null)(_.toJsonAST.toOption.getOrElse(Json.Null)),
                         "telemetry" -> tel.fold(Json.Null)(_.toJsonAST.toOption.getOrElse(Json.Null))).toJsonPretty)
      _             <- write("transcript.txt", turn.fold("(no transcript)")(Transcript.render(_, Int.MaxValue)))
      head           = s"Goal: ${args.goal}\nEnded $ending; accomplished: p ${f"$accomplished%.2f"}\n${tel.fold("")(_.describe)}\n\n" +
                         s"Answer:\n$answer\n"
      _             <- Console.printLine(head)
      design        <- ZIO.attemptBlocking(Files.readString(args.soness.resolve("DESIGN.md"), UTF_8)).orElseSucceed("(no DESIGN.md)")
      o              = Improve.Options(log, args.rounds, args.eval, None)
      rounds        <- if !args.improve then ZIO.succeed(Nil)
                       else ZIO.foldLeft(1 to args.rounds)(List.empty[String]): (history, n) =>
                         if history.lastOption.exists(_.startsWith(Improve.NoChange)) then ZIO.succeed(history)
                         else
                           for
                             prepared <- steps.prepare(o, n, log, history)
                             round     = prepared.copy(prompt = improvementPrompt(args, outcome, design, log, n, history))
                             _        <- ZIO.attemptBlocking(Files.writeString(round.out.resolve("round.json"), Json.Obj(
                                           "round" -> Json.Num(n), "project" -> Json.Str(round.project.toString),
                                           "goal" -> Json.Str(args.goal), "prompt" -> Json.Str(round.prompt)).toJsonPretty, UTF_8))
                             _        <- ZIO.logInfo(s"goal run: improvement round $n in ${args.soness}")
                             worked   <- attempt(agent, text, args.soness, round.prompt).either
                             verdict  <- steps.gate(o, round)
                             line      = Improve.history(verdict, worked.toOption.map(_._2))
                             _        <- Console.printLine(s"Round $n: $line\nIts record: ${round.out}\n")
                           yield history :+ line
      summary        = head + (if rounds.isEmpty then "" else rounds.zipWithIndex.map((l, i) => s"${i + 1}. $l")
                         .mkString("\nImprovement rounds:\n", "\n", "\nThe changes take effect once soness is staged again (./sbta Test/stage); rerun the goal to measure them.\n"))
      _             <- write("summary.txt", summary)
    yield summary

  def run =
    (for
      raw     <- getArgs
      cwd      = Path.of("").toAbsolutePath
      home     = Logging.logsDir.getParent.getParent // target/logs → the build
      args    <- ZIO.fromEither(parse(raw.toList, cwd, home)).mapError(IllegalArgumentException(_))
      _       <- ZIO.foreachDiscard(Logging.current)(l => ZIO.logInfo(s"goal run: the debug log is $l"))
      catalog <- Soness.catalog.mapError(IllegalStateException(_))
      stamp   <- Clock.localDateTime.map(_.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")))
      out      = args.soness.resolve("target").resolve("goal-runs").resolve(stamp)
      _       <- runWith(args, catalog, Improve.Local(args.soness), Logging.current, out)
      _       <- Console.printLine(s"The run's record: $out")
    yield ()).provideSome[Scope & ZIOAppArgs](Soness.httpClient)
      .tapError(e => ZIO.logError(Option(e.getMessage).getOrElse(e.toString)))
      .catchSome { case _: IllegalArgumentException => exit(ExitCode.failure) }
