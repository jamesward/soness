package com.jamesward.soness

import com.jamesward.zioacp.{AcpClient, AgentCommand, AgentConnection}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}

/**
 * Where a Kiro writing's time goes, live: Kiro (`kiro-cli acp`) launched as soness's writer launches it (a scratch
 * directory, soness's tool-less writing agent), each arm a model and reasoning effort set in that agent's config.
 * Measured for each arm:
 *
 *  - spin-up: the process to `initialize`'s answer, `session/new`, `session/set_mode`;
 *  - a writing: until Kiro's first update (its own overhead and the model's time to first token), the reasoning
 *    (first thought to first answer text), the answer's streaming, and the end of the turn;
 *  - a small prompt (a short tool call to fill in) in fresh sessions, a large one (about 60,000 characters, the size
 *    of soness's typical arguments prompt), and the small one again in a session already used for several (for
 *    comparison: soness writes each in a clean session).
 *
 * `SONESS_KIRO_ARMS` (default `opus,sonnet,haiku,opus-low`: `<model>[-<effort>]`, the model by Kiro's id prefix),
 * `SONESS_EVAL_SAMPLES` (default 3). Paid and a few minutes: only with `SONESS_EVAL_KIRO_LATENCY=true` and `kiro-cli` signed in.
 */
object KiroLatencySpec extends ZIOSpecDefault:

  private val on = if sys.env.get("SONESS_EVAL_KIRO_LATENCY").contains("true") then TestAspect.identity else TestAspect.ignore

  private val models = Map("opus" -> "claude-opus-5.5", "sonnet" -> "claude-sonnet-5.5", "haiku" -> "claude-haiku-5.5")

  private val small =
    "You fill in the arguments of a tool call. The tool: read_file {path: string (required)}. The request: show me " +
      "src/main/scala/shop/Discounts.scala. Answer with only one JSON object of the arguments, nothing else. Don't use any tool."

  /** About 60,000 characters: a request with a large context before the same short question. */
  private val large =
    val code = (1 to 900).map(i => s"  def f$i(x: Int): Int = x * $i + ${i % 7} // step $i of the pricing rules").mkString("\n")
    s"Context, the project's code:\n```scala\nobject Pricing:\n$code\n```\n\n$small"

  /** The times of one writing, in milliseconds from the prompt's send. */
  final case class Writing(firstUpdate: Option[Long], firstThought: Option[Long], firstText: Option[Long], lastText: Option[Long],
      total: Long, answer: String)

  /** Each session's updates, timestamped as they arrive. */
  private final class Timed(updates: Ref[Map[SessionId, Vector[(Long, SessionUpdate)]]]) extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] =
      Clock.nanoTime.flatMap(t => updates.update(m => m.updated(n.sessionId, m.getOrElse(n.sessionId, Vector.empty) :+ (t -> n.update))))
    def requestPermission(r: RequestPermissionRequest) = ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))

  private def ms(from: Long, to: Long) = (to - from) / 1_000_000

  private def writing(agent: AgentConnection, updates: Ref[Map[SessionId, Vector[(Long, SessionUpdate)]]], session: SessionId,
      prompt: String): Task[Writing] =
    for
      _     <- updates.update(_ - session)
      start <- Clock.nanoTime
      _     <- agent.prompt(PromptRequest(session, List(ContentBlock.text(prompt)))).mapError(e => RuntimeException(e.message))
      end   <- Clock.nanoTime
      seen  <- updates.get.map(_.getOrElse(session, Vector.empty))
    yield
      val thoughts = seen.collect { case (t, _: SessionUpdate.AgentThoughtChunk) => t }
      val texts = seen.collect { case (t, SessionUpdate.AgentMessageChunk(ContentBlock.Text(x, _, _), _, _)) => (t, x) }
      Writing(seen.headOption.map(u => ms(start, u._1)), thoughts.headOption.map(ms(start, _)), texts.headOption.map(t => ms(start, t._1)),
        texts.lastOption.map(t => ms(start, t._1)), ms(start, end), texts.map(_._2).mkString)

  final case class Arm(name: String, launch: Long, initialize: Long, sessions: List[Long], modes: List[Long],
      small: List[Writing], large: List[Writing], reused: List[Writing])

  private def arm(name: String, samples: Int): Task[Arm] =
    val (model, effort) = name.split('-').toList match
      case m :: e :: _ => (models.getOrElse(m, m), Some(e))
      case m :: _      => (models.getOrElse(m, m), None)
      case Nil         => ("auto", None)
    ZIO.scoped:
      for
        dir     <- ZIO.attemptBlocking(Files.createTempDirectory("kiro-latency"))
        _       <- ZIO.attemptBlocking:
                     val agents = Files.createDirectories(dir.resolve(".kiro/agents"))
                     Files.writeString(agents.resolve(s"${AcpWriter.WriteAgent}.json"), Json.Obj(
                       "name" -> Json.Str(AcpWriter.WriteAgent), "description" -> Json.Str("latency"), "prompt" -> Json.Null,
                       "tools" -> Json.Arr(), "allowedTools" -> Json.Arr(), "mcpServers" -> Json.Obj(),
                       "useLegacyMcpJson" -> Json.Bool(false), "resources" -> Json.Arr(), "model" -> Json.Str(model)).toJson)
        updates <- Ref.make(Map.empty[SessionId, Vector[(Long, SessionUpdate)]])
        t0      <- Clock.nanoTime
        agent   <- AcpClient.launch(AgentCommand("kiro-cli", List("acp", "--agent", AcpWriter.WriteAgent, "--model", model) ++
                     effort.toList.flatMap(e => List("--effort", e)), cwd = Some(dir)), Timed(updates), _ => ZIO.unit)
        t1      <- Clock.nanoTime
        _       <- agent.initialize(InitializeRequest(ProtocolVersion.V1, ClientCapabilities())).mapError(e => RuntimeException(e.message))
        t2      <- Clock.nanoTime
        // a fresh session for each small writing, as a loop's first writing gets
        opened  <- ZIO.foreach((1 to samples + 1).toList): _ =>
                     for
                       a <- Clock.nanoTime
                       s <- agent.newSession(NewSessionRequest(dir.toString, Nil)).mapError(e => RuntimeException(e.message))
                       b <- Clock.nanoTime
                       _ <- agent.setSessionMode(SetSessionModeRequest(s.sessionId, SessionModeId(AcpWriter.WriteAgent))).ignore
                       c <- Clock.nanoTime
                     yield (s.sessionId, ms(a, b), ms(b, c))
        smalls  <- ZIO.foreach(opened.take(samples))(o => writing(agent, updates, o._1, small))
        larges  <- ZIO.foreach(opened.take(samples))(o => writing(agent, updates, o._1, large))
        // the last session, used for several writings in a row (as soness's loop does): its later writings
        last     = opened.last._1
        reused  <- ZIO.foreach((1 to samples + 3).toList)(_ => writing(agent, updates, last, small)).map(_.drop(3))
      yield Arm(name, ms(t0, t1), ms(t1, t2), opened.map(_._2), opened.map(_._3), smalls, larges, reused)

  private def median(xs: List[Long]): Long = if xs.isEmpty then -1 else xs.sorted.apply(xs.size / 2)

  private def describe(label: String, ws: List[Writing]): String =
    def m(f: Writing => Option[Long]) = median(ws.flatMap(f(_)))
    val reasoning = median(ws.flatMap(w => for t <- w.firstThought; x <- w.firstText yield x - t))
    val streaming = median(ws.flatMap(w => for f <- w.firstText; l <- w.lastText yield l - f))
    f"  $label%-28s total ${median(ws.map(_.total))}%6d ms   first update ${m(_.firstUpdate)}%6d   reasoning $reasoning%6d   " +
      f"answer streamed $streaming%5d   after the answer ${median(ws.flatMap(w => w.lastText.map(w.total - _)))}%4d"

  def spec = suite("Kiro latency")(
    test("where a Kiro writing's time goes: spin-up, Kiro's overhead and the model's first token, reasoning, the answer") {
      val arms = sys.env.get("SONESS_KIRO_ARMS").filter(_.nonEmpty).getOrElse("opus,sonnet,haiku,opus-low").split(',').map(_.trim).toList
      val samples = sys.env.get("SONESS_EVAL_SAMPLES").flatMap(_.toIntOption).getOrElse(3)
      for
        results <- ZIO.foreach(arms)(a => arm(a, samples).either.map(a -> _))
        _       <- ZIO.foreachDiscard(results):
                     case (a, Left(e)) => Console.printLine(s"$a: failed: ${e.getMessage}")
                     case (_, Right(r)) =>
                       Console.printLine(
                         s"${r.name}: launch ${r.launch} ms, initialize ${r.initialize} ms, session/new ${r.sessions.mkString("/")} ms, " +
                           s"set_mode ${r.modes.mkString("/")} ms\n" +
                           describe(s"small (${small.length} chars)", r.small) + "\n" + describe(s"large (${large.length} chars)", r.large) + "\n" +
                           describe("small, a used session", r.reused) + "\n" +
                           s"  answers: ${r.small.map(_.answer.take(80)).distinct.mkString(" | ")}")
      yield assertTrue(results.exists(_._2.isRight))
    } @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(30.minutes) @@ on,
  )
