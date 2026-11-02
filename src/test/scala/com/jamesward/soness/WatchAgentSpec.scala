package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, StandardOpenOption}

/**
 * A session watching another's transcript: a prompt starts a managed watch of a debug log (`watch_log`); each turn
 * that ends there is judged, and worked on as a task of the session when it shows something to act on.
 */
object WatchAgentSpec extends ZIOSpecDefault:

  /** Starts a watch for "watch …" prompts; records every other prompt with its context; judges by `act`. */
  private final class Watcher(log: String, seen: Ref[Vector[(String, Option[String])]], act: Double) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
        steering: UIO[List[String]]): Task[HarnessResult] =
      if prompt.startsWith("watch") then
        toolset.call("watch_log", Json.Obj("log" -> Json.Str(log), "instructions" -> Json.Str("improve soness from what you see")))
          .map(r => HarnessResult(r.toString, ModelUsage()))
      else
        // a watch's task: an edit of the watched turn's file is refused, one of this project's harness code isn't
        for
          refused <- toolset.call("write_file", Json.Obj("path" -> Json.Str("copy/A.java"), "content" -> Json.Str("x"))).either
          allowed <- toolset.call("write_file", Json.Obj("path" -> Json.Str("Harness.scala"), "content" -> Json.Str("y"))).either
          _       <- seen.update(_ :+ (prompt -> context.map(_ + s"\nREFUSED=${refused.isLeft} ALLOWED=${allowed.isRight}")))
        yield HarnessResult("improved", ModelUsage())
    override def judge(situation: String, question: String, yes: String, no: String): Task[Double] = ZIO.succeed(act)

  private final class Updates(updates: Ref[Vector[SessionUpdate]]) extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] = updates.update(_ :+ n.update)
    def requestPermission(r: RequestPermissionRequest) = ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))

  private val Other = "33333333-aaaa-4bbb-8ccc-000000000003"
  private def entry(n: Int, level: String, message: String, loop: Boolean = true) =
    f"timestamp=2026-10-18T01:00:$n%02d.000000000-06 level=$level thread=t message=\"$message\" session=$Other" +
      (if loop then " loop=33333333/task-1" else "")
  private val turn = List(
    entry(1, "DEBUG", "prompt (task 1, model m, mode auto): Fix the SAVE10 bug", loop = false),
    entry(2, "DEBUG", """tool call call-1 apply_patch: {"input":"*** Begin Patch"}"""),
    entry(3, "DEBUG", """tool call call-1 apply_patch returned in 4ms: {"text":"0 out of 1 operations applied."}"""),
    entry(3, "DEBUG", """tool call call-2 read_file: {"file_path":"src/A.java"}"""),
    entry(3, "DEBUG", """tool call call-2 read_file returned in 2ms: {"text":"class A"}"""),
    entry(4, "DEBUG", "answer: Done.", loop = false),
    entry(5, "DEBUG", """turn metrics: {"model":"m","ending":"answered"}""", loop = false),
  )

  private def setup(act: Double) =
    for
      http    <- ZIO.environment[Client]
      dir     <- ZIO.attemptBlocking(Files.createTempDirectory("watching"))
      log      = dir.resolve("soness.log")
      _       <- ZIO.attemptBlocking(Files.writeString(log, ""))
      seen    <- Ref.make(Vector.empty[(String, Option[String])])
      updates <- Ref.make(Vector.empty[SessionUpdate])
      parent  <- ZIO.scope
      pipe    <- Transport.pipe
      catalog  = HarnessCatalog.make(List("m" -> Watcher(log.toString, seen, act)), "m").toOption.get
      _       <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent)).forkScoped
      agent   <- AcpClient.connect(pipe._2, Updates(updates))
      _       <- agent.initialize(InitializeRequest(ProtocolVersion.V1, ClientCapabilities()))
      session <- agent.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
    yield (agent, session, log, seen, updates)

  private def prompt(agent: com.jamesward.zioacp.AgentConnection, id: SessionId, text: String) =
    agent.prompt(PromptRequest(id, List(ContentBlock.text(text))))

  private def text(us: Vector[SessionUpdate]) = us.collect { case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) => t }.mkString

  def spec = suite("watching a transcript")(
    test("a prompt starts a managed watch; a turn ending in the log is worked on with what it shows; /stop ends the watch") {
      for
        (agent, id, log, seen, updates) <- setup(act = 0.9)
        _       <- prompt(agent, id, "watch the log and improve soness from what you see")
        status  <- updates.get.map(text)
        _       <- ZIO.attemptBlocking(Files.writeString(log, turn.mkString("\n") + "\n", StandardOpenOption.APPEND))
        worked  <- seen.get.repeatUntil(_.nonEmpty).timeout(40.seconds)
        _       <- prompt(agent, id, "/status")
        running <- updates.get.map(text)
        pid      = """process \[(\d+)\]""".r.findFirstMatchIn(status).map(_.group(1))
        _       <- prompt(agent, id, s"/stop ${pid.getOrElse("0")}")
        after   <- updates.get.map(text)
      yield
        val (task, context) = worked.flatMap(_.headOption).get
        assertTrue(
          status.contains("is watched from now on"), pid.nonEmpty,
          task.startsWith("improve soness from what you see"), task.contains("turn 33333333/task-1"),
          context.exists(_.contains("1 apply_patch calls applied nothing")), context.exists(_.contains("Fix the SAVE10 bug")),
          running.contains("watch soness.log"), context.exists(_.contains("REFUSED=true ALLOWED=true")),
          after.contains(s"Stopping [${pid.get}]"),
        )
    },
    test("a second watch of the same log isn't started; a watch's own tasks aren't offered watch_log") {
      for
        (agent, id, log, seen, updates) <- setup(act = 0.9)
        _      <- prompt(agent, id, "watch the log")
        _      <- prompt(agent, id, "watch it again")
        said   <- updates.get.map(text)
        _      <- ZIO.attemptBlocking(Files.writeString(log, turn.mkString("\n") + "\n", StandardOpenOption.APPEND))
        _      <- seen.get.repeatUntil(_.nonEmpty).timeout(40.seconds)
        _      <- prompt(agent, id, "/status")
        status <- updates.get.map(text)
      yield assertTrue(said.contains("is already watched"), "watch soness.log".r.findAllIn(status.split("Started").last).size == 1)
    },
    test("a turn Jev judges fine is noted, not worked on") {
      for
        (agent, id, log, seen, updates) <- setup(act = 0.1)
        _     <- prompt(agent, id, "watch the log")
        _     <- ZIO.attemptBlocking(Files.writeString(log, turn.mkString("\n") + "\n", StandardOpenOption.APPEND))
        noted <- updates.get.map(text).repeatUntil(_.contains("nothing to act on")).timeout(15.seconds)
        tasks <- seen.get
      yield assertTrue(noted.nonEmpty, tasks.isEmpty)
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds)
