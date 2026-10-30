package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zio_decisions.orchestration.Recall
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files

/**
 * Sessions live in the project's event log (`.soness/events.jsonl`): listed, loaded with their history replayed,
 * resumed; and a new request is given the earlier work of its session (resumed or loaded) that Jev recalls as
 * bearing on it.
 */
object SessionsSpec extends ZIOSpecDefault:

  /** Reads one file, answers with the request, and records what it was given. */
  private final class Recording(contexts: Ref[Vector[Option[String]]], asked: Ref[Vector[List[Recall.Earlier]]]) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String], mode: HarnessMode,
        steering: UIO[List[String]]): Task[HarnessResult] =
      contexts.update(_ :+ context) *>
        toolset.call("read_text_file", Json.Obj("path" -> Json.Str("src/A.scala"))).as(HarnessResult(s"did: $prompt", ModelUsage()))
    override def recall(prompt: String, earlier: List[Recall.Earlier]): Task[List[Int]] =
      // the stand-in for Jev: recalls the tasks the request names
      asked.update(_ :+ earlier).as(earlier.zipWithIndex.collect { case (e, i) if prompt.contains(e.request.take(6)) => i })

  private final class Updates(updates: Ref[Vector[(SessionId, SessionUpdate)]]) extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] = updates.update(_ :+ (n.sessionId -> n.update))
    def requestPermission(r: RequestPermissionRequest) = ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))
    override def readTextFile(r: ReadTextFileRequest) = ZIO.succeed(ReadTextFileResponse("class A"))

  /** An agent (a soness process), its client offering its files (the session's one tool, `read_text_file`). */
  private def agent(harness: Harness, updates: Ref[Vector[(SessionId, SessionUpdate)]]) =
    for
      http    <- ZIO.environment[Client]
      parent  <- ZIO.scope
      pipe    <- Transport.pipe
      catalog  = HarnessCatalog.make(List("m" -> harness), "m", HarnessMode.Single).toOption.get
      _       <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent))
                   .forkScoped
      agent   <- AcpClient.connect(pipe._2, Updates(updates))
      _       <- agent.initialize(InitializeRequest(ProtocolVersion.V1, ClientCapabilities(fs = FileSystemCapabilities(readTextFile = true))))
    yield agent

  private def prompt(agent: com.jamesward.zioacp.AgentConnection, id: SessionId, text: String) =
    agent.prompt(PromptRequest(id, List(ContentBlock.text(text))))

  def spec = suite("sessions in the event log")(
    test("a turn is logged; a second process lists the session, loads it (history replayed) and goes on from it") {
      for
        dir      <- ZIO.attemptBlocking(Files.createTempDirectory("sessions"))
        contexts <- Ref.make(Vector.empty[Option[String]])
        asked    <- Ref.make(Vector.empty[List[Recall.Earlier]])
        u1       <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        first    <- agent(Recording(contexts, asked), u1)
        s1       <- first.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
        _        <- prompt(first, s1, "fix the SAVE10 bug")
        events   <- EventLog.at(dir).read
        ignore   <- ZIO.attemptBlocking(Files.readString(dir.resolve(".soness/.gitignore")))
        // another soness process, as after a restart of the client
        u2       <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        second   <- agent(Recording(contexts, asked), u2)
        listed   <- second.listSessions(ListSessionsRequest(Some(dir.toString)))
        _        <- second.loadSession(LoadSessionRequest(s1, dir.toString, Nil))
        replayed <- u2.get.map(_.collect { case (`s1`, u) => u })
        _        <- prompt(second, s1, "now add tests for fix the SAVE10 bug")
        after    <- EventLog.at(dir).read
        lastCtx  <- contexts.get.map(_.last)
      yield assertTrue(
        events.map(_.getClass.getSimpleName) == List("SessionStarted", "TaskStarted", "ToolCalled", "TaskEnded"),
        ignore.trim == "*",
        listed.sessions.map(s => s.sessionId -> s.title) == List(s1 -> Some("fix the SAVE10 bug")),
        replayed.collect { case SessionUpdate.UserMessageChunk(c, _, _) => c } == List(ContentBlock.text("fix the SAVE10 bug")),
        replayed.exists { case SessionUpdate.ToolCallStarted(c) => c.title.contains("src/A.scala") && c.status == ToolCallStatus.Completed; case _ => false },
        replayed.collect { case SessionUpdate.AgentMessageChunk(c, _, _) => c } == List(ContentBlock.text("did: fix the SAVE10 bug")),
        // the resumed session's next task is task 2, and the task it follows was recalled into its context
        after.collect { case t: EventLog.Event.TaskStarted => t.task } == List(1, 2),
        lastCtx.exists(_.contains("Earlier work in this session")), lastCtx.exists(_.contains("did: fix the SAVE10 bug")),
        lastCtx.exists(_.contains("Files it worked on: src/A.scala")),
      )
    },
    test("recall is the session's own: a new session is offered no other session's work; its own, only what's recalled") {
      for
        dir      <- ZIO.attemptBlocking(Files.createTempDirectory("sessions"))
        contexts <- Ref.make(Vector.empty[Option[String]])
        asked    <- Ref.make(Vector.empty[List[Recall.Earlier]])
        u        <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        a        <- agent(Recording(contexts, asked), u)
        s1       <- a.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
        _        <- prompt(a, s1, "rename Foo to Bar")
        _        <- prompt(a, s1, "explain the build")
        s2       <- a.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
        before   <- asked.get.map(_.size)
        _        <- prompt(a, s2, "about rename Foo: did it update the docs?")
        other    <- asked.get.map(_.size)
        otherCtx <- contexts.get.map(_.last)
        _        <- prompt(a, s1, "about rename Foo: did it update the docs?")
        offered  <- asked.get.map(_.last)
        ctx      <- contexts.get.map(_.last)
      yield assertTrue(
        // the new session has no earlier tasks of its own: nothing is asked of Jev, nothing recalled
        other == before, !otherCtx.exists(_.contains("Earlier work")),
        offered.map(e => e.request -> e.sameSession) == List("rename Foo to Bar" -> true, "explain the build" -> true),
        ctx.exists(_.contains("did: rename Foo to Bar")), !ctx.exists(_.contains("did: explain the build")),
      )
    },
    test("session/resume in another process: the session goes on from its last task, recalled, with no history replayed") {
      for
        dir      <- ZIO.attemptBlocking(Files.createTempDirectory("sessions"))
        contexts <- Ref.make(Vector.empty[Option[String]])
        asked    <- Ref.make(Vector.empty[List[Recall.Earlier]])
        u1       <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        first    <- agent(Recording(contexts, asked), u1)
        s1       <- first.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
        _        <- prompt(first, s1, "fix the SAVE10 bug")
        u2       <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        second   <- agent(Recording(contexts, asked), u2)
        resumed  <- second.resumeSession(ResumeSessionRequest(s1, dir.toString, Nil))
        before   <- u2.get.map(_.collect { case (`s1`, u) => u })
        _        <- prompt(second, s1, "now add tests for fix the SAVE10 bug")
        after    <- EventLog.at(dir).read
        lastCtx  <- contexts.get.map(_.last)
        unknown  <- second.resumeSession(ResumeSessionRequest(SessionId("nope"), dir.toString, Nil)).either
      yield assertTrue(
        resumed.configOptions.isDefined, resumed.modes.isEmpty,
        // no replay: the client has the history
        !before.exists { case _: SessionUpdate.UserMessageChunk => true; case _: SessionUpdate.AgentMessageChunk => true; case _ => false },
        after.collect { case t: EventLog.Event.TaskStarted => t.task } == List(1, 2),
        lastCtx.exists(_.contains("did: fix the SAVE10 bug")),
        unknown.isLeft,
      )
    },
    test("an unknown session can't be loaded") {
      for
        dir <- ZIO.attemptBlocking(Files.createTempDirectory("sessions"))
        u   <- Ref.make(Vector.empty[(SessionId, SessionUpdate)])
        c   <- Ref.make(Vector.empty[Option[String]])
        q   <- Ref.make(Vector.empty[List[Recall.Earlier]])
        a   <- agent(Recording(c, q), u)
        r   <- a.loadSession(LoadSessionRequest(SessionId("nope"), dir.toString, Nil)).either
      yield assertTrue(r.isLeft)
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)
