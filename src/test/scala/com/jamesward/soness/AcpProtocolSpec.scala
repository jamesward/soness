package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.EncoderOps
import zio.json.ast.Json
import zio.test.*

/**
 * The ACP surface a client sees of the work, against the protocol's v1 pages: tool calls (kinds, titles, locations,
 * diffs, terminals), the client's file system and terminals as tools (only when advertised), a turn's cancellation
 * (no tool call left in progress), and the session's commands.
 */
object AcpProtocolSpec extends ZIOSpecDefault:

  /** A harness that makes the calls `script` lists, in order, and answers with their results. */
  private final class Calls(script: List[(String, Json.Obj)], gate: Option[Promise[Nothing, Unit]] = None) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
        steering: UIO[List[String]]): Task[HarnessResult] =
      ZIO.foreach(script)((n, a) => toolset.call(n, a).either)
        .flatMap(rs => ZIO.foreachDiscard(gate)(_.await).as(rs))
        .map(rs => HarnessResult(rs.map(_.fold(e => s"failed: ${e.getMessage}", _.toString)).mkString("\n"), ModelUsage()))

  /** A client with files and terminals, recording what the agent asks of it. */
  private final class Recording(updates: Ref[Vector[SessionUpdate]], asked: Ref[Vector[String]], exit: Promise[Nothing, Unit])
      extends com.jamesward.zioacp.Client:
    def sessionUpdate(notification: SessionNotification): UIO[Unit] = updates.update(_ :+ notification.update)
    def requestPermission(request: RequestPermissionRequest) = ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))
    override def readTextFile(request: ReadTextFileRequest) = asked.update(_ :+ s"read ${request.path}").as(ReadTextFileResponse("hello"))
    override def writeTextFile(request: WriteTextFileRequest) = asked.update(_ :+ s"write ${request.path}" + (if request.content == "hello" || request.content.startsWith("class") then "" else s" ${request.content}")).as(WriteTextFileResponse())
    override def createTerminal(request: CreateTerminalRequest) =
      asked.update(_ :+ s"create ${request.args.mkString(" ")}").as(CreateTerminalResponse(TerminalId("t1")))
    override def waitForTerminalExit(request: WaitForTerminalExitRequest) =
      asked.update(_ :+ "wait") *> exit.await.as(WaitForTerminalExitResponse(Some(0L)))
    override def terminalOutput(request: TerminalOutputRequest) = asked.update(_ :+ "output").as(TerminalOutputResponse("ok\n", false))
    override def releaseTerminal(request: ReleaseTerminalRequest) = asked.update(_ :+ "release").as(ReleaseTerminalResponse())
    override def killTerminal(request: KillTerminalRequest) = asked.update(_ :+ "kill").as(KillTerminalResponse())

  private def setup(harness: Harness, capabilities: ClientCapabilities, exitAtOnce: Boolean = true,
      extra: List[java.nio.file.Path] = Nil) =
    for
      http    <- ZIO.environment[Client]
      updates <- Ref.make(Vector.empty[SessionUpdate])
      asked   <- Ref.make(Vector.empty[String])
      exit    <- Promise.make[Nothing, Unit]
      _       <- exit.succeed(()).when(exitAtOnce)
      parent  <- ZIO.scope
      pipe    <- Transport.pipe
      catalog  = HarnessCatalog.make(List("m" -> harness), "m").toOption.get
      _       <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent)).forkScoped
      agent   <- AcpClient.connect(pipe._2, Recording(updates, asked, exit))
      init    <- agent.initialize(InitializeRequest(ProtocolVersion.V1, capabilities))
      dir     <- ZIO.attempt(java.nio.file.Files.createTempDirectory("acp")).orDie
      session <- agent.newSession(NewSessionRequest(dir.toString, Nil, extra.map(_.toString)))
    yield (agent, session.sessionId, updates, asked, exit, dir)

  private val full = ClientCapabilities(fs = FileSystemCapabilities(readTextFile = true, writeTextFile = true), terminal = true)

  private def started(us: Vector[SessionUpdate]) = us.collect { case SessionUpdate.ToolCallStarted(c) => c }
  private def updated(us: Vector[SessionUpdate]) = us.collect { case SessionUpdate.ToolCallUpdated(c) => c }

  def spec = suite("ACP protocol surface")(
    test("tool calls carry their kind, a title that says what they do, the files they touch, and an edit's diff") {
      val script = List(
        "write_text_file" -> Json.Obj("path" -> Json.Str("src/A.java"), "content" -> Json.Str("class A {}")),
        "read_text_file" -> Json.Obj("path" -> Json.Str("src/A.java")),
      )
      for
        (agent, id, updates, asked, _, dir) <- setup(Calls(script), full)
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        us     <- updates.get
        calls   = started(us)
        done   <- asked.get
      yield assertTrue(
        calls.map(_.kind) == List(ToolKind.Edit, ToolKind.Read),
        calls.map(_.title) == List("Edit src/A.java", "Read src/A.java"),
        calls.forall(_.locations.map(_.path) == List(dir.resolve("src/A.java").toString)),
        calls.head.content == List(ToolCallContent.Diff("src/A.java", "class A {}", None)),
        done == Vector(s"write $dir/src/A.java", s"read $dir/src/A.java"),
        updated(us).forall(_.status.contains(ToolCallStatus.Completed)),
      )
    },
    test("an edit's tool call ends with what it changed: each file's diff, before and after, and how many lines") {
      val script = List("edit_file" -> Json.Obj("path" -> Json.Str("src/B.java"), "old_text" -> Json.Str("int x = 1;"),
        "new_text" -> Json.Str("int x = 2;\n  int y = 3;")))
      for
        // without the client's fs, the edit is soness's own edit_file (with it, the client's edit_text_file is)
        (agent, id, updates, _, _, dir) <- setup(Calls(script), ClientCapabilities(terminal = true))
        _      <- ZIO.attemptBlocking { java.nio.file.Files.createDirectories(dir.resolve("src"))
                    java.nio.file.Files.writeString(dir.resolve("src/B.java"), "class B {\n  int x = 1;\n}\n") }.orDie
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        us     <- updates.get
        done    = updated(us).find(_.status.contains(ToolCallStatus.Completed))
        events <- EventLog.at(dir).read
      yield assertTrue(
        // and in the project's event log, as the task's change: the file and its patch (what the task changed is read
        // from there, not from a copy of the project)
        EventLog.changes(events, id.value, 1).map(c => (c.file, c.patch)) == List((dir.resolve("src/B.java").toString,
          "--- a/src/B.java\n+++ b/src/B.java\n@@ -2,1 +2,2 @@\n-  int x = 1;\n+  int x = 2;\n+  int y = 3;\n")),
        done.flatMap(_.content).contains(List(ToolCallContent.Diff(dir.resolve("src/B.java").toString, "class B {\n  int x = 2;\n  int y = 3;\n}\n",
          Some("class B {\n  int x = 1;\n}\n")))),
        done.flatMap(_.rawOutput).exists(_.toString.contains("src/B.java: +2 −1")),
        started(us).head.locations.map(_.path) == List(dir.resolve("src/B.java").toString),
      )
    },
    test("with the client's fs, an exact replacement is read and written back through the editor (edit_text_file)") {
      val script = List("edit_text_file" -> Json.Obj("path" -> Json.Str("src/A.txt"), "old_text" -> Json.Str("ell"),
        "new_text" -> Json.Str("ipp")))
      for
        (agent, id, updates, asked, _, dir) <- setup(Calls(script), full)
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        us     <- updates.get
        done   <- asked.get
      yield assertTrue(
        done == Vector(s"read $dir/src/A.txt", s"write $dir/src/A.txt hippo"),
        started(us).head.kind == ToolKind.Edit,
        updated(us).exists(_.status.contains(ToolCallStatus.Completed)),
      )
    },
    test("the client's additionalDirectories are roots soness's file tools reach, beside the project") {
      for
        other  <- ZIO.attemptBlocking { val d = java.nio.file.Files.createTempDirectory("other")
                    java.nio.file.Files.writeString(d.resolve("B.txt"), "beside"); d }.orDie
        (agent, id, updates, _, _, _) <- setup(Calls(List("read_file" -> Json.Obj("path" -> Json.Str(other.resolve("B.txt").toString)))),
                    ClientCapabilities(terminal = true), extra = List(other))
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        us     <- updates.get
      yield assertTrue(updated(us).exists(u => u.status.contains(ToolCallStatus.Completed) && u.rawOutput.exists(_.toString.contains("beside"))))
    },
    test("a command runs in the client's terminal, shown in its tool call, waited for with wait_for_exit, then released") {
      for
        (agent, id, updates, asked, _, _) <- setup(Calls(List("run_terminal_command" -> Json.Obj("command" -> Json.Str("./gradlew test")))), full)
        r      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        us     <- updates.get
        done   <- asked.get
      yield assertTrue(
        done == Vector("create -c ./gradlew test", "wait", "output", "release"),
        started(us).head.kind == ToolKind.Execute, started(us).head.title == "Run ./gradlew test",
        updated(us).exists(_.content.contains(List(ToolCallContent.Terminal(TerminalId("t1"))))),
        r.stopReason == StopReason.EndTurn,
      )
    },
    test("cancelling a turn kills its command, releases the terminal, leaves no tool call in progress, and answers cancelled") {
      for
        (agent, id, updates, asked, _, _) <- setup(Calls(List("run_terminal_command" -> Json.Obj("command" -> Json.Str("sleep 999")))), full,
                                               exitAtOnce = false)
        turn   <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go")))).fork
        _      <- asked.get.repeatUntil(_.contains("wait"))
        _      <- agent.cancel(CancelNotification(id))
        r      <- turn.join
        _      <- asked.get.repeatUntil(_.contains("release"))
        us     <- updates.get
        done   <- asked.get
      yield
        val callId = started(us).head.toolCallId
        assertTrue(
          r.stopReason == StopReason.Cancelled,
          done.contains("kill"), done.last == "release",
          updated(us).filter(_.toolCallId == callId).exists(_.status.contains(ToolCallStatus.Failed)),
        )
    } @@ TestAspect.withLiveClock,
    test("without the client's capabilities, its files and terminals aren't offered") {
      for
        tools  <- Ref.make(Chunk.empty[String])
        h       = new Harness:
                    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
                        steering: UIO[List[String]]): Task[HarnessResult] =
                      tools.set(toolset.tools.map(_.name)).as(HarnessResult("ok", ModelUsage()))
        (agent, id, _, _, _, _) <- setup(h, ClientCapabilities())
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        names  <- tools.get
      yield assertTrue(!names.exists(n => n == "read_text_file" || n == "write_text_file" || n == "run_terminal_command"))
    },
    test("a harness's plan reaches the client as plan updates, every entry each time") {
      import com.jamesward.zio_decisions.orchestration.ParallelLoop.Progress
      val h = new Harness:
        def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
            steering: UIO[List[String]]): Task[HarnessResult] =
          ClientTools.currentPlan.get.flatMap: report =>
            report(List("read" -> Progress.Completed, "part A" -> Progress.InProgress)) *>
              report(List("read" -> Progress.Completed, "part A" -> Progress.Completed)).as(HarnessResult("ok", ModelUsage()))
      for
        (agent, id, updates, _, _, _) <- setup(h, ClientCapabilities())
        _      <- agent.prompt(PromptRequest(id, List(ContentBlock.text("go"))))
        plans  <- updates.get.map(_.collect { case SessionUpdate.Plan(es, _) => es.map(e => e.content -> e.status) })
      yield assertTrue(plans == Vector(
        List("read" -> PlanEntryStatus.Completed, "part A" -> PlanEntryStatus.InProgress),
        List("read" -> PlanEntryStatus.Completed, "part A" -> PlanEntryStatus.Completed),
      ))
    },
    test("an execute call whose arguments name no command is shown with one (IntelliJ shows rawInput.command); others as they are") {
      val test = ToolCallView.shownInput("test", Json.Obj("tests" -> Json.Arr(Json.Str("com.x.ASpec"), Json.Str("com.x.BSpec"))))
      val full = ToolCallView.shownInput("sbta.test", Json.Obj("full" -> Json.Bool(true)))
      val sbt = ToolCallView.shownInput("test", Json.Obj("project" -> Json.Str("core"), "configuration" -> Json.Str("test")))
      val terminal = Json.Obj("command" -> Json.Str("./gradlew test"))
      assertTrue(
        test.get("command").contains(Json.Str("test com.x.ASpec com.x.BSpec")), test.get("tests").isDefined,
        full.get("command").contains(Json.Str("test --full")),
        sbt.get("command").contains(Json.Str("test core --configuration test")),
        ToolCallView.shownInput("execute_terminal_command", terminal).toJson == terminal.toJson,
        ToolCallView.shownInput("read_file", Json.Obj("path" -> Json.Str("A"))).toJson == """{"path":"A"}""",
      )
    },
    test("tool kinds and titles for the IDE's and common tools") {
      assertTrue(
        ToolCallView.kind("apply_patch") == ToolKind.Edit, ToolCallView.kind("search_text") == ToolKind.Search,
        ToolCallView.kind("execute_terminal_command") == ToolKind.Execute, ToolCallView.kind("build_project") == ToolKind.Execute,
        ToolCallView.kind("read_file") == ToolKind.Read, ToolCallView.kind("web_fetch") == ToolKind.Fetch,
        ToolCallView.kind("open_file_in_editor") == ToolKind.Read,
        ToolCallView.bareTitle("apply_patch", Json.Obj("path" -> Json.Str("src/A.scala"))) == "src/A.scala",
        ToolCallView.bareTitle("execute_terminal_command", Json.Obj("command" -> Json.Str("ls"))) == "Run ls", ToolCallView.kind("edit_file") == ToolKind.Edit,
        ToolCallView.kind("list_directory_tree") == ToolKind.Search,
        ToolCallView.title("apply_patch", Json.Obj("input" -> Json.Str("*** Begin Patch\n*** Update File: src/A.scala\n@@\n-a\n+b\n*** End Patch"))) ==
          "Edit src/A.scala",
        ToolCallView.title("search_text", Json.Obj("q" -> Json.Str("SAVE10"))) == "Search for SAVE10",
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)
