package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.{ArgumentsRequest, Ending, ReplyRequest, Tool}
import com.jamesward.zioacp.{AcpAgent, Agent, ClientConnection, PromptTurn, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** An ACP agent as the writer: a loop's writings in a warm session of its own; JSON found in its text; no tools allowed. */
object AcpWriterSpec extends ZIOSpecDefault:

  /** Answers arguments prompts with fenced JSON in two chunks, other prompts with text; asks for a permission first. */
  private final class Fake(client: ClientConnection, prompts: Ref[Vector[(SessionId, String)]], permissions: Ref[Vector[String]],
      opened: Ref[Int] = Unsafe.unsafely(Ref.unsafe.make(0)), slow: Duration = Duration.Zero,
      closed: Ref[Vector[SessionId]] = Unsafe.unsafely(Ref.unsafe.make(Vector.empty[SessionId]))) extends Agent:
    def initialize(request: InitializeRequest) = ZIO.succeed(InitializeResponse(ProtocolVersion.V1))
    override def closeSession(request: CloseSessionRequest) = closed.update(_ :+ request.sessionId).as(CloseSessionResponse())
    def newSession(request: NewSessionRequest) = opened.update(_ + 1) *> Random.nextUUID.map(u => NewSessionResponse(SessionId(u.toString)))
    def prompt(request: PromptRequest, turn: PromptTurn): IO[RpcError, PromptResponse] =
      val text = request.prompt.collect { case ContentBlock.Text(t, _, _) => t }.mkString
      val say = (s: String) => client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(s)))
      for
        _      <- prompts.update(_ :+ (request.sessionId -> text)) *> ZIO.sleep(slow)
        asked  <- client.requestPermission(RequestPermissionRequest(request.sessionId, ToolCallUpdate(ToolCallId("t")),
                    List(PermissionOption(PermissionOptionId("allow"), "Allow", PermissionOptionKind.AllowOnce))))
        _      <- permissions.update(_ :+ asked.outcome.toString)
        _      <- if text.contains("Answer with only the arguments") then say("Here:\n```json\n{\"path\":") *> say(" \"A.java\"}\n```\nDone {ok}.")
                  else say("The file is ") *> say("**A.java**.")
      yield PromptResponse(StopReason.EndTurn, Some(TurnUsage(30, 20, 10)))

  private val read = Tool("read_file", "Reads a file.", Json.Obj("type" -> Json.Str("object"),
    "properties" -> Json.Obj("path" -> Json.Obj("type" -> Json.Str("string"))), "required" -> Json.Arr(Json.Str("path"))))

  def spec = suite("ACP writer")(
    test("arguments from the agent's text (fenced, in chunks, prose around); a reply as its text; each writing in a clean session; permissions refused") {
      for
        prompts     <- Ref.make(Vector.empty[(SessionId, String)])
        permissions <- Ref.make(Vector.empty[String])
        pipe        <- Transport.pipe
        _           <- AcpAgent.serve(pipe._1)(c => ZIO.succeed(Fake(c, prompts, permissions))).forkScoped
        writer      <- AcpWriter.over("fake", pipe._2, WritingPrompts())
        args        <- writer.arguments(ArgumentsRequest("read A", read, Vector.empty))
        reply       <- writer.reply(ReplyRequest("read A", Vector.empty, Ending.Answered))
        asked       <- prompts.get
        refused     <- permissions.get
      yield assertTrue(
        args.value.toJson == """{"path":"A.java"}""", args.inputTokens == 20, args.outputTokens == 10,
        reply.value == "The file is **A.java**.",
        // one after the other, each in a session of its own, with only its own prompt
        asked.size == 2, asked.map(_._1).distinct.size == 2, asked(1)._2.startsWith("You"),
        asked.head._2.contains("read_file"), asked.head._2.contains("Don't use any tool"),
        refused.forall(_.contains("Cancelled")),
      )
    },
    test("every writing in a clean session, made ahead and closed once answered; writings at the same time each in their own") {
      import com.jamesward.zio_decisions.orchestration.ParallelLoop
      for
        prompts     <- Ref.make(Vector.empty[(SessionId, String)])
        permissions <- Ref.make(Vector.empty[String])
        opened      <- Ref.make(0)
        closed      <- Ref.make(Vector.empty[SessionId])
        pipe        <- Transport.pipe
        _           <- AcpAgent.serve(pipe._1)(c => ZIO.succeed(Fake(c, prompts, permissions, opened, 100.millis, closed))).forkScoped
        writer      <- AcpWriter.over("fake", pipe._2, WritingPrompts())
        reply        = writer.reply(ReplyRequest("read A", Vector.empty, Ending.Answered))
        _           <- ParallelLoop.currentLoop.locally(Some("t/task-1/main"))(reply *> reply *> reply.zipPar(reply))
        asked       <- prompts.get
        _           <- closed.get.repeatUntil(_.size == 4).timeout(5.seconds)
        shut        <- closed.get
        made        <- opened.get
      yield assertTrue(asked.map(_._1).distinct.size == 4, shut.toSet == asked.map(_._1).toSet,
        // the four, and a spare made ahead after each was taken
        made <= 5 + 1)
    } @@ TestAspect.withLiveClock,
    test("a writing leaves the loop's FiberRefs its own: the spare session another loop started doesn't bring that loop's") {
      import com.jamesward.zio_decisions.orchestration.ParallelLoop
      for
        prompts     <- Ref.make(Vector.empty[(SessionId, String)])
        permissions <- Ref.make(Vector.empty[String])
        pipe        <- Transport.pipe
        _           <- AcpAgent.serve(pipe._1)(c => ZIO.succeed(Fake(c, prompts, permissions))).forkScoped
        writer      <- AcpWriter.over("fake", pipe._2, WritingPrompts())
        reply        = writer.reply(ReplyRequest("read A", Vector.empty, Ending.Answered))
        // each loop in a fiber of its own (awaited, not joined), as sessions' loops are
        apart        = (z: Task[Any]) => z.fork.flatMap(_.await).flatMap(exit => exit)
        // session a's loop takes the first spare, and starts the next one: in its fiber, with its FiberRefs
        _           <- apart(ZIO.logAnnotate("session", "a")(ParallelLoop.currentLoop.locally(Some("a/task-1/main"))(reply)))
        seen        <- Ref.make((Option.empty[String], Map.empty[String, String]))
        _           <- apart(ZIO.logAnnotate("session", "b")(ParallelLoop.currentLoop.locally(Some("b/task-1/main"))(
                         reply *> (ParallelLoop.currentLoop.get <*> ZIO.logAnnotations).flatMap(seen.set))))
        after       <- seen.get
      yield assertTrue(after._1.contains("b/task-1/main"), after._2.get("session").contains("b"))
    } @@ TestAspect.withLiveClock,
    test("Kiro's sessions: soness's agents in the writer's directory; writing in the tool-less one, a search in the web one") {
      final class Moded(client: ClientConnection, modes: Ref[Vector[(SessionId, String)]], cwds: Ref[Vector[String]]) extends Agent:
        def initialize(request: InitializeRequest) = ZIO.succeed(InitializeResponse(ProtocolVersion.V1))
        def newSession(request: NewSessionRequest) =
          cwds.update(_ :+ request.cwd) *> Random.nextUUID.map(u => NewSessionResponse(SessionId(u.toString), modes = Some(SessionModeState(
            SessionModeId("kiro_default"), List(AcpWriter.WriteAgent, AcpWriter.SearchAgent, "kiro_default").map(m => SessionMode(SessionModeId(m), m))))))
        override def setSessionMode(request: SetSessionModeRequest) =
          modes.update(_ :+ (request.sessionId -> request.modeId.value)).as(SetSessionModeResponse())
        def prompt(request: PromptRequest, turn: PromptTurn) =
          client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text("ok"))).as(PromptResponse(StopReason.EndTurn))
      for
        modes  <- Ref.make(Vector.empty[(SessionId, String)])
        cwds   <- Ref.make(Vector.empty[String])
        pipe   <- Transport.pipe
        _      <- AcpAgent.serve(pipe._1)(c => ZIO.succeed(Moded(c, modes, cwds))).forkScoped
        writer <- AcpWriter.over("kiro", pipe._2, WritingPrompts())
        _      <- writer.reply(ReplyRequest("hi", Vector.empty, Ending.Answered))
        _      <- writer.search("search it")
        set    <- modes.get
        dir    <- cwds.get.map(_.head)
        files  <- ZIO.attemptBlocking(java.nio.file.Files.list(java.nio.file.Path.of(dir).resolve(".kiro/agents")).toArray.map(_.toString).toList)
        search <- ZIO.attemptBlocking(java.nio.file.Files.readString(java.nio.file.Path.of(dir).resolve(s".kiro/agents/${AcpWriter.SearchAgent}.json")))
      yield assertTrue(
        files.exists(_.endsWith(s"${AcpWriter.WriteAgent}.json")), search.contains("\"web_search\""), search.contains("\"allowedTools\""), !search.contains("web_fetch"), search.contains("\"mcpServers\":{}"), search.contains("\"useLegacyMcpJson\":false"),
        // every session starts tool-less; the search's is switched to the web agent, and only it
        set.count(_._2 == AcpWriter.WriteAgent) >= 2, set.count(_._2 == AcpWriter.SearchAgent) == 1,
        set.filter(_._2 == AcpWriter.SearchAgent).map(_._1).toSet.subsetOf(set.filter(_._2 == AcpWriter.WriteAgent).map(_._1).toSet),
      )
    } @@ TestAspect.withLiveClock,
    test("a search answers with its tool call's results as soon as they come: the agent's turn is cancelled, not waited for") {
      val output = Json.Obj("items" -> Json.Arr(Json.Obj("Json" -> Json.Obj("results" -> Json.Arr(
        Json.Obj("title" -> Json.Str("ZIO"), "url" -> Json.Str("https://zio.dev"), "snippet" -> Json.Str("Type-safe.")),
        Json.Obj("url" -> Json.Str("https://github.com/zio/zio")))))))
      // searches, then would write an answer forever: only a cancel ends its turn
      final class Searching(client: ClientConnection, cancelled: Promise[Nothing, Unit]) extends Agent:
        def initialize(request: InitializeRequest) = ZIO.succeed(InitializeResponse(ProtocolVersion.V1))
        def newSession(request: NewSessionRequest) = Random.nextUUID.map(u => NewSessionResponse(SessionId(u.toString)))
        def prompt(request: PromptRequest, turn: PromptTurn) =
          client.sessionUpdate(request.sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(ToolCallId("t1"),
            kind = Some(ToolKind.Search), status = Some(ToolCallStatus.Completed), rawOutput = Some(output)))) *>
            turn.awaitCancel.as(PromptResponse(StopReason.Cancelled))
        override def cancel(notification: CancelNotification) = cancelled.succeed(()).unit
      for
        cancelled <- Promise.make[Nothing, Unit]
        pipe      <- Transport.pipe
        _         <- AcpAgent.serve(pipe._1)(c => ZIO.succeed(Searching(c, cancelled))).forkScoped
        writer    <- AcpWriter.over("kiro", pipe._2, WritingPrompts())
        answer    <- writer.search("zio").timeoutFail(RuntimeException("waited for the turn"))(5.seconds)
        _         <- cancelled.await.timeoutFail(RuntimeException("not cancelled"))(5.seconds)
      yield assertTrue(answer == "- [ZIO](https://zio.dev) — Type-safe.\n- [https://github.com/zio/zio](https://github.com/zio/zio)",
        AcpWriter.searchResults(Json.Obj("items" -> Json.Arr())).isEmpty)
    } @@ TestAspect.withLiveClock,
    test("Kiro writes with Sonnet unless its command names a model; other agents' commands are as given") {
      import com.jamesward.zioacp.AgentCommand
      val (kiro, model) = AcpWriter.launched(AgentCommand("kiro-cli", List("acp")))
      val (named, none) = AcpWriter.launched(AgentCommand("/usr/bin/kiro-cli", List("acp", "--model", "claude-haiku-5.5")))
      val (other, otherModel) = AcpWriter.launched(AgentCommand("npx", List("-y", "claude-agent-acp")))
      assertTrue(
        kiro.args == List("acp", "--agent", AcpWriter.WriteAgent, "--model", "claude-sonnet-5.5"), model.contains("claude-sonnet-5.5"),
        named.args == List("acp", "--model", "claude-haiku-5.5", "--agent", AcpWriter.WriteAgent), none.isEmpty,
        other.args == List("-y", "claude-agent-acp"), otherModel.isEmpty)
    },
    test("the writer's prompts: the project's agent instructions left out; the last steps' results cut longer than the older ones'") {
      import com.jamesward.zio_decisions.orchestration.AgentLoop.{Outcome, Step}
      val context = "Project directory: /p\nFiles (…):\nA.scala" + ProjectContext.InstructionsHeading + "AGENTS.md):\nbe nice" +
        "\nSource dependencies (…): /q\n\nThe MCP server sbta's instructions (for its tools):\nuse compile"
      val big = Json.Str("y" * 30000)
      val steps = (1 to 4).map(n => Step(n, "read_file", Json.Obj("path" -> Json.Str(s"F$n")), Outcome.Succeeded(big))).toVector
      val prompt = WritingPrompts().argumentsPrompt(ArgumentsRequest("read A", read, steps, context = Some(context)))
      def kept(n: Int) = prompt.drop(prompt.indexOf(s"$n. read_file")).takeWhile(_ != '…').count(_ == 'y')
      assertTrue(
        prompt.contains("A.scala"), !prompt.contains("be nice"), prompt.contains("Source dependencies"), prompt.contains("use compile"),
        kept(1) <= WritingPrompts.OlderLimit, kept(2) <= WritingPrompts.OlderLimit,
        kept(3) > WritingPrompts.OlderLimit, kept(3) <= WritingPrompts.RecentLimit, kept(4) > WritingPrompts.OlderLimit)
    },
    test("the agents of SONESS_ACP_AGENTS") {
      assertTrue(
        Soness.parseAgents("kiro=kiro-cli acp; claude=npx -y @agentclientprotocol/claude-agent-acp ;bad").map((n, c) => (n, c.command, c.args)) ==
          List(("kiro", "kiro-cli", List("acp")), ("claude", "npx", List("-y", "@agentclientprotocol/claude-agent-acp"))),
        AcpWriter.jsonObject("x {not json} then {\"a\": {\"b\": \"}\"}} y").map(_.toJson).contains("""{"a":{"b":"}"}}"""),
      )
    },
  ) @@ TestAspect.timeout(30.seconds)
