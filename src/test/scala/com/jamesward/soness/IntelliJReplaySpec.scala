package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.*
import com.jamesward.soness.FakeBedrock.Reply
import com.jamesward.zio_decisions.typesafe.FakeJev
import zio.*
import zio.http.Status
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/**
 * End to end with IntelliJ IDEA's ACP client: soness runs as a subprocess (its `Main`, as an editor launches it) and
 * receives the messages IntelliJ IDEA 2026.2.3 sent it (`intellij-idea-2026.2.3.jsonl`, captured from the IDE's ACP
 * transport log). Its session/new carries the IDE's own MCP server, `jetbrains_air_ide`, a stdio server sent with
 * `"type":"stdio"`, here launched as `IdeMcpFixture`, next to the orders MCP server over HTTP. Jev and Bedrock are local
 * fakes, so nothing leaves the machine. `LiveE2ESpec` is the same path against the real services.
 */
object IntelliJReplaySpec extends ZIOSpecDefault:

  /** The fakes' configuration; real keys in this JVM's environment are replaced so the subprocess can't use them. */
  private def env(bedrock: String, jev: String): Map[String, String] =
    Map(
      "SONESS_BEDROCK_API" -> "mantle",
      "SONESS_BEDROCK_ENDPOINT" -> bedrock,
      "AWS_BEARER_TOKEN_BEDROCK" -> "dev",
      "BEDROCK_MODEL_ID" -> "fake",
      // offered in the model picker; "missing" fails the startup check and is left out
      "SONESS_BEDROCK_MODELS" -> "fake,missing,fake-2",
      "TYPESAFE_API_KEY" -> "dev",
      "SONESS_TYPESAFE_ENDPOINT" -> jev,
      // the replay is of IntelliJ's messages, which answer no permission requests: the guard has its own spec
      "SONESS_GUARD" -> "off",
    )

  /** The fake Bedrock: rejects `missing`, answers the startup check and the summary, and records every request. */
  private def bedrock(requests: Ref[Vector[Json.Obj]]) =
    FakeBedrock.serveUrl: body =>
      requests.update(_ :+ body).as:
        if FakeBedrock.model(body).contains("missing") then Reply.Failure(Status.NotFound, "model not found")
        else if FakeBedrock.messages(body).exists(_.toJson.contains("Reply with OK.")) then Reply.Text("OK")
        // the router's command, as the skill read explains it, else a guess
        else if FakeBedrock.isArguments(body) then
          if body.toJson.contains("Takes no parameters") then Reply.Text("""{"command":"get_project_info"}""")
          else Reply.Text("""{"command":"say hello"}""")
        else if FakeBedrock.messages(body).exists(_.toJson.contains("say hello")) then Reply.Text("Hello!")
        else Reply.Text("The project is soness.")

  /** What one replay of the capture produced. */
  private final case class Replay(
    init: Json.Obj,
    session: Json.Obj,
    updates: List[Json.Obj],
    prompt: Json.Obj,
    logged: Chunk[String],
    bedrock: Vector[Json.Obj],
    cwd: String,
  )

  /**
   * Replays the capture against soness started with `args`, its `jetbrains_air_ide` server launched as
   * `IdeMcpFixture` (the IDE's own command and arguments substituted), router-only when `routerOnly`, and the prompt
   * `prompt` when given (else the captured one).
   */
  private def replay(
    args: List[String],
    routerOnly: Boolean = false,
    prompt: Option[String] = None,
    jevScript: FakeJev.Answers = JevScript.callThenAnswer("get_project_info"),
    withOrders: Boolean = true,
    skillRoot: Option[java.nio.file.Path] = None,
    fixture: String = "com.jamesward.soness.IdeMcpFixture",
    logFile: Option[String] = None,
  ): ZIO[Scope, Throwable, Replay] =
    for
      mcpUrl    <- OrdersMcpServer.serve
      requests  <- Ref.make(Vector.empty[Json.Obj])
      llm       <- bedrock(requests)
      jev       <- FakeJev.serve(jevScript)
      peer      <- launch(env(llm.encode, jev.encode) ++ logFile.map("SONESS_LOG_FILE" -> _), args)
      lines     <- intellijCapture
      cwd       <- property("user.dir")
      captured   = if withOrders then lines(1) else lines(1).replace(""",{"type":"http","name":"orders","url":"${MCP_URL}","headers":[]}""", "")
      newSession <- intellijNewSession(captured, cwd, mcpUrl, routerOnly, skillRoot, fixture)
      _         <- peer.send(lines(0))
      (_, init) <- peer.awaitResponse(1)
      _         <- peer.send(newSession)
      (_, session) <- peer.awaitResponse(2)
      sessionId  = str(session, "result", "sessionId").getOrElse("")
      _         <- peer.send(prompt.fold(lines(2).replace("${SESSION_ID}", sessionId))(promptRequest(3, sessionId, _)))
      (turn, prompt) <- peer.awaitResponse(3)
      // the session's commands are announced 200 ms after session/new: a turn faster than that (the native
      // executable's) ends first
      late      <- if turn.exists(announcesCommands) then ZIO.succeed(Nil) else peer.collect(_.exists(announcesCommands), 3.seconds)
      updates    = turn ++ late
      logged    <- peer.logged
      sent      <- requests.get
    yield Replay(init, session, updates, prompt, logged, sent, cwd)

  private def announcesCommands(message: Json.Obj): Boolean =
    str(message, "params", "update", "sessionUpdate").contains("available_commands_update")

  private def debugLines(lines: Chunk[String]): Chunk[String] = lines.filter(_.contains("level=DEBUG"))

  /** The session's logged coverage by capability (`ToolResolver.Resolution.summary`). */
  private def coverage(lines: Chunk[String]): Option[String] = lines.find(_.contains("The session's tools by capability"))

  def spec = suite("IntelliJ IDEA replay")(
    test("a SIGTERM stops the agent at once, even with nothing to read on its stdin") {
      for
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- bedrock(requests)
        jev      <- FakeJev.serve(JevScript.answer)
        soness   <- AcpSubprocess.sonessCommand(jvmOptions = Nil)
        process  <- ZIO.acquireRelease(ZIO.attemptBlocking:
                      val b = ProcessBuilder((soness._1 :: soness._2)*)
                      env(llm.encode, jev.encode).foreach((k, v) => b.environment().put(k, v))
                      b.environment().put("SONESS_ACP_AGENTS", "")
                      b.redirectError(ProcessBuilder.Redirect.DISCARD).start()
                    )(p => ZIO.succeed(p.destroyForcibly()))
        _        <- ZIO.attemptBlocking:
                      process.getOutputStream.write("""{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":1}}""".getBytes)
                      process.getOutputStream.write('\n'); process.getOutputStream.flush()
                      java.io.BufferedReader(java.io.InputStreamReader(process.getInputStream)).readLine()
        stopped  <- ZIO.attemptBlocking { process.destroy(); process.waitFor(8, java.util.concurrent.TimeUnit.SECONDS) }.timed
      yield assertTrue(stopped._2, stopped._1 < 8.seconds)
    } @@ TestAspect.withLiveClock,
    test("initialize, session/new with the IDE's stdio MCP server, and a prompt answered with that server's tool") {
      for
        r        <- replay(Nil)
        result    = r.session.get("result").getOrElse(Json.Null)
        sessionId = str(result, "sessionId").getOrElse("")
        kinds     = r.updates.flatMap(u => str(u, "params", "update", "sessionUpdate"))
        output    = r.updates.flatMap(u => field(u, "params", "update", "rawOutput")).map(_.toJson)
      yield assertTrue(
        str(r.init, "result", "agentInfo", "name").contains("soness"),
        field(r.init, "result", "agentCapabilities", "mcpCapabilities", "http").contains(Json.Bool(true)),
        r.session.get("error").isEmpty,
        sessionId.nonEmpty,
        field(result, "modes").isEmpty,
        configOption(result, "mode").isEmpty,
        // every provider shown, each one without its credentials or tool marked unavailable
        configOption(result, "decision_model").flatMap(o => str(o, "currentValue")).contains("typesafe:jev-latest"),
        configOption(result, "decision_model").toList.flatMap(values).size == 2,
        configOption(result, "web_search").toList.flatMap(values) == List("kiro", "bedrock", "brave", "none"),
        configOption(result, "web_search").flatMap(o => str(o, "currentValue")).contains("none"),
        configOption(result, "work").isEmpty,
        configOption(result, "model").flatMap(o => str(o, "category")).contains("_llm_model"),
        configOption(result, "model").flatMap(o => str(o, "currentValue")).contains("fake"),
        // Bedrock's models, and Kiro shown unavailable (no ACP agents here)
        configOption(result, "model").toList.flatMap(values) == List("fake", "fake-2", "acp:kiro"),
        // grouped by provider: Bedrock's models
        configOption(result, "model").toList.flatMap(o => field(o, "options").flatMap(_.asArray).toList.flatten)
          .map(g => str(g, "name")) == List(Some("Bedrock"), Some("Kiro")),
        configOption(result, "model").toList.flatMap(o => field(o, "options").flatMap(_.asArray).toList.flatten)
          .flatMap(g => field(g, "options").flatMap(_.asArray).toList.flatten).map(o => str(o, "name")) ==
          List(Some("fake (Bedrock)"), Some("fake-2 (Bedrock)"), Some("Kiro (unavailable)")),
        r.logged.exists(_.contains("Bedrock model missing is unavailable")),
        r.logged.exists(_.contains("2 MCP servers, 4 tools")),
        !r.logged.exists(_.contains("skipping")),
        r.updates.forall(u => str(u, "method").contains("session/update") && str(u, "params", "sessionId").contains(sessionId)),
        // the turn's updates; the session's commands are announced too, as session/new answers
        // what the loop does between calls comes as thoughts (here, writing the reply; the call took no arguments)
        // first, the note that the session has no web search
        kinds.filterNot(_ == "available_commands_update") == List("agent_thought_chunk", "tool_call", "tool_call_update", "agent_thought_chunk", "agent_message_chunk"),
        r.updates.flatMap(u => field(u, "params", "update", "content").flatMap(c => str(c, "text"))).exists(_.contains("Writing the reply")),
        r.updates.exists(u => str(u, "params", "update", "sessionUpdate").contains("available_commands_update")),
        r.updates.flatMap(u => str(u, "params", "update", "title")) == List("get_project_info"),
        // the IDE server got the session's environment and runs in its directory
        output.exists(o => o.contains(s""""projectPath":"${r.cwd}"""") && o.contains(s""""workingDirectory":"${r.cwd}"""")),
        answer(r.updates) == "The project is soness.",
        str(r.prompt, "result", "stopReason").contains("end_turn"),
        field(r.prompt, "result", "_meta", "soness", "toolCalls").contains(Json.Num(1)),
        field(r.prompt, "result", "_meta", "soness", "ending").contains(Json.Str("answered")),
        (field(r.prompt, "result", "_meta", "soness", "jevCalls").flatMap(_.asNumber).map(_.value.intValue).getOrElse(0) > 0),
        // Bedrock is given no agent tool: the startup check, the summary, and arguments as one forced call
        r.bedrock.nonEmpty,
        r.bedrock.forall(FakeBedrock.offersNoTools),
        r.bedrock.exists(_.toJson.contains("get_project_info")),
        debugLines(r.logged).isEmpty,
      )
    },
    test("IntelliJ's own router (its mock): its routed tools cover files and the build, so no implicit file tool or shell") {
      for
        r <- replay(Nil, prompt = Some("say hello"), withOrders = false, jevScript = JevScript.answer,
               fixture = "com.jamesward.soness.IntellijMcpFixture")
        c  = coverage(r.logged).getOrElse("")
      yield assertTrue(
        c.contains("FileRead: read_file"), c.contains("FileWrite: create_new_file"), c.contains("FilePatch: apply_patch"),
        c.contains("Build: build_project"), !c.contains("shell stands in"),
        answer(r.updates) == "Hello!",
      )
    },
    test("IntelliJ's router-only IDE server: a prompt no tool fits is answered, not failed (\"say hello\")") {
      for
        // as in IntelliJ: the IDE's server is the session's only one
        r     <- replay(Nil, routerOnly = true, prompt = Some("say hello"), withOrders = false,
                   jevScript = JevScript.callThenAnswer("execute_tool"))
        calls  = r.updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call_update"))
        result = r.session.get("result").getOrElse(Json.Null)
      yield assertTrue(
        r.logged.exists(_.contains("1 MCP servers, 1 tools")),
        str(r.prompt, "result", "stopReason").contains("end_turn"),
        answer(r.updates) == "Hello!",
        calls.nonEmpty,
        calls.forall(u => str(u, "params", "update", "status").contains("failed")),
        field(r.prompt, "result", "_meta", "soness", "ending").contains(Json.Str("answered")),
        r.bedrock.forall(FakeBedrock.offersNoTools),
        str(result, "sessionId").nonEmpty,
      )
    },
    test("IntelliJ's router-only IDE server and a router skill in the session's roots: Jev reads its way to the command") {
      for
        root   <- SkillsSpec.tree(SkillsSpec.routerSkill*)
        r      <- replay(
                    Nil,
                    routerOnly = true,
                    prompt = Some("What is this project?"),
                    withOrders = false,
                    jevScript = JevScript.readThenCall(
                      List("project-info", "project-info: reference/tools.md", "project-info: get_project_info"),
                      "execute_tool",
                    ),
                    skillRoot = Some(root),
                  )
        calls   = r.updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call"))
        results = r.updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call_update"))
      yield assertTrue(
        // the router skill, besides this project's own (`.agents/skills`, when extracted)
        r.logged.exists(_.contains("1 MCP servers, 1 tools (and web_fetch), ")),
        calls.flatMap(u => field(u, "params", "update", "rawInput")).map(_.toJson) == List("""{"command":"get_project_info"}"""),
        results.flatMap(u => str(u, "params", "update", "status")) == List("completed"),
        results.exists(u => field(u, "params", "update", "rawOutput").exists(_.toJson.contains("projectPath"))),
        answer(r.updates) == "The project is soness.",
        str(r.prompt, "result", "stopReason").contains("end_turn"),
      )
    },
    test("--debug logs the ACP messages, the prompt, each tool call, Bedrock and Jev exchange, and keeps a copy in its file") {
      for
        file   <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempFile("soness", ".log")))(f => ZIO.attemptBlocking(Files.deleteIfExists(f)).ignore)
        r      <- replay(List("--debug"), logFile = Some(file.toString))
        saved  <- ZIO.attemptBlocking(Files.readAllLines(file).asScala.toList)
        debug   = debugLines(r.logged)
      yield assertTrue(
        debug.exists(l => l.contains("ACP receive: ") && l.contains("session/prompt")),
        debug.exists(l => l.contains("ACP send: ") && l.contains("agent_message_chunk")),
        debug.exists(_.contains("Which Platinum-tier customers are in EMEA?")),
        debug.exists(_.contains("tool call call-1 get_project_info: {}")),
        debug.exists(l => l.contains("tool call call-1 get_project_info returned") && l.contains("projectPath")),
        debug.exists(_.contains("Bedrock fake exchange")),
        debug.exists(_.contains("Jev exchange in")),
        debug.exists(_.contains("answer: The project is soness.")),
        debug.filter(_.contains("tool call call-1")).forall(_.contains("session=")),
        r.logged.exists(l => l.contains("level=INFO") && l.contains("2 MCP servers, 4 tools")),
        saved.exists(l => l.contains("level=DEBUG") && l.contains("tool call call-1 get_project_info")),
      )
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(2.minutes)

