package com.jamesward.soness

import com.jamesward.zio_evals.*
import com.jamesward.zioacp.{AcpAgent, AcpClient, AgentConnection, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

/** The ACP surface, with stub harnesses that call the orders MCP server: no model is involved. */
object SonessAgentSpec extends ZIOSpecDefault:

  /** Calls `calls` in order and answers with their results and the model it was made for. */
  private final case class Scripted(model: String, calls: List[(String, Json.Obj)]) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[com.jamesward.zio_decisions.orchestration.AgentLoop.Document], context: Option[String], steering: UIO[List[String]]): Task[HarnessResult] =
      ZIO.foreach(calls)((name, arguments) => toolset.call(name, arguments).either)
        .map: results =>
          HarnessResult(s"$model: ${results.map(_.fold(_ => "failed", _ => "ok")).mkString(",")}", ModelUsage(1, 10, 2, 3, 30, 3))

  private val catalog: HarnessCatalog = HarnessCatalog.make(
    List(
      "model-a" -> Scripted("model-a", List(
        "list_orders" -> Json.Obj("customerId" -> Json.Str("C0046")),
        "get_order" -> Json.Obj("orderId" -> Json.Str("nope")),
      )),
      "model-b" -> Scripted("model-b", List("list_customers" -> Json.Obj())),
    ),
    "model-a",
  ).toOption.get

  /** Bedrock's two models, and an ACP agent. */
  private val withAcp: HarnessCatalog = HarnessCatalog.make(
    List(
      "model-a" -> Scripted("model-a", List("list_orders" -> Json.Obj("customerId" -> Json.Str("C0046")))),
      "model-b" -> Scripted("model-b", List("list_customers" -> Json.Obj())),
      "acp:kiro" -> Scripted("acp:kiro", Nil),
    ),
    "model-a",
  ).toOption.get

  private def loop(http: ZEnvironment[Client]) =
    SonessAgentLoop(client => ZIO.scope.flatMap(parent => SonessAgent.make(client, http, catalog, parent)))

  /** An ACP client connected in memory to a soness agent with `catalog`. */
  private def connected(http: ZEnvironment[Client], catalog: HarnessCatalog = catalog,
      updates: Ref[Vector[SessionUpdate]] = Unsafe.unsafely(Ref.unsafe.make(Vector.empty[SessionUpdate]))) =
    for
      parent <- ZIO.scope
      pipe   <- Transport.pipe
      _      <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent)).forkScoped
      agent  <- AcpClient.connect(pipe._2, new com.jamesward.zioacp.Client:
                  def sessionUpdate(notification: SessionNotification): UIO[Unit] = updates.update(_ :+ notification.update)
                  def requestPermission(request: RequestPermissionRequest) =
                    ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))
                )
      _      <- agent.initialize(InitializeRequest(ProtocolVersion.V1))
    yield agent

  private def answer(agent: AgentConnection, session: SessionId) =
    agent.prompt(PromptRequest(session, List(ContentBlock.text("go")))).map(r => TurnMetrics.fromMeta(r.meta))

  private def current(options: List[SessionConfigOption]): Map[String, String] =
    options.collect { case o: SessionConfigOption.Select => o.id.value -> o.currentValue.value }.toMap

  def spec = suite("SonessAgent")(
    test("reports tool calls, the answer and the turn metrics") {
      for
        url    <- OrdersMcpServer.serve
        http   <- ZIO.environment[Client]
        result <- loop(http).run("go", "model-a", List(McpServerConfig("orders", url)), AgentPolicy.default)
        passed <- Checks.evaluateAll(List(EvalCheck.ToolCalled("list_orders"), EvalCheck.ToolInputContains("get_order", "nope")), result)
      yield assertTrue(
        result.answer == "model-a: ok,failed",
        result.toolCalls == 2,
        result.iterations == 4,
        result.inputTokens == 40L,
        result.outputTokens == 5L,
        passed,
        result.events.collect { case TranscriptEvent.ToolResult(name, text, failed) => (name, failed, text.contains("O10460")) } ==
          List(("list_orders", false, true), ("get_order", true, false)),
      )
    },
    test("the writer is the model option, a drill-down by provider (Bedrock, the ACP agents); no mode selector and no work option") {
      for
        http     <- ZIO.environment[Client]
        agent    <- connected(http, withAcp)
        session  <- agent.newSession(NewSessionRequest("/", Nil))
        options   = session.configOptions.getOrElse(Nil)
        before   <- answer(agent, session.sessionId)
        model    <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("model"), SessionConfigValue.ValueId(SessionConfigValueId("model-b"))))
        after    <- answer(agent, session.sessionId)
        badModel <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("model"), SessionConfigValue.ValueId(SessionConfigValueId("nope")))).flip
        badMode  <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("mode"), SessionConfigValue.ValueId(SessionConfigValueId("acp")))).flip
        badWork  <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("work"), SessionConfigValue.ValueId(SessionConfigValueId("parallel")))).flip
        // a catalog of stubs has no decision models or searches to choose
        badSearch <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("web_search"), SessionConfigValue.ValueId(SessionConfigValueId("kiro")))).flip
        noMode   <- agent.setSessionMode(SetSessionModeRequest(session.sessionId, SessionModeId("acp"))).either
        acp      <- agent.setSessionConfigOption(SetSessionConfigOptionRequest(
                      session.sessionId, SessionConfigId("model"), SessionConfigValue.ValueId(SessionConfigValueId("acp:kiro"))))
        byAgent  <- answer(agent, session.sessionId)
      yield
        val groups = options.collect { case o: SessionConfigOption.Select => o.options }.collect {
          case SessionConfigSelectOptions.Grouped(gs) => gs.map(g => g.name -> g.options.map(_.value.value)) }
        assertTrue(
          session.modes.isEmpty,
          options.collect { case o: SessionConfigOption.Select => (o.id.value, o.category) } == List(("model", Some(SessionConfigOptionCategory.Other("_llm_model")))),
          groups == List(List("Bedrock" -> List("model-a", "model-b"), "Kiro" -> List("acp:kiro"))),
          current(options) == Map("model" -> "model-a"),
          // IntelliJ IDEA rejects a model option whose name is its value
          options.collect { case o: SessionConfigOption.Select => o.options.all }.flatten.forall(o => o.name != o.value.value),
          before.map(_.model).contains("model-a"),
          current(model.configOptions) == Map("model" -> "model-b"),
          after.map(_.model).contains("model-b"),
          badModel.message.contains("unknown model: nope"),
          badMode.message.contains("unknown config option: mode"),
          badWork.message.contains("unknown config option: work"),
          badSearch.message.contains("unknown config option: web_search"),
          noMode.isLeft,
          current(acp.configOptions) == Map("model" -> "acp:kiro"),
          byAgent.map(_.model).contains("acp:kiro"),
        )
    },
    test("decision model, LLM model and web search: each a drill-down by provider, switched on its own; no search is noted once") {
      final case class Named(name: String) extends Harness:
        def run(prompt: String, toolset: Toolset, documents: Seq[com.jamesward.zio_decisions.orchestration.AgentLoop.Document],
            context: Option[String], steering: UIO[List[String]]): Task[HarnessResult] =
          ZIO.succeed(HarnessResult(name, ModelUsage()))
      val decisions = List(HarnessCatalog.Choice("typesafe:jev-latest", "jev-latest", "Jev", "Jev"),
        HarnessCatalog.Choice("openai:gpt-6-luna", "gpt-6-luna", "OpenAI's", "OpenAI"))
      val searches = List(HarnessCatalog.Choice("kiro", "Kiro", "Kiro's", "Kiro"),
        HarnessCatalog.Choice("bedrock", "Bedrock (unavailable)", "Bedrock's", "Bedrock", Some("needs a GPT-5.4+ model on Mantle")))
      val composed = HarnessCatalog.composed(List("model-a", "acp:kiro"), decisions, searches,
        HarnessCatalog.Selection(Some("typesafe:jev-latest"), "model-a", None),
        List(HarnessCatalog.Choice("acp:claude", "Claude (unavailable)", "Claude writes", "Claude", Some("set SONESS_ACP_AGENTS"))))(sel =>
          Some(Named(s"${sel.decision.getOrElse("")}|${sel.model}|${sel.search.getOrElse("none")}"))).toOption.get
      def set(agent: AgentConnection, session: SessionId, id: String, value: String) =
        agent.setSessionConfigOption(SetSessionConfigOptionRequest(session, SessionConfigId(id), SessionConfigValue.ValueId(SessionConfigValueId(value))))
      def reply(agent: AgentConnection, session: SessionId, updates: Ref[Vector[SessionUpdate]]) =
        updates.set(Vector.empty) *> agent.prompt(PromptRequest(session, List(ContentBlock.text("go")))) *> updates.get.map: us =>
          (us.collect { case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) => t }.mkString,
            us.collect { case SessionUpdate.AgentThoughtChunk(ContentBlock.Text(t, _, _), _, _) => t }.mkString)
      for
        http     <- ZIO.environment[Client]
        updates  <- Ref.make(Vector.empty[SessionUpdate])
        agent    <- connected(http, composed, updates)
        session  <- agent.newSession(NewSessionRequest("/", Nil))
        options   = session.configOptions.getOrElse(Nil)
        first    <- reply(agent, session.sessionId, updates)
        second   <- reply(agent, session.sessionId, updates)
        _        <- set(agent, session.sessionId, "decision_model", "openai:gpt-6-luna")
        _        <- set(agent, session.sessionId, "model", "acp:kiro")
        searched <- set(agent, session.sessionId, "web_search", "kiro")
        third    <- reply(agent, session.sessionId, updates)
        _        <- set(agent, session.sessionId, "web_search", "none")
        fourth   <- reply(agent, session.sessionId, updates)
        bad      <- set(agent, session.sessionId, "decision_model", "nope").flip
        greyed   <- set(agent, session.sessionId, "web_search", "bedrock").flip
        greyedM  <- set(agent, session.sessionId, "model", "acp:claude").flip
      yield
        val shape = options.collect { case o: SessionConfigOption.Select => o.id.value -> (o.options match
          case SessionConfigSelectOptions.Grouped(gs) => gs.map(g => g.name -> g.options.map(_.value.value))
          case SessionConfigSelectOptions.Ungrouped(os) => List("" -> os.map(_.value.value))) }
        assertTrue(
          shape == List(
            "decision_model" -> List("Jev" -> List("typesafe:jev-latest"), "OpenAI" -> List("openai:gpt-6-luna")),
            "model" -> List("Bedrock" -> List("model-a"), "Kiro" -> List("acp:kiro"), "Claude" -> List("acp:claude")),
            "web_search" -> List("Kiro" -> List("kiro"), "Bedrock" -> List("bedrock"), "None" -> List("none"))),
          // an unavailable one is shown, named and marked so, saying why, and can't be chosen
          options.collect { case o: SessionConfigOption.Select => o.options.all }.flatten.filter(_.meta.isDefined).map(o => o.value.value -> o.name) ==
            List("acp:claude" -> "Claude (unavailable)", "bedrock" -> "Bedrock (unavailable)"),
          greyed.message.contains("Bedrock is unavailable: needs a GPT-5.4+ model on Mantle"),
          greyedM.message.contains("Claude is unavailable: set SONESS_ACP_AGENTS"),
          options.collect { case o: SessionConfigOption.Select if o.id.value == "model" => o.category }.flatten == List(SessionConfigOptionCategory.Other("_llm_model")),
          options.collect { case o: SessionConfigOption.Select if o.id.value == "decision_model" => o.category }.flatten == List(SessionConfigOptionCategory.Model),
          options.collect { case o: SessionConfigOption.Select if o.id.value == "model" => o.name } == List("LLM provider"),
          current(options) == Map("decision_model" -> "typesafe:jev-latest", "model" -> "model-a", "web_search" -> "none"),
          // no search: the first turn says so, once
          first._1.contains("typesafe:jev-latest|model-a|none"), first._2.contains("web search is off"),
          !second._2.contains("web search"),
          current(searched.configOptions) == Map("decision_model" -> "openai:gpt-6-luna", "model" -> "acp:kiro", "web_search" -> "kiro"),
          third._1.contains("openai:gpt-6-luna|acp:kiro|kiro"), !third._2.contains("web search"),
          // switched off again: noted again
          fourth._2.contains("web search is off"),
          bad.message.contains("unknown decision model: nope"),
        )
    },
    test("skips SSE servers and servers that fail to connect, and keeps the others") {
      for
        http    <- ZIO.environment[Client]
        url     <- OrdersMcpServer.serve
        agent   <- connected(http)
        init    <- agent.initialize(InitializeRequest(ProtocolVersion.V1))
        mixed   <- agent.newSession(NewSessionRequest("/", List(
                     McpServer.Stdio("ide", "/nonexistent/ide", List("stdioMcpServer")),
                     McpServer.Sse("events", "http://127.0.0.1:1/sse"),
                     McpServer.Http("down", "http://127.0.0.1:1/mcp"),
                     McpServer.Http("orders", url),
                   )))
        result  <- agent.prompt(PromptRequest(mixed.sessionId, List(ContentBlock.text("go"))))
        logged  <- ZTestLogger.logOutput.map(_.map(_.message()))
      yield assertTrue(
        init.agentCapabilities.mcpCapabilities.http,
        result.stopReason == StopReason.EndTurn,
        TurnMetrics.fromMeta(result.meta).map(_.toolCalls).contains(2),
        logged.exists(_.contains("skipping SSE MCP servers, which soness doesn't support: events (sse)")),
        logged.exists(_.contains("skipping MCP server ide (stdio: /nonexistent/ide)")),
        logged.exists(_.contains("skipping MCP server down (http://127.0.0.1:1/mcp)")),
        logged.exists(_.contains("1 MCP servers, 3 tools")),
      )
    },
    test("a failed turn answers the prompt with an error, and says which calls ran before it stopped") {
      val failing = new Harness:
        def run(prompt: String, toolset: Toolset, documents: Seq[com.jamesward.zio_decisions.orchestration.AgentLoop.Document], context: Option[String], steering: UIO[List[String]]): Task[HarnessResult] =
          toolset.call("read_file", zio.json.ast.Json.Obj("path" -> zio.json.ast.Json.Str("nope.txt"))).ignore *>
            ZIO.fail(RuntimeException("Bedrock 400 Validation: bad"))
      for
        http    <- ZIO.environment[Client]
        updates <- Ref.make(Vector.empty[SessionUpdate])
        agent   <- connected(http, HarnessCatalog.make(List("model-a" -> failing), "model-a").toOption.get, updates)
        dir     <- ZIO.attempt(java.nio.file.Files.createTempDirectory("failed")).orDie
        session <- agent.newSession(NewSessionRequest(dir.toString, Nil))
        error   <- agent.prompt(PromptRequest(session.sessionId, List(ContentBlock.text("go")))).flip.timeout(10.seconds)
        said    <- updates.get.map(_.collect { case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) => t })
      yield assertTrue(error.exists(_.message.contains("Bedrock 400 Validation: bad")),
        // the user is told what ran before it stopped
        said.exists(t => t.contains("stopped before it finished") && t.contains("read_file")))
    },
    test("model display names differ from the model ids") {
      assertTrue(
        SonessAgent.modelName("openai.gpt-oss-120b") == "gpt-oss-120b (openai)",
        SonessAgent.modelName("us.anthropic.claude-sonnet-4-5-20250929-v1:0") == "claude-sonnet-4-5-20250929 (anthropic)",
        SonessAgent.modelName("amazon.nova-pro-v1:0") == "nova-pro (amazon)",
        SonessAgent.modelName("fake") == "fake (Bedrock)",
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
