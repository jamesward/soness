package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.*
import com.jamesward.soness.OrdersMcpServer.orders
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * Live end to end: soness as a subprocess, driven with IntelliJ IDEA's captured `initialize` and `session/new` (its
 * stdio IDE server launched as `IdeMcpFixture`, next to the orders MCP server over HTTP), against the real Bedrock and
 * Jev. For every Bedrock model the agent offers it checks basic inference (a session without tools, where Jev is
 * offered only the endings) and a tool-using prompt that Jev orchestrates: the tool calls reported as ACP updates, the
 * answer, and the turn's metrics. It also sends a prompt no tool fits ("say hello") as IntelliJ does: to that
 * session, and to one whose only server is the IDE's in router-only mode (one `execute_tool` router, as IntelliJ
 * IDEA offers ACP agents by default), which must be answered rather than fail. That router session also has a skill
 * in its roots documenting the router's commands (as IntelliJ's skill views do), and a question only the IDE can answer
 * must be answered by reading the skill and calling the router with the documented command. It checks the protocol and the answers,
 * not speed or cost (`SonessEvalSpec` reports those).
 *
 * soness reads its configuration from this JVM's environment (see the README): it needs `TYPESAFE_API_KEY`,
 * `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` and is skipped without them; `SONESS_BEDROCK_MODELS` adds models.
 * It calls paid APIs: a few calls per model.
 */
object LiveE2ESpec extends ZIOSpecDefault:

  private val customer = "C0046"
  private val customerOrders = orders.filter(_.customerId == customer).map(_.orderId)

  private val toolPrompt = s"List the order IDs of customer $customer, with each order's status."
  private val inferencePrompt = "What is 17 + 25? Reply with just the number."
  private val greeting = "say hello"
  private val capabilitiesQuestion = "what tools do you have available?"
  private val projectQuestion = "What is the name and directory of the project open in the IDE?"

  /** What one prompt turn did. */
  private final case class Turn(updates: List[Json.Obj], result: Json):
    def text: String = answer(updates)
    def toolCalls: List[(String, String)] =
      updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call"))
        .flatMap(u => str(u, "params", "update", "toolCallId").zip(str(u, "params", "update", "title")))
    def finished: Map[String, String] =
      updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call_update"))
        .flatMap(u => str(u, "params", "update", "toolCallId").zip(str(u, "params", "update", "status"))).toMap
    def metric(name: String): Option[Json] = field(result, "_meta", "soness", name)
    def count(name: String): Long = metric(name).flatMap(_.asNumber).map(_.value.longValue).getOrElse(0L)

  /** One model's results: the failed checks, and a report line. */
  private final case class Outcome(model: String, failures: List[String], report: String)

  private def select(peer: Peer, sessionId: String, model: String): Task[Unit] =
    peer.call("session/set_config_option", Json.Obj(
      "sessionId" -> Json.Str(sessionId), "configId" -> Json.Str(SonessAgent.ModelOption), "value" -> Json.Str(model),
    )).unit

  private val promptTimeout = 3.minutes

  /** A prompt turn; one that doesn't answer in time is cancelled and fails, with the session's updates so far. */
  private def prompt(peer: Peer, sessionId: String, text: String): IO[String, Turn] =
    peer.call(
      "session/prompt",
      Json.Obj("sessionId" -> Json.Str(sessionId), "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(text)))),
      timeout = promptTimeout,
    ).map(Turn(_, _)).catchAll: error =>
      peer.send(Json.Obj(
        "jsonrpc" -> Json.Str("2.0"), "method" -> Json.Str("session/cancel"), "params" -> Json.Obj("sessionId" -> Json.Str(sessionId)),
      ).toJson).ignore *> ZIO.fail(Option(error.getMessage).getOrElse(error.toString).take(400))

  /** The checks every turn must pass: its stop reason, and metrics for the model it ran with. */
  private def turnChecks(turn: Turn, model: String): List[(Boolean, String)] =
    List(
      str(turn.result, "stopReason").contains("end_turn") -> s"stopReason ${str(turn.result, "stopReason")}",
      turn.metric("model").contains(Json.Str(model)) -> s"_meta model ${turn.metric("model").map(_.toJson)}",
      (turn.count("llmCalls") > 0) -> "no Bedrock calls in _meta",
      (turn.count("llmInputTokens") > 0 && turn.count("llmOutputTokens") > 0) -> "no Bedrock tokens in _meta",
      turn.text.nonEmpty -> "empty answer",
    )

  private def inferenceChecks(turn: Turn, model: String): List[(Boolean, String)] =
    turnChecks(turn, model) ++ List(
      turn.text.contains("42") -> s"answer has no 42: ${turn.text.take(200)}",
      turn.toolCalls.isEmpty -> s"unexpected tool calls ${turn.toolCalls}",
      turn.metric("ending").contains(Json.Str("answered")) -> s"ending ${turn.metric("ending").map(_.toJson)}",
    )

  private def toolChecks(withTools: Turn, model: String): List[(Boolean, String)] =
    val calls = withTools.toolCalls
    turnChecks(withTools, model) ++ List(
      (withTools.count("jevCalls") > 0) -> "Jev made no calls",
      withTools.metric("ending").contains(Json.Str("answered")) -> s"ending ${withTools.metric("ending").map(_.toJson)}",
      calls.exists(_._2 == "list_orders") -> s"no list_orders call (calls: ${calls.map(_._2)})",
      calls.forall((id, _) => withTools.finished.get(id).contains("completed")) ->
        s"tool calls not all completed: ${calls.map((id, name) => s"$name=${withTools.finished.get(id)}")}",
      (withTools.count("toolCalls") == calls.size.toLong) -> s"_meta toolCalls ${withTools.count("toolCalls")} vs ${calls.size} updates",
      customerOrders.forall(withTools.text.contains) ->
        s"answer lacks ${customerOrders.filterNot(withTools.text.contains)}: ${withTools.text.take(300)}",
    )

  /** A prompt no tool fits: answered, in text, with the turn's metrics. */
  private def greetingChecks(turn: Turn, model: String): List[(Boolean, String)] =
    turnChecks(turn, model) ++ List((turn.text.toLowerCase.contains("hello") || turn.text.toLowerCase.contains("hi")) ->
      s"greeting not returned: ${turn.text.take(200)}")

  /** The router session's question: answered through the skill, with the documented router command. */
  private def routedChecks(turn: Turn, model: String, project: String): List[(Boolean, String)] =
    val routed = turn.updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("tool_call"))
      .flatMap(u => field(u, "params", "update", "rawInput")).map(_.toJson)
    turnChecks(turn, model) ++ List(
      routed.exists(_.contains("get_project_info")) -> s"no get_project_info command (commands: $routed)",
      turn.finished.values.exists(_ == "completed") -> s"no routed call completed: ${turn.finished}",
      turn.text.contains(project) -> s"answer lacks the project name $project: ${turn.text.take(300)}",
    )

  /** "What tools do you have?": the session's own tools and skills, not ones the model imagines. */
  private def capabilityChecks(turn: Turn, model: String): List[(Boolean, String)] =
    val text = turn.text.toLowerCase
    turnChecks(turn, model) ++ List(
      // the router, or the tools it routes to (which a call to it lists)
      List("execute_tool", "get_project_info").exists(text.contains) -> s"answer doesn't name the session's tools: ${turn.text.take(300)}",
      !List("python", "browse", "calculator", "web search").exists(text.contains) -> s"answer invents tools: ${turn.text.take(300)}",
    )

  private def failed(checks: List[(Boolean, String)]): List[String] = checks.collect { case (false, message) => message }

  private def summary(turn: Either[String, Turn]): String =
    turn.fold(
      _ => "no answer",
      t => s"${t.count("latencyMs")} ms, ${t.toolCalls.size} tool calls, ${t.count("llmCalls")} llm/${t.count("jevCalls")} jev calls, " +
        t.metric("ending").flatMap(_.asString).getOrElse("?"),
    )

  private def run(peer: Peer, tools: String, plain: String, router: String, model: String, project: String): Task[Outcome] =
    for
      _         <- ZIO.foreachDiscard(List(tools, plain, router))(select(peer, _, model))
      inference <- prompt(peer, plain, inferencePrompt).either
      withTools <- prompt(peer, tools, toolPrompt).either
      greet     <- prompt(peer, tools, greeting).either
      routed    <- prompt(peer, router, greeting).either
      viaSkill  <- prompt(peer, router, projectQuestion).either
      abilities <- prompt(peer, router, capabilitiesQuestion).either
    yield
      def checked(name: String, turn: Either[String, Turn], checks: Turn => List[(Boolean, String)]) =
        turn.fold(e => List(e), t => failed(checks(t))).map(f => s"$name: $f")
      val failures =
        checked("inference", inference, inferenceChecks(_, model)) ++ checked("tools", withTools, toolChecks(_, model)) ++
          checked("greeting with tools", greet, greetingChecks(_, model)) ++
          checked("greeting with router-only IDE", routed, greetingChecks(_, model)) ++
          checked("router-only IDE through its skill", viaSkill, routedChecks(_, model, project)) ++
          checked("what tools do you have", abilities, capabilityChecks(_, model))
      Outcome(model, failures,
        f"$model%-22s inference: ${summary(inference)}%-36s tools: ${summary(withTools)}%-36s " +
          s"greeting: ${summary(greet)} | router-only: ${summary(routed)} | via skill: ${summary(viaSkill)} | abilities: ${summary(abilities)}")

  def spec = suite("live end to end")(
    test("every model: inference, tool calls Jev orchestrates over stdio and HTTP MCP servers, and prompts no tool fits") {
      for
        defaultModel <- System.env("BEDROCK_MODEL_ID").someOrFail(IllegalStateException("BEDROCK_MODEL_ID"))
        mcpUrl       <- OrdersMcpServer.serve
        peer         <- launch(Map.empty)
        lines        <- intellijCapture
        cwd          <- property("user.dir")
        newSession   <- intellijNewSession(lines(1), cwd, mcpUrl)
        // startup checks every model with a chat before it answers initialize
        _            <- peer.send(lines(0))
        (_, init)    <- peer.awaitResponse(1, 2.minutes)
        _            <- peer.send(newSession)
        (_, created) <- peer.awaitResponse(2)
        session       = created.get("result").getOrElse(Json.Null)
        tools         = str(session, "sessionId").getOrElse("")
        (_, bare)    <- peer.call("session/new", Json.Obj("cwd" -> Json.Str(cwd), "mcpServers" -> Json.Arr()))
        plain         = str(bare, "sessionId").getOrElse("")
        // as IntelliJ sends it by default: only the IDE's server, in router-only mode
        ideOnly       = lines(1).replace(""",{"type":"http","name":"orders","url":"${MCP_URL}","headers":[]}""", "")
                          .replace(""""id":2,""", """"id":90,""")
        skillRoot    <- SkillsSpec.tree(SkillsSpec.routerSkill*)
        routerLine   <- intellijNewSession(ideOnly, cwd, mcpUrl, routerOnly = true, skillRoot = Some(skillRoot))
        _            <- peer.send(routerLine)
        (_, routed)  <- peer.awaitResponse(90)
        router        = str(routed, "result", "sessionId").getOrElse("")
        models        = configOption(session, "model").toList.flatMap(values)
        project       = java.nio.file.Paths.get(cwd).getFileName.toString
        outcomes     <- ZIO.foreach(models)(model => run(peer, tools, plain, router, model, project))
        logged       <- peer.logged
        _            <- Console.printLine(
                          (s"== live end to end: models ${models.mkString(", ")}" :: outcomes.map(_.report) ++
                            logged.filter(_.contains("is unavailable")).map(line => s"   $line").toList).mkString("\n"),
                        )
        failures      = outcomes.flatMap(o => o.failures.map(f => s"${o.model}: $f"))
        _            <- Console.printLine(("failures:" :: failures.map("  " + _) ++ ("soness stderr (last 40 lines):" ::
                          logged.takeRight(40).toList.map(line => s"  ${line.take(400)}"))).mkString("\n")).when(failures.nonEmpty)
      yield assertTrue(
        str(init, "result", "agentInfo", "name").contains("soness"),
        created.get("error").isEmpty,
        tools.nonEmpty,
        plain.nonEmpty,
        router.nonEmpty,
        logged.exists(_.contains("2 MCP servers, 4 tools")),
        logged.exists(_.contains("1 MCP servers, 1 tools (and web_fetch), 1 skills")),
        configOption(session, "model").flatMap(o => str(o, "currentValue")).contains(defaultModel),
        failures == Nil,
      )
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK")
      @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID"),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(30.minutes)
