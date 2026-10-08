package com.jamesward.jevness

import com.jamesward.zio_evals.*
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

/** The ACP surface, with stub harnesses that call the orders MCP server: no model is involved. */
object JevnessAgentSpec extends ZIOSpecDefault:

  /** Calls `calls` in order and answers with their results. */
  private final case class Scripted(mode: HarnessMode, calls: List[(String, Json.Obj)]) extends Harness:
    def run(prompt: String, toolset: Toolset): Task[HarnessResult] =
      ZIO.foreach(calls)((name, arguments) => toolset.call(name, arguments).either)
        .map(results => HarnessResult(s"${mode.id}: ${results.map(_.fold(_ => "failed", _ => "ok")).mkString(",")}", ModelUsage(1, 10, 2, 3, 30, 3)))

  private val harnesses: Map[HarnessMode, Harness] = Map(
    HarnessMode.Jev -> Scripted(HarnessMode.Jev, List(
      "list_orders" -> Json.Obj("customerId" -> Json.Str("C0046")),
      "get_order" -> Json.Obj("orderId" -> Json.Str("nope")),
    )),
    HarnessMode.Llm -> Scripted(HarnessMode.Llm, List("list_customers" -> Json.Obj())),
  )

  private def loop(http: ZEnvironment[Client]) =
    AcpAgentLoop(client => JevnessAgent.make(client, http, harnesses, HarnessMode.Jev))

  def spec = suite("JevnessAgent")(
    test("reports tool calls, the answer and the turn metrics") {
      for
        url    <- OrdersMcpServer.serve
        http   <- ZIO.environment[Client]
        result <- loop(http).run("go", "jev", List(McpServerConfig("orders", url)), AgentPolicy.default)
        passed <- Checks.evaluateAll(List(EvalCheck.ToolCalled("list_orders"), EvalCheck.ToolInputContains("get_order", "nope")), result)
      yield assertTrue(
        result.answer == "jev: ok,failed",
        result.toolCalls == 2,
        result.iterations == 4,
        result.inputTokens == 40L,
        result.outputTokens == 5L,
        passed,
        result.events.collect { case TranscriptEvent.ToolResult(name, text, failed) => (name, failed, text.contains("O10460")) } ==
          List(("list_orders", false, true), ("get_order", true, false)),
      )
    },
    test("switches harness with the session mode") {
      for
        url    <- OrdersMcpServer.serve
        http   <- ZIO.environment[Client]
        result <- loop(http).run("go", "llm", List(McpServerConfig("orders", url)), AgentPolicy.default)
      yield assertTrue(result.answer == "llm: ok", result.capturedToolCalls.map(_.name) == List("list_customers"))
    },
    test("rejects an unknown mode and a stdio MCP server") {
      for
        http    <- ZIO.environment[Client]
        unknown <- loop(http).run("go", "nope", Nil, AgentPolicy.default).flip
        pipe    <- Transport.pipe
        _       <- AcpAgent.serve(pipe._1)(client => JevnessAgent.make(client, http, harnesses, HarnessMode.Jev)).forkScoped
        agent   <- AcpClient.connect(pipe._2, new com.jamesward.zioacp.Client:
                     def sessionUpdate(notification: SessionNotification): UIO[Unit] = ZIO.unit
                     def requestPermission(request: RequestPermissionRequest) =
                       ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))
                   )
        init    <- agent.initialize(InitializeRequest(ProtocolVersion.V1))
        stdio   <- agent.newSession(NewSessionRequest("/", List(McpServer.Stdio("x", "x", Nil, Nil)))).flip
        session <- agent.newSession(NewSessionRequest("/", Nil))
      yield assertTrue(
        unknown.getMessage.contains("unknown mode"),
        init.agentCapabilities.mcpCapabilities.http,
        stdio.message.contains("only HTTP MCP servers"),
        session.modes.map(_.availableModes.map(_.id.value)).contains(List("jev", "llm")),
        session.modes.map(_.currentModeId.value).contains("jev"),
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
