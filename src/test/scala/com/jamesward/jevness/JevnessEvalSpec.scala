package com.jamesward.jevness

import com.jamesward.jevness.OrdersMcpServer.*
import com.jamesward.zio_evals.*
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import zio.*
import zio.http.Client
import zio.json.*
import zio.test.*

/**
 * Live evals: each task runs through the jevness ACP agent in both session modes (`jev`, then the `llm` baseline)
 * against the orders MCP server, is graded by Jev (`JevJudge`) and deterministic checks, and then compared. Both modes
 * must answer correctly, and the jev mode must be faster and use fewer tokens (Jev plus LLM) than the baseline.
 *
 * They need `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` (see the README) and are skipped
 * without them. `JEVNESS_EVAL_SAMPLES` sets the samples per mode (default 3); metrics are averaged over them.
 */
object JevnessEvalSpec extends ZIOSpecDefault:

  private val platinumEmea = customers.filter(c => c.tier == "Platinum" && c.region == "EMEA")

  private val placedOrders =
    orders.filter(o => o.status == "placed" && platinumEmea.exists(_.customerId == o.customerId))

  val filterTask: EvalSpec = EvalSpec(
    task = "Which Platinum-tier customers are in the EMEA region? Give each one's customer ID and name.",
    criteria = "The answer lists exactly these customers and no others: " +
      platinumEmea.map(c => s"${c.customerId} (${c.name})").mkString(", ") + ".",
    checks = List(EvalCheck.AnswerContainsAll(platinumEmea.map(_.customerId)), EvalCheck.ToolCalled("list_customers")),
  )

  val fanOutTask: EvalSpec = EvalSpec(
    task = "Which orders of our Platinum-tier EMEA customers are still in 'placed' status? Give each order ID and its customer ID.",
    criteria = "The answer lists exactly these placed orders and no others: " +
      placedOrders.map(o => s"${o.orderId} (customer ${o.customerId})").mkString(", ") + ".",
    checks = List(EvalCheck.AnswerContainsAll(placedOrders.map(_.orderId)), EvalCheck.ToolCalled("list_orders")),
  )

  private val modes = List(HarnessMode.Jev, HarnessMode.Llm)

  def spec = suite("jevness vs a pure LLM harness")(
    test("fixture: the tasks have small, non-empty answers") {
      assertTrue(platinumEmea.size >= 3, platinumEmea.size <= 12, placedOrders.nonEmpty, placedOrders.size <= 12)
    },
    suite("live")(
      compare("filter a wide customer list", filterTask),
      compare("filter, then fan out to each match's orders", fanOutTask),
    ) @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK")
      @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID") @@ TestAspect.sequential @@ TestAspect.timeout(30.minutes)
      @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem,
  )

  private def compare(name: String, eval: EvalSpec) =
    test(name) {
      ZIO.scoped:
        for
          samples   <- System.env("JEVNESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
          url       <- OrdersMcpServer.serve
          http      <- ZIO.environment[Client]
          harnesses <- Jevness.harnesses
          _         <- ZIO.fail(RuntimeException(s"unavailable: ${harnesses.unavailable}")).when(harnesses.unavailable.nonEmpty)
          judge     <- JevJudge.make()
          loop       = AcpAgentLoop(client => JevnessAgent.make(client, http, harnesses.available, HarnessMode.Jev))
          arm        = EvalArm.mcp("orders", "Orders MCP server", List(McpServerConfig("orders", url)))
          results   <- EvalRunner.run(eval, List(arm), modes.map(_.id), samples, loop, judge)
          byMode     = results.map(r => r.modelId -> r).toMap
          _         <- Console.printLine(report(name, samples, results))
          jev        = byMode(HarnessMode.Jev.id)
          llm        = byMode(HarnessMode.Llm.id)
        yield assertTrue(
          jev.checksPassed,
          jev.verdict == EvalVerdict.Pass,
          llm.checksPassed,
          llm.verdict == EvalVerdict.Pass,
          jev.metrics.latencyMs < llm.metrics.latencyMs,
          tokens(jev.metrics) < tokens(llm.metrics),
        )
    }.provide(Client.default, TypeSafeAI.Client.live)

  private def tokens(metrics: ArmMetrics): Double = metrics.inputTokens + metrics.outputTokens

  /** Per-mode averages, with the LLM/Jev token split from each sample's turn metrics. */
  private def report(name: String, samples: Int, results: List[ArmResult]): String =
    val header = f"${"mode"}%-5s ${"verdict"}%-7s ${"checks"}%-6s ${"latencyMs"}%10s ${"tokens"}%9s ${"llmTokens"}%10s ${"jevTokens"}%10s ${"toolCalls"}%9s ${"modelCalls"}%10s"
    val rows = results.map: r =>
      val turns = r.samples.flatMap(_.events.collect { case TranscriptEvent.Note(text) => text.fromJson[TurnMetrics].toOption }.flatten)
      def avg(f: TurnMetrics => Long): Double = if turns.isEmpty then 0 else turns.map(f).sum.toDouble / turns.size
      f"${r.modelId}%-5s ${r.verdict.toString}%-7s ${r.checksPassed.toString}%-6s ${r.metrics.latencyMs}%10.0f ${tokens(r.metrics)}%9.0f " +
        f"${avg(t => t.llmInputTokens + t.llmOutputTokens)}%10.0f ${avg(t => t.jevInputTokens + t.jevOutputTokens)}%10.0f " +
        f"${r.metrics.toolCalls}%9.1f ${r.metrics.iterations}%10.1f"
    (s"== $name ($samples samples per mode)" :: header :: rows).mkString("\n")
