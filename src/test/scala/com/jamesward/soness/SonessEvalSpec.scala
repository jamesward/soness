package com.jamesward.soness

import com.jamesward.soness.OrdersMcpServer.*
import com.jamesward.zio_evals.*
import com.jamesward.zio_decisions.typesafe.TypeSafe
import zio.*
import zio.http.Client
import zio.json.*
import zio.test.*

/**
 * Live evals: each task runs through the soness ACP agent with each Bedrock model it offers, against the orders MCP
 * server, is graded by Jev (`JevJudge`) and deterministic checks, and reported with its latency and token use (Jev and
 * Bedrock apart). Every model must answer correctly.
 *
 * They need `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` (see the README) and are skipped
 * without them. `SONESS_EVAL_SAMPLES` sets the samples per model (default 3); metrics are averaged over them.
 */
object SonessEvalSpec extends ZIOSpecDefault:

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

  def spec = suite("soness evals")(
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
          samples   <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
          url       <- OrdersMcpServer.serve
          http      <- ZIO.environment[Client]
          catalog   <- Soness.catalog.mapError(RuntimeException(_))
          judge     <- JevJudge.make()
          loop       = SonessAgentLoop(client => ZIO.scope.flatMap(parent => SonessAgent.make(client, http, catalog, parent)))
          arm        = EvalArm.mcp("orders", "Orders MCP server", List(McpServerConfig("orders", url)))
          results   <- EvalRunner.run(eval, List(arm), catalog.models, samples, loop, judge)
          _         <- Console.printLine(report(name, samples, results))
        yield assertTrue(
          results.map(_.modelId).sorted == catalog.models.sorted,
          results.forall(_.checksPassed),
          results.forall(_.verdict == EvalVerdict.Pass),
        )
    }.provide(Soness.httpClient, TypeSafe.live)

  private def tokens(metrics: ArmMetrics): Double = metrics.inputTokens + metrics.outputTokens

  /** Per-model averages, with the Bedrock/Jev token split from each sample's turn metrics. */
  private def report(name: String, samples: Int, results: List[ArmResult]): String =
    val header = f"${"model"}%-24s ${"verdict"}%-7s ${"checks"}%-6s ${"latencyMs"}%10s ${"tokens"}%9s ${"llmTokens"}%10s ${"jevTokens"}%10s ${"toolCalls"}%9s ${"modelCalls"}%10s"
    val rows = results.map: r =>
      val turns = r.samples.flatMap(_.events.collect { case TranscriptEvent.Note(text) => text.fromJson[TurnMetrics].toOption }.flatten)
      def avg(f: TurnMetrics => Long): Double = if turns.isEmpty then 0 else turns.map(f).sum.toDouble / turns.size
      f"${r.modelId}%-24s ${r.verdict.toString}%-7s ${r.checksPassed.toString}%-6s ${r.metrics.latencyMs}%10.0f ${tokens(r.metrics)}%9.0f " +
        f"${avg(t => t.llmInputTokens + t.llmOutputTokens)}%10.0f ${avg(t => t.jevInputTokens + t.jevOutputTokens)}%10.0f " +
        f"${r.metrics.toolCalls}%9.1f ${r.metrics.iterations}%10.1f"
    (s"== $name ($samples samples per model)" :: header :: rows).mkString("\n")
