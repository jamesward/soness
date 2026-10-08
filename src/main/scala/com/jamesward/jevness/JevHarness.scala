package com.jamesward.jevness

import com.jamesward.zio_bedrock.Bedrock
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import com.jamesward.zio_typesafe_ai.orchestration.*
import com.jamesward.zio_typesafe_ai.orchestration.runtime.OperationInvoker
import zio.*
import zio.json.ast.Json

/**
 * Jev drives the session: at each runtime checkpoint it chooses the next tool, filter or the answer, while the host
 * binds arguments from typed tool outputs. The Bedrock LLM is only a tool Jev calls, to extract arguments it can't
 * bind, to write filter predicates beyond Jev's own classification capacity, and to write the final answer from the
 * retained evidence.
 */
final class JevHarness(
  jev: ZEnvironment[TypeSafeAI.Client],
  llm: LlmTransport,
  mode: OrchestrationMode = OrchestrationMode.NoPlan,
  config: OrchestrationConfig = JevHarness.config,
) extends Harness:

  def run(prompt: String, toolset: Toolset): Task[HarnessResult] =
    val catalog = toolset.tools.map(JevHarness.operation).toVector ++
      Vector(Catalog.extract, Catalog.synthesizeFilter, Catalog.summarize)
    val invoker = new OperationInvoker:
      def call(operation: String, arguments: Json.Obj): Task[Json] = toolset.call(operation, arguments)
    for
      internal <- InternalLlmInvoker.make(llm)
      result   <- GenericOrchestrator.runMode(prompt, catalog, invoker, internal, mode, config).provideEnvironment(jev)
    yield HarnessResult(result.finalText, JevHarness.usage(result.metrics))

object JevHarness:
  /**
   * The orchestrator's defaults, except that filters over up to 1024 distinct records are classified by Jev instead of
   * having an LLM read a sample of them to write a predicate.
   */
  val config: OrchestrationConfig = OrchestrationConfig(jevFilterVariantThreshold = 1024)

  private val anyObject = Json.Obj("type" -> Json.Str("object"))

  def operation(tool: ToolSpec): OperationSpec =
    OperationSpec(tool.name, tool.description, tool.inputSchema, tool.outputSchema.getOrElse(anyObject), CapabilityKind.Mcp)

  def usage(metrics: OrchestrationMetrics): ModelUsage =
    ModelUsage(
      llmCalls = metrics.extractCalls + metrics.filterCalls + metrics.summaryCalls,
      llmInputTokens = (metrics.extractInputTokens + metrics.filterInputTokens + metrics.summaryInputTokens).toLong,
      llmOutputTokens = (metrics.extractOutputTokens + metrics.filterOutputTokens + metrics.summaryOutputTokens).toLong,
      jevCalls = metrics.jevLogicalTurns + metrics.jevFilterCalls,
      jevInputTokens = (metrics.jevLogicalInputTokens + metrics.jevFilterInputTokens).toLong,
      jevOutputTokens = (metrics.jevLogicalOutputTokens + metrics.jevFilterOutputTokens).toLong,
    )
