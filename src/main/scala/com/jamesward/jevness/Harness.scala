package com.jamesward.jevness

import zio.*
import zio.json.ast.Json

/** One tool a harness can call, as an MCP server described it. */
final case class ToolSpec(name: String, description: String, inputSchema: Json.Obj, outputSchema: Option[Json.Obj])

/** The tools of one session. `call` returns the tool's structured result and fails when the tool reports an error. */
trait Toolset:
  def tools: Chunk[ToolSpec]
  def call(name: String, arguments: Json.Obj): Task[Json]

/** Model usage of one prompt turn. Generative-LLM (Bedrock) and Jev usage are kept apart: they are different models. */
final case class ModelUsage(
  llmCalls: Int = 0,
  llmInputTokens: Long = 0,
  llmOutputTokens: Long = 0,
  jevCalls: Int = 0,
  jevInputTokens: Long = 0,
  jevOutputTokens: Long = 0,
) derives CanEqual:
  def inputTokens: Long = llmInputTokens + jevInputTokens
  def outputTokens: Long = llmOutputTokens + jevOutputTokens

final case class HarnessResult(answer: String, usage: ModelUsage)

/** Answers one prompt with the session's tools. */
trait Harness:
  def run(prompt: String, toolset: Toolset): Task[HarnessResult]

/** The harnesses a session can switch between with ACP session modes. */
enum HarnessMode(val id: String, val title: String, val description: String) derives CanEqual:
  case Jev extends HarnessMode("jev", "Jev", "Jev orchestrates the tools; a Bedrock LLM only extracts arguments and writes the answer.")
  case Llm extends HarnessMode("llm", "LLM", "Baseline: a Bedrock LLM tool loop drives every step.")

object HarnessMode:
  def fromId(id: String): Option[HarnessMode] = values.find(_.id == id)
