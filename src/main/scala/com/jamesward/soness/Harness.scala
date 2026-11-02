package com.jamesward.soness

import com.jamesward.zio_decisions.Decisions
import com.jamesward.zio_decisions.orchestration.{Interjection, Overseer, Sections}
import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import zio.*
import zio.json.ast.Json

/**
 * One tool a harness can call, as an MCP server described it. `schemasInChoice` false offers it to Jev by name,
 * description and annotations only (a large toolset's schemas overflow Jev's choice); its arguments are still written
 * against `inputSchema`.
 */
final case class ToolSpec(
  name: String,
  description: String,
  inputSchema: Json.Obj,
  outputSchema: Option[Json.Obj],
  annotations: Option[Json.Obj] = None,
  schemasInChoice: Boolean = true,
  /** The group it's offered in when the session has many tools (its name and description): see [[ToolResolver]]. */
  group: Option[(String, String)] = None,
)

/** The tools of one session. `call` returns the tool's structured result and fails when the tool reports an error. */
trait Toolset:
  def tools: Chunk[ToolSpec]
  def call(name: String, arguments: Json.Obj): Task[Json]

object Toolset:
  val empty: Toolset = new Toolset:
    val tools: Chunk[ToolSpec] = Chunk.empty
    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.fail(IllegalArgumentException(s"unknown tool: $name"))

/** Model usage of one prompt turn. Generative-LLM (Bedrock) and Jev usage are kept apart: they are different models. */
final case class ModelUsage(
  llmCalls: Int = 0,
  llmInputTokens: Long = 0,
  llmOutputTokens: Long = 0,
  jevCalls: Int = 0,
  jevInputTokens: Long = 0,
  jevOutputTokens: Long = 0,
  jevLatencyMs: Long = 0,
  llmLatencyMs: Long = 0,
  toolLatencyMs: Long = 0,
) derives CanEqual:
  def inputTokens: Long = llmInputTokens + jevInputTokens
  def outputTokens: Long = llmOutputTokens + jevOutputTokens

/** A turn's reply, its model usage, and how it ended (`answered`, `unable`, `budget_exhausted`). */
final case class HarnessResult(answer: String, usage: ModelUsage, ending: String = "answered")

/**
 * Answers one prompt with the session's tools, the documents (skills) it can read, and what's known about the project
 * the session is in (`context`, [[ProjectContext]]). What can run in parallel does: a request made of independent parts
 * is split and its parts worked on at the same time (zio-decisions' `ParallelLoop`); a request that isn't is one loop.
 */
trait Harness:
  /** `steering` is read again as the work goes on: the user's messages about this task since it started. */
  def run(prompt: String, toolset: Toolset, documents: Seq[Document] = Nil, context: Option[String] = None,
      steering: UIO[List[String]] = ZIO.succeed(Nil)): Task[HarnessResult]

  /**
   * [[run]], with `shared` too: what the project's other sessions are doing ([[Workspace.view]]), read again as the work
   * goes on, for the context of every decision and writing; by default left out.
   */
  def runInProject(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
      steering: UIO[List[String]], shared: UIO[com.jamesward.zio_decisions.orchestration.Sections]): Task[HarnessResult] =
    run(prompt, toolset, documents, context, steering)

  /** What a message that arrives while tasks run is: by default, a new task. */
  def route(message: String, running: List[Interjection.Running], context: Option[String],
      processes: List[Interjection.Process] = Nil): Task[Interjection.Decision] =
    ZIO.succeed(Interjection.Decision(Interjection.Route.NewTask, Map.empty, 1.0, Decisions.Usage(0, 0), 0))

  /** Jev's probability that the answer to `question` about `situation` is yes: by default, yes. */
  def judge(situation: String, question: String, yes: String, no: String): Task[Double] = ZIO.succeed(1.0)

  /** The indices of `earlier` (the project's earlier tasks) that bear on `prompt`: by default, none. */
  def recall(prompt: String, earlier: List[com.jamesward.zio_decisions.orchestration.Recall.Earlier]): Task[List[Int]] =
    ZIO.succeed(Nil)

  /** Jev's verdicts on stalled tool calls (keep waiting, or stop): by default, wait for each. */
  def oversee(stalled: List[Overseer.Stalled]): Task[List[Overseer.Judgment]] = Watchdog.waitAlways(stalled)

  /**
   * How likely each call is safe to make without asking the user ([[Guard]], for the calls its rules don't settle): by
   * default, safe (a harness without a decision model has nothing to judge with).
   */
  def safety(calls: List[com.jamesward.zio_decisions.orchestration.Safety.Call]): Task[List[Double]] = ZIO.succeed(calls.map(_ => 1.0))

  /** Gets the harness's writer ready (an ACP agent started), so a session's first turn doesn't wait for it. */
  def warm: UIO[Unit] = ZIO.unit

  /** The web search its writer's provider gives (TOOLS_SPEC.md): Kiro's when Kiro writes; by default, none. */
  def webSearch: Option[WebSearch] = None

  /** The answer to a question about the work in progress, from the process tree as text: by default, the tree. */
  def status(question: String, tree: String): Task[String] = ZIO.succeed(tree)

object Harness:
  /**
   * The user's steering of a task, as a context section for its loop: the highest priority, never cut (the user's
   * words), or no section.
   */
  def steering(messages: List[String]): Sections =
    Sections.of(
      "The user added, while this request was being worked on (follow it; where it conflicts with the request, it wins)",
      messages.map(m => s"- $m").mkString("\n"),
      SteeringPriority,
      Int.MaxValue,
    )

  val SteeringPriority = 90

/**
 * What a session can choose from, each a config option of its own: the decision model (Jev, OpenAI's), the model that
 * writes (a Bedrock model, an ACP agent) and the web search (Kiro's, Bedrock's, or none); the harness of a selection of
 * the three, and what a new session starts with. A catalog made of harnesses by model alone (the tests' stubs) has no
 * decision or search choices: the model picks its harness.
 */
final case class HarnessCatalog private (models: List[String], defaultModel: String, private val harnesses: Map[String, Harness],
    decisions: List[HarnessCatalog.Choice] = Nil, searches: List[HarnessCatalog.Choice] = Nil,
    default: HarnessCatalog.Selection, unavailableModels: List[HarnessCatalog.Choice] = Nil,
    private val compose: Option[HarnessCatalog.Selection => Option[Harness]] = None):
  /** The model's harness, with the default decision model and search. */
  def harness(model: String): Option[Harness] = harness(default.copy(model = model))

  /** The harness of a selection: its decision model, its writer and its search. */
  def harness(selection: HarnessCatalog.Selection): Option[Harness] =
    compose.fold(harnesses.get(selection.model))(_(selection))

  /** Whether the session can choose its decision model and search (a catalog of the real providers). */
  def composed: Boolean = compose.isDefined

object HarnessCatalog:
  /**
   * One value a session can choose: its id, its name and description, and the provider it's grouped under; one
   * `unavailable` (its credentials or tool missing) is shown, saying why, and can't be chosen.
   */
  final case class Choice(id: String, name: String, description: String, group: String, unavailable: Option[String] = None)
      derives CanEqual:
    def available: Boolean = unavailable.isEmpty

  /** A session's choice: its decision model (none in a catalog of stubs), its writer, its web search (none: no search). */
  final case class Selection(decision: Option[String], model: String, search: Option[String]) derives CanEqual

  /** The models, in order, with their harnesses; `defaultModel` must be among them. */
  def make(models: List[(String, Harness)], defaultModel: String): Either[String, HarnessCatalog] =
    if models.isEmpty then Left("no model is available")
    else if !models.exists(_._1 == defaultModel) then Left(s"model $defaultModel is unavailable")
    else Right(HarnessCatalog(models.map(_._1), defaultModel, models.toMap, default = Selection(None, defaultModel, None)))

  /**
   * The real providers' catalog: `harness` composes a selection's decision model, writer and search; `default` is
   * what a new session starts with.
   */
  def composed(models: List[String], decisions: List[Choice], searches: List[Choice], default: Selection,
      unavailableModels: List[Choice] = Nil)(harness: Selection => Option[Harness]): Either[String, HarnessCatalog] =
    if models.isEmpty then Left("no model is available")
    else if !decisions.exists(_.available) then Left("no decision model is available")
    else Right(HarnessCatalog(models, default.model, Map.empty, decisions, searches, default, unavailableModels, Some(harness)))
