package com.jamesward.soness

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.orchestration.{AgentLoop, Interjection, Overseer, ParallelLoop, Sections, SkillChoice}
import zio.*

/**
 * The session's decision model (zio-decisions' `DecisionModel`, [[DecisionModels]]: Jev today) drives the session as the
 * orchestrator of an agent loop (zio-decisions' [[AgentLoop]]): at each step it chooses
 * one of the session's tools, offered with their schemas, or ends the loop with an answer or as unable. The Bedrock
 * model writes each chosen tool's arguments and the reply, and is never given tools. The session's skills are
 * documents Jev can read, progressively, to learn how to use the tools; what it reads goes to the Bedrock model too. A
 * session without tools or skills offers Jev only the endings.
 */
final class DecisionHarness(
  decisions: ZEnvironment[DecisionModel],
  llm: Writer,
  config: AgentLoop.Config = AgentLoop.Config(instructions = Prompts.Jev),
  search: Option[WebSearch] = None,
) extends Harness:

  def run(prompt: String, toolset: Toolset, documents: Seq[AgentLoop.Document] = Nil, context: Option[String] = None,
      steering: UIO[List[String]] = ZIO.succeed(Nil)): Task[HarnessResult] =
    runInProject(prompt, toolset, documents, context, steering, ZIO.succeed(Sections.empty))

  override def runInProject(prompt: String, toolset: Toolset, documents: Seq[AgentLoop.Document], context: Option[String],
      steering: UIO[List[String]], shared: UIO[Sections]): Task[HarnessResult] =
    val tools = toolset.tools.map: t =>
      // a tool that only shows the user something does no work for the request: said so, so it's chosen only when asked for
      val description = if t.name == "open_file_in_editor" then t.description + " It only shows the file to the user and reads " +
        "nothing for the agent: call it only when the user asks to see or open a file; to read a file, use the read tool." else t.description
      AgentLoop.Tool(t.name, description, t.inputSchema, t.outputSchema, t.annotations, t.schemasInChoice,
        t.group.map((name, about) => AgentLoop.ToolGroup(name, about)))
    // which skills the request needs: Jev chooses from their names and descriptions (the ones the project's instructions
    // name marked so), and only those are read, once, into the context of every decision and writing (cut to the budget
    // before the request and the steering are); the others stay offered to read, and what a loaded one links to is too
    val skills = documents.toList
    SkillChoice.choose(prompt, skills.map(d => SkillChoice.Offered(d.title, d.description, Skills.isRequired(d))), context)
      .provideEnvironment(decisions)
      .catchAll(e => ZIO.logWarning(s"choosing the skills to load failed: ${e.getMessage}; loading the ones the project names").as(
        skills.zipWithIndex.collect { case (d, i) if Skills.isRequired(d) => i }))
      .flatMap: chosen =>
        val (load, offered) = skills.zipWithIndex.partition((_, i) => chosen.contains(i))
        ZIO.foreachPar(load.map(_._1))(d => d.read.either.map(d -> _)).flatMap: read =>
          val sections = read.collect { case (d, Right(c)) =>
            Sections.of(s"The ${d.title} skill (loaded for this request)", c.text, DecisionHarness.SkillPriority, DecisionHarness.SkillFloor) }
            .foldLeft(Sections.empty)(_ ++ _)
          val linked = read.collect { case (_, Right(c)) => c.linked }.flatten
          ZIO.foreachDiscard(read.collect { case (d, Left(e)) => d -> e })((d, e) => ZIO.logWarning(s"skill ${d.title}: ${e.getMessage}")) *>
            ZIO.logInfo(s"skills loaded for this request: ${read.collect { case (d, Right(_)) => d.title }.mkString(", ")}" +
              s" (of ${skills.size})").unless(sections.isEmpty) *>
            runParallel(prompt, toolset, tools, offered.map(_._1) ++ linked, context,
              steering.zipWith(shared)((s, others) => sections ++ Harness.steering(s) ++ others))

  /** Jev's route for a message that arrives while tasks run ([[Interjection]]). */
  override def route(message: String, running: List[Interjection.Running], context: Option[String],
      processes: List[Interjection.Process]): Task[Interjection.Decision] =
    Interjection.route(message, running, context, processes).provideEnvironment(decisions)

  override def judge(situation: String, question: String, yes: String, no: String): Task[Double] =
    com.jamesward.zio_decisions.orchestration.Judge.yesNo(situation, question, yes, no).provideEnvironment(decisions)

  override def recall(prompt: String, earlier: List[com.jamesward.zio_decisions.orchestration.Recall.Earlier]): Task[List[Int]] =
    com.jamesward.zio_decisions.orchestration.Recall.recall(prompt, earlier).provideEnvironment(decisions)

  override def oversee(stalled: List[Overseer.Stalled]): Task[List[Overseer.Judgment]] =
    Overseer.judge(stalled).provideEnvironment(decisions)

  override def safety(calls: List[com.jamesward.zio_decisions.orchestration.Safety.Call]): Task[List[Double]] =
    com.jamesward.zio_decisions.orchestration.Safety.judge(calls).provideEnvironment(decisions)

  override def warm: UIO[Unit] = llm.warm

  override def webSearch: Option[WebSearch] = search

  /** The Bedrock model answers from the tree, and the tree follows its answer. */
  override def status(question: String, tree: String): Task[String] =
    llm.status(question, tree).map(answer => s"$answer\n\n$tree")

  /**
   * [[ParallelLoop]]: what can run at the same time does. A request made of independent parts is split and its parts
   * worked on at the same time; one that isn't is one loop. The toolset keeps their edits and builds from colliding
   * ([[SharedTools]], the session's, which the agent gives every turn).
   */
  private def runParallel(prompt: String, toolset: Toolset, tools: Seq[AgentLoop.Tool], documents: Seq[AgentLoop.Document],
      context: Option[String], live: UIO[Sections]): Task[HarnessResult] =
    ZIO.scoped:
      for
        reads   = SharedTools.reads(toolset.tools)
        report  <- ClientTools.currentPlan.get
        parallel = ParallelLoop.Config(loop = config, readOnly = t => reads(t.name), workTools = t => !DecisionHarness.ShowsUser(t.name) && !config.endsTurn(t),
                     progress = report, maxParallel = config.maxParallel)
        result <- ParallelLoop.auto(prompt, tools, toolset.call(_, _), llm, parallel, documents, context, live)
                    .provideEnvironment(decisions)
        _      <- ZIO.logInfo(s"parallel: ${result.plan.fold("no plan")(p => s"${p.parts.size} parts (${p.parts.map(_.name).mkString(", ")})")}; " +
                    result.parts.map((n, e, ms) => s"$n ${DecisionHarness.ending(e)} in ${ms}ms").mkString(", "))
      yield HarnessResult(
        result.reply,
        ModelUsage(
          llmCalls = result.llmCalls,
          llmInputTokens = result.llmInputTokens,
          llmOutputTokens = result.llmOutputTokens,
          jevCalls = result.decisions.size,
          jevInputTokens = result.decisionUsage.inputTokens.toLong,
          jevOutputTokens = result.decisionUsage.outputTokens.toLong,
          jevLatencyMs = result.decisionLatencyMs,
          llmLatencyMs = result.llmLatencyMs,
          toolLatencyMs = result.toolLatencyMs,
        ),
        DecisionHarness.ending(result.ending),
      )

object DecisionHarness:
  /**
   * Required skills' priority in the context, and how much of each is kept: below the project's own context (its
   * files, its AGENTS.md), which tells Jev where things are; a skill is how to write, which the writer gets in full.
   */
  val SkillPriority = 35
  val SkillFloor = 1500

  /** Tools that only show the user something (an IDE's editor), doing no work for a loop working on its own. */
  val ShowsUser: Set[String] = Set("open_file_in_editor", "watch_log")

  /** How a turn ended, as reported in its metrics. */
  def ending(ending: AgentLoop.Ending): String = ending match
    case AgentLoop.Ending.Answered        => "answered"
    case AgentLoop.Ending.Unable          => "unable"
    case AgentLoop.Ending.BudgetExhausted => "budget_exhausted"
