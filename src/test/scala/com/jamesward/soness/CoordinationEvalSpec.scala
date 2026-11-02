package com.jamesward.soness

import com.jamesward.soness.dev.GoalRun
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path, StandardOpenOption}

/**
 * Coordination of tasks working on the same file at once, end to end, as soness always coordinates: soness in this process (its
 * ACP agent driven in memory, as `soness-goal` does) with two sessions in one fresh copy of the shop project, each
 * given its own change of `Discounts.scala` at the same moment (SPRING20, SUMMER15). Both changes must be on disk,
 * neither undoing the other, the old codes still working, nothing else changed.
 *
 * Each sample's record (`target/evals/coordination.jsonl`, with `SONESS_EVAL_LABEL`): the checks, the pair's wall
 * time, both turns' decision-model and LLM calls and tokens, the edits refused as stale (the file had changed since
 * its loop read it), the work stopped by a notice, and the notices heard and judged (urgent or not): before and after a
 * coordination change, compare them by label.
 * `SONESS_EVAL_SAMPLES` (default 2). Paid, so it runs only with `SONESS_EVAL_COORDINATION=true`: needs a decision
 * model's key and a writer (Bedrock, or Kiro).
 */
object CoordinationEvalSpec extends ZIOSpecDefault:

  private val first = "Add a new discount code, SPRING20, that takes 20% off at checkout."
  private val second = "Add a new discount code, SUMMER15, that takes 15% off at checkout."

  /** Paid and about 15 minutes: only when asked for (`SONESS_EVAL_COORDINATION=true`), with a decision model's key. */
  private val keyed =
    if sys.env.get("SONESS_EVAL_COORDINATION").contains("true") && (sys.env.contains("TYPESAFE_API_KEY") || sys.env.contains("OPENAI_API_KEY"))
    then TestAspect.identity else TestAspect.ignore

  /** The notices' log lines of a run, counted: heard (`note`), judged, urgent, work stopped. */
  final case class Heard(noted: Int, judged: Int, urgent: Int, stopped: Int)

  private def counted(lines: Vector[String]) = Heard(
    lines.count(_.contains(" heard ")), lines.count(_.contains(" judged ")), lines.count(_.contains("(urgent)")),
    lines.count(_.contains(" stopped ")))

  /** A logger that keeps the notices' lines. */
  private def noticing(kept: java.util.concurrent.ConcurrentLinkedQueue[String]): ZLogger[String, Unit] = new ZLogger[String, Unit]:
    def apply(trace: Trace, fiberId: FiberId, logLevel: LogLevel, message: () => String, cause: Cause[Any], context: FiberRefs,
        spans: List[LogSpan], annotations: Map[String, String]): Unit =
      val m = message()
      if m.startsWith("notices: ") then kept.add(m)

  def spec = suite("coordination")(
    test("two sessions changing the same file at once: both changes land") {
      for
        samples <- ZIO.succeed(sys.env.get("SONESS_EVAL_SAMPLES").flatMap(_.toIntOption).getOrElse(2))
        results <- ZIO.foreach((1 to samples).toList)(n => sample("soness", n))
        _       <- Console.printLine(summary("soness", results.map(_._2)))
      yield assertTrue(results.nonEmpty)
    } @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(2.hours) @@ keyed,
  ).provideShared(Client.default)

  final case class Sample(passed: Boolean, checks: List[(String, Boolean)], wallMs: Long, decisions: Int, decisionTokens: Long,
      llmCalls: Int, llmTokens: Long, stale: Int, failed: Int, heard: Heard, endings: List[String] = Nil)

  private def summary(arm: String, samples: List[Sample]): String =
    def avg(f: Sample => Double) = if samples.isEmpty then 0.0 else samples.map(f).sum / samples.size
    f"$arm%-10s passed ${samples.count(_.passed)}/${samples.size}  wall ${avg(_.wallMs.toDouble) / 1000}%.0fs  " +
      f"decisions ${avg(_.decisions.toDouble)}%.0f (${avg(_.decisionTokens.toDouble) / 1000}%.0fk tokens)  llm ${avg(_.llmCalls.toDouble)}%.0f " +
      f"(${avg(_.llmTokens.toDouble) / 1000}%.0fk)  stale edits ${avg(_.stale.toDouble)}%.1f  failed calls ${avg(_.failed.toDouble)}%.1f  " +
      f"notices judged ${avg(_.heard.judged.toDouble)}%.1f urgent ${avg(_.heard.urgent.toDouble)}%.1f stopped ${avg(_.heard.stopped.toDouble)}%.1f"

  private def sample(arm: String, n: Int): ZIO[Client, Throwable, (String, Sample)] =
    ZIO.scoped:
      for
        kept     = java.util.concurrent.ConcurrentLinkedQueue[String]()
        // before the agent starts: its fibers log through it
        _       <- ZIO.withLoggerScoped(noticing(kept))
        root    <- SkillsSpec.tree(IntellijTaskEvalSpec.shopFixed*).map(_.toRealPath())
        catalog <- Soness.catalog.mapError(IllegalStateException(_))
        (agent, text) <- GoalRun.connect(catalog, allow = true)
        started <- Clock.instant
        both    <- GoalRun.attempt(agent, text, root, first).either.zipPar(GoalRun.attempt(agent, text, root, second).either)
        ended   <- Clock.instant
        lines    = Vector.from(kept.toArray.map(_.toString))
        events  <- EventLog.at(root).read
        source   = Files.readString(root.resolve("src/main/scala/shop/Discounts.scala"))
        turns    = List(both._1, both._2)
        checks   = List(
                     "both prompts answered" -> turns.forall(_.exists(_._3 == "answered")),
                     "both codes, and the old ones, work" -> IntellijTaskEvalSpec.codesWork(source,
                       Map("save10" -> 10, "welcome5" -> 5, "spring20" -> 20, "summer15" -> 15)),
                     "the other sources are unchanged" -> IntellijTaskEvalSpec.shopFixed.filterNot(_._1.endsWith("Discounts.scala"))
                       .filter(_._1.startsWith("src/main")).forall((f, t) => Files.readString(root.resolve(f)) == t),
                   )
        metrics  = turns.flatMap(_.toOption.flatMap(_._4))
        calls    = events.collect { case c: EventLog.Event.ToolCalled => c }
        s        = Sample(checks.forall(_._2), checks, java.time.Duration.between(started, ended).toMillis,
                     metrics.map(_.jevCalls).sum, metrics.map(m => m.jevInputTokens + m.jevOutputTokens).sum,
                     metrics.map(_.llmCalls).sum, metrics.map(m => m.llmInputTokens + m.llmOutputTokens).sum,
                     calls.count(c => !c.succeeded && c.result.contains("changed since you last read it")),
                     calls.count(!_.succeeded), counted(lines),
                     turns.map(_.fold(e => s"error: ${Option(e.getMessage).getOrElse(e.toString).take(200)}", _._3)))
        _       <- Console.printLine(s"${if s.passed then "PASS" else "FAIL"} $arm #$n in ${s.wallMs / 1000}s; " +
                     checks.map((k, ok) => s"${if ok then "✓" else "✗"} $k").mkString(", ") + s"; endings ${s.endings.mkString(", ")}; ${s.heard}; stale ${s.stale}")
        _       <- record(arm, s)
      yield arm -> s

  private def record(arm: String, s: Sample): Task[Unit] =
    val line = Json.Obj(
      "at" -> Json.Str(java.time.Instant.now.toString), "eval" -> Json.Str("coordination"), "arm" -> Json.Str(arm),
      "label" -> sys.env.get("SONESS_EVAL_LABEL").fold[Json](Json.Null)(Json.Str(_)), "passed" -> Json.Bool(s.passed),
      "checks" -> Json.Obj(Chunk.from(s.checks.map((k, ok) => k -> Json.Bool(ok)))), "wallMs" -> Json.Num(s.wallMs),
      "decisions" -> Json.Num(s.decisions), "decisionTokens" -> Json.Num(s.decisionTokens), "llmCalls" -> Json.Num(s.llmCalls),
      "llmTokens" -> Json.Num(s.llmTokens), "staleEdits" -> Json.Num(s.stale), "failedCalls" -> Json.Num(s.failed),
      "noted" -> Json.Num(s.heard.noted), "judged" -> Json.Num(s.heard.judged), "urgent" -> Json.Num(s.heard.urgent),
      "stopped" -> Json.Num(s.heard.stopped), "endings" -> Json.Arr(Chunk.from(s.endings.map(Json.Str(_))))).toJson
    ZIO.attemptBlocking:
      val dir = Path.of("target", "evals")
      Files.createDirectories(dir)
      Files.writeString(dir.resolve("coordination.jsonl"), line + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    .unit
