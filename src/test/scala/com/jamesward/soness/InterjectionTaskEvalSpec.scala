package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.str
import com.jamesward.soness.IntellijSession.*
import java.nio.file.{Files, Path}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * A user who sends a second message while soness works on the first, end to end: soness launched as IntelliJ IDEA
 * launches it, the mock IDE MCP server, the shop project of [[IntellijTaskEvalSpec]], the bug report as the first
 * prompt, and the second sent after the first's first tool call. Each case checks how Jev routed it and what's on disk
 * and in the answers afterwards. `SONESS_EVAL_SAMPLES` (default 1) per case; records in
 * `target/evals/interjection-tasks.jsonl`. Needs `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID`.
 */
object InterjectionTaskEvalSpec extends ZIOSpecDefault:

  private val bug = IntellijTaskEvalSpec.tasks.head

  private def discounts(root: Path) = Files.readString(root.resolve("src/main/scala/shop/Discounts.scala"))

  private final case class Case(name: String, second: String, route: String, checks: (Path, Interjected) => List[(String, Boolean)])

  private val cases = List(
    Case(
      "steer: a constraint on the fix",
      "Don't change the keys of the codes map; fix it in lookup, and put a comment above lookup saying the codes are stored in lower case.",
      "steer:1",
      (root, r) => List(
        "the codes are fixed" -> IntellijTaskEvalSpec.discountsFixed(discounts(root)),
        "the map's keys are unchanged" -> (discounts(root).contains("\"save10\" ->") && discounts(root).contains("\"welcome5\" ->")),
        // what only the steering asked for
        "a comment above lookup says the codes are lower case" ->
          """(?is)//[^\n]*lower[- ]?case[^\n]*\n\s*def lookup|/\*.*lower[- ]?case.*\*/\s*def lookup""".r.findFirstIn(discounts(root)).isDefined,
        "the steering was acknowledged" -> r.answers.exists(_.startsWith("Got it")),
      ),
    ),
    Case(
      "new task: an unrelated question",
      "Separately: when does a customer get free shipping at checkout?",
      "new",
      (root, r) => List(
        "the codes are fixed" -> IntellijTaskEvalSpec.discountsFixed(discounts(root)),
        "an answer gives the free-shipping threshold" -> r.answers.exists(a => """\b50\b""".r.findFirstIn(a).isDefined),
      ),
    ),
    Case(
      "follow-up: after the fix",
      "Once that's fixed, add a SPRING20 code that takes 20% off too.",
      "after:1",
      (root, r) => List(
        "the codes are fixed and SPRING20 works" ->
          IntellijTaskEvalSpec.codesWork(discounts(root), Map("save10" -> 10, "welcome5" -> 5, "spring20" -> 20)),
      ),
    ),
  )

  def spec = suite("interjections, end to end")(
    test("a second message while the first runs is steered, started or queued, and done") {
      ZIO.scoped:
        for
          samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
          label   <- System.env("SONESS_EVAL_LABEL").map(_.getOrElse(""))
          ide      = Ide(None, None, None)
          // `SONESS_EVAL_LOG`: a debug log of the run
          log     <- System.env("SONESS_EVAL_LOG")
          driver  <- IntellijSession.driver(ide, log)
          results <- ZIO.foreach(for c <- cases; n <- 1 to samples yield (c, n)): (c, n) =>
                       for
                         root    <- SkillsSpec.tree(bug.project*).map(_.toRealPath())
                         started <- Clock.instant
                         r       <- driver.interjected(bug.prompt, c.second, root, afterCalls = 1)
                         ended   <- Clock.instant
                         checks   = ("routed as " + c.route -> r.route.contains(c.route)) ::
                                      ("both prompts answered" -> (r.first.exists(_.get("result").isDefined) && r.second.exists(_.get("result").isDefined))) ::
                                      c.checks(root, r)
                         ms       = java.time.Duration.between(started, ended).toMillis
                         _       <- Console.printLine(
                                      s"${if checks.forall(_._2) then "PASS" else "FAIL"} ${c.name} [#$n] in ${ms}ms; routed ${r.route.getOrElse("?")}; " +
                                        s"calls ${r.calls.mkString(", ")}\n" + checks.map((k, ok) => s"     ${if ok then "✓" else "✗"} $k").mkString("\n") +
                                        r.answers.map(a => s"\n     answer: ${a.take(300).replace("\n", " ")}").mkString)
                         _       <- ZIO.attempt:
                                      val file = Path.of("target/evals/interjection-tasks.jsonl")
                                      Files.createDirectories(file.getParent)
                                      val line = Json.Obj("label" -> Json.Str(label), "case" -> Json.Str(c.name), "route" -> Json.Str(r.route.getOrElse("")),
                                        "passed" -> Json.Bool(checks.forall(_._2)), "latencyMs" -> Json.Num(ms), "toolCalls" -> Json.Num(r.calls.size),
                                        "checks" -> Json.Obj(checks.map((k, ok) => k -> (Json.Bool(ok): Json))*))
                                      Files.writeString(file, line.toJson + "\n", java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
                       yield checks.forall(_._2)
          _       <- Console.printLine(s"interjections: ${results.count(identity)}/${results.size} passed")
        yield assertTrue(results.count(identity) * 3 >= results.size * 2)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK") @@ TestAspect.withLiveClock
      @@ TestAspect.withLiveSystem @@ TestAspect.timeout(40.minutes),
  )
