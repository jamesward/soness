package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ParallelLoop
import com.jamesward.zio_decisions.orchestration.ParallelLoop.{Plan, PlanRequest}
import zio.*
import zio.http.Client
import zio.test.*

import java.nio.file.{Files, Path}

/**
 * The planner of parallel mode (`BedrockLlmTransport.plan`), live: requests from the parallel evals, with what the
 * gathering would have read, each planned `SONESS_EVAL_SAMPLES` times (default 3) by the Bedrock model, and checked
 * for the parts it should have (their number, named for what they are) and the ones it shouldn't (a single bug is one
 * part). Needs only Bedrock (`AWS_BEARER_TOKEN_BEDROCK`, `BEDROCK_MODEL_ID`): no Jev. Seconds, not an eval run.
 */
object PlannerEvalSpec extends ZIOSpecDefault:

  private def resource(path: String): Task[String] =
    AcpSubprocess.property("user.dir").flatMap(d => ZIO.attemptBlocking(Files.readString(Path.of(d, "src/test/resources/eval-projects", path))))

  /** A case: the request, what was gathered for it, and what its plan must be like. */
  final case class Case(name: String, prompt: String, gathered: Task[String], check: Plan => List[(String, Boolean)])

  private def named(plan: Plan, words: List[String]): Boolean =
    words.forall(w => plan.parts.exists(p => p.name.toLowerCase.replaceAll("[^a-z0-9]", "").contains(w.toLowerCase)))

  private val cases = List(
    Case(
      "katas",
      "Implement the six katas in Katas.java, as README.md describes them, and run each one's test to show it passes.",
      resource("katas/README.md").map(r => s"read_file README.md:\n$r"),
      plan => List(
        "six parts" -> (plan.parts.size == 6),
        "named for the katas" -> named(plan, List("roman", "runlength", "balanced", "wordfrequenc", "luhn", "spiral")),
        "each part's task names its test" -> plan.parts.forall(_.task.contains("KatasTest")),
      ),
    ),
    Case(
      "easyracer 1, 2, 4",
      "Implement EasyRacer scenarios 1, 2, 4 in Java 25: https://github.com/jamesward/easyracer\n\nThis project is the start of " +
        "the client: each of those scenarios is a stub method of Main.Scenarios, and MainTest has a test for each.",
      resource("easyracer-java/src/main/java/Main.java").map(m => s"read_file src/main/java/Main.java:\n$m"),
      plan => List(
        "three parts" -> (plan.parts.size == 3),
        "named for the scenarios" -> named(plan, List("1", "2", "4")),
        "each part's task names its test" -> List(1, 2, 4).forall(n => plan.parts.exists(_.task.contains(s"scenario$n"))),
      ),
    ),
    Case(
      "one bug",
      "Customers are telling us the SAVE10 code doesn't do anything at checkout, they still pay full price. Can you find out why and fix it?",
      ZIO.succeed("read_file src/main/scala/shop/Discounts.scala:\n" + IntellijTaskEvalSpec.shop.toMap.apply("src/main/scala/shop/Discounts.scala")),
      plan => List("one part" -> (plan.parts.size <= 1)),
    ),
  )

  def spec = suite("the parallel planner")(
    test("plans the eval requests' parts, given what was gathered") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
        model   <- System.env("BEDROCK_MODEL_ID").someOrFail(IllegalStateException("BEDROCK_MODEL_ID is not set"))
        access  <- Soness.bedrockAccess.mapError(IllegalStateException(_))
        bedrock <- Soness.connect(access, model).mapError(IllegalStateException(_))
        llm      = BedrockLlmTransport(bedrock)
        results <- ZIO.foreach(cases): c =>
                     c.gathered.flatMap: gathered =>
                       ZIO.foreachPar((1 to samples).toList): _ =>
                         llm.plan(PlanRequest(c.prompt, None, Nil, Some(gathered))).map(_.value).map(p => ParallelLoop.normalize(p, 12)).either
                       .withParallelism(3).map(c -> _)
        lines    = results.flatMap: (c, plans) =>
                     plans.zipWithIndex.map: (p, i) =>
                       p match
                         case Left(e) => (false, s"FAIL ${c.name} #${i + 1}: no plan: ${e.getMessage.take(200)}")
                         case Right(plan) =>
                           val checks = c.check(plan)
                           (checks.forall(_._2), s"${if checks.forall(_._2) then "PASS" else "FAIL"} ${c.name} #${i + 1}: " +
                             s"${plan.parts.map(_.name).mkString(" | ")}; setup: ${plan.shared.take(80)}" +
                             checks.filterNot(_._2).map(x => s"\n     ✗ ${x._1}").mkString)
        _       <- ZIO.foreachDiscard(lines)(l => Console.printLine(l._2))
        passed   = lines.count(_._1)
        _       <- Console.printLine(s"planner: $passed/${lines.size}")
      yield assertTrue(passed * 10 >= lines.size * 8)
    } @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK") @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID"),
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes)
