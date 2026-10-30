package com.jamesward.soness

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.orchestration.ParallelLoop
import zio.*
import zio.http.Client
import zio.test.*

/**
 * The auto mode's first decision, live: is a request several independent parts (worked on in parallel) or one (one
 * loop)? Labelled requests, each decided by the real Jev `SONESS_EVAL_SAMPLES` times (default 3); prints each with
 * Jev's probability and the accuracy. Needs `TYPESAFE_API_KEY`; one Jev call per request and sample.
 */
object AutoModeEvalSpec extends ZIOSpecDefault:

  private val several = List(
    "Implement EasyRacer scenarios 1, 2, 4 in Java 25: https://github.com/jamesward/easyracer",
    "Implement the six katas in Katas.java, as README.md describes them, and run each one's test to show it passes.",
    "A few checkout changes for this release: 1. Free shipping should start at 75 instead of 50. 2. Add a BULK15 code. " +
      "3. Receipts should include the total number of items. Add tests for each change.",
    "Add CRUD endpoints for users, orders and products, each with its own tests.",
    "Write unit tests for Pricing, Shipping and Discounts.",
    "Port these three utility classes from Java to Kotlin: StringUtils, DateUtils and FileUtils.",
  )
  private val one = List(
    "Customers are telling us the SAVE10 code doesn't do anything at checkout, they still pay full price. Can you find out why and fix it?",
    "When does a customer get free shipping at checkout?",
    "Add a new discount code, SPRING20, that takes 20% off at checkout.",
    "Rename getUserName to getUsername everywhere.",
    "Why is the build failing?",
    "Explain how the checkout total is computed.",
    "Upgrade the project to Scala 3.7 and fix whatever breaks.",
    "list the files in src",
  )

  private val jev: ZLayer[Client, Throwable, DecisionModel] =
    ZLayer.fromZIO(ZIO.succeed(Option(java.lang.System.getenv("TYPESAFE_API_KEY")).getOrElse("unset")))
      .flatMap(key => TypeSafe.layer(TypeSafe.ApiKey(key.get), TypeSafe.JevLatest))

  def spec = suite("auto mode")(
    test("Jev tells requests of several independent parts from requests that are one thing") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
        cases    = several.map(_ -> true) ++ one.map(_ -> false)
        results <- ZIO.foreachPar(cases): (prompt, expected) =>
                     ZIO.foreach((1 to samples).toList)(_ => ParallelLoop.severalParts(prompt, None).map(_._3).either).map((prompt, expected, _))
                   .withParallelism(6)
        lines    = results.map: (prompt, expected, ps) =>
                     val got = ps.collect { case Right(p) => p }
                     val right = got.count(p => (p >= 0.5) == expected)
                     (right, ps.size, f"${if right == ps.size then "PASS" else "FAIL"} $right/${ps.size} ${if expected then "several" else "one    "} " +
                       s"[${got.map(p => f"$p%.2f").mkString(" ")}] ${prompt.take(90)}")
        _       <- ZIO.foreachDiscard(lines)(l => Console.printLine(l._3))
        right    = lines.map(_._1).sum
        all      = lines.map(_._2).sum
        _       <- Console.printLine(s"auto: $right/$all")
      yield assertTrue(right * 10 >= all * 9)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes),
  ).provideShared(Client.default, jev)
