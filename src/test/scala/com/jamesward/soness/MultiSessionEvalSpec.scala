package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.{field, str}
import com.jamesward.soness.IntellijSession.*
import java.nio.file.{Files, Path}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/**
 * Two chats of one IDE working in one project at the same time, end to end: soness launched as IntelliJ IDEA launches
 * it, two sessions over the shop project of [[IntellijTaskEvalSpec]], each given its own change at once (the SAVE10
 * fix, and SPRING20). Both must be done on disk, neither undoing the other; the report says how long the pair took
 * and how many builds and test runs the project's shared coordinator made. `SONESS_EVAL_SAMPLES` (default 1).
 * Needs `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID`.
 */
object MultiSessionEvalSpec extends ZIOSpecDefault:

  private val bug = IntellijTaskEvalSpec.tasks.head
  private val feature = IntellijTaskEvalSpec.tasks(2)

  def spec = suite("two sessions, one project")(
    test("two chats' changes at once are both done, and neither undoes the other") {
      ZIO.scoped:
        for
          samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
          log     <- System.env("SONESS_EVAL_LOG")
          driver  <- IntellijSession.driver(Ide(None, None, None), log)
          results <- ZIO.foreach((1 to samples).toList): n =>
                       for
                         root    <- SkillsSpec.tree(bug.project*).map(_.toRealPath())
                         started <- Clock.instant
                         r       <- driver.twoSessions(bug.prompt, feature.prompt, root, 15.minutes)
                         ended   <- Clock.instant
                         source   = Files.readString(root.resolve("src/main/scala/shop/Discounts.scala"))
                         checks   = List(
                                      "both prompts answered" -> (r.first.exists(_.get("result").isDefined) && r.second.exists(_.get("result").isDefined)),
                                      "the codes are fixed, SPRING20 included" ->
                                        IntellijTaskEvalSpec.codesWork(source, Map("save10" -> 10, "welcome5" -> 5, "spring20" -> 20)),
                                    )
                         ms       = java.time.Duration.between(started, ended).toMillis
                         _       <- Console.printLine(s"${if checks.forall(_._2) then "PASS" else "FAIL"} two sessions [#$n] in ${ms}ms; " +
                                      s"calls ${r.calls.mkString(", ")}\n" + checks.map((k, ok) => s"     ${if ok then "✓" else "✗"} $k").mkString("\n"))
                       yield checks.forall(_._2)
        yield assertTrue(results.forall(identity))
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK") @@ TestAspect.withLiveClock
      @@ TestAspect.withLiveSystem @@ TestAspect.timeout(40.minutes),
  )
