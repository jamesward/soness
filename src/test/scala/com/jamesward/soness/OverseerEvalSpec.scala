package com.jamesward.soness

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.orchestration.{Overseer, ProcessTree}
import com.jamesward.zio_decisions.orchestration.Overseer.{Stalled, Verdict}
import com.jamesward.zio_decisions.orchestration.ProcessTree.{Id, Kind, Node, State}
import zio.*
import zio.http.Client
import zio.test.*

/**
 * Jev as the overseer of stalled tool calls, live: labelled calls (a test run far past its usual time, a first build
 * with no history, a read that hangs, a dependency download, a server that never answers), each judged by the real Jev
 * `SONESS_EVAL_SAMPLES` times (default 3), in one request for all of them as the watchdog asks. Prints each with Jev's
 * stop probability and the accuracy. Needs `TYPESAFE_API_KEY`.
 */
object OverseerEvalSpec extends ZIOSpecDefault:

  private val t0 = java.time.Instant.parse("2026-01-01T00:00:00Z")

  private def call(id: Long, tool: String, detail: String, running: Long, idle: Long, similar: List[Long], sameTool: List[Long],
      task: String, waited: Int = 0): Stalled =
    Stalled(Node(Id(id), Some(Id(0)), Kind.Call, tool, Some(detail), State.Running, t0, t0, None, None), Some(task), running, idle,
      similar, sameTool, waited)

  private val bug = "Customers are telling us the SAVE10 code doesn't do anything at checkout. Find out why and fix it."
  private val racer = "Implement EasyRacer scenarios 1, 2, 4 in Java 25"

  /** A case: the call, and what should be done. */
  private val cases = List(
    "a test run 10x past its usual" -> (call(1, "execute_terminal_command", """{"command":"./gradlew test --tests MainTest.scenario4"}""", 300, 300,
      List(16, 15, 17), List(16, 15, 17, 3), racer) -> Verdict.Stop),
    "a first build, no history" -> (call(2, "build_project", "{}", 130, 130, Nil, Nil, bug) -> Verdict.Wait),
    "a read that hangs" -> (call(3, "read_file", """{"file_path":"src/main/scala/shop/Discounts.scala"}""", 150, 150, Nil, List(0, 0, 0, 0), bug) -> Verdict.Stop),
    "a search that hangs" -> (call(4, "search_text", """{"q":"SAVE10"}""", 200, 200, Nil, List(1, 0, 1), bug) -> Verdict.Stop),
    "a first full test suite, no history" -> (call(5, "execute_terminal_command", """{"command":"./sbt test"}""", 140, 140, Nil, List(2, 1), bug) -> Verdict.Wait),
    "a test run a bit past its usual" -> (call(6, "execute_terminal_command", """{"command":"./gradlew test"}""", 150, 150,
      List(120, 110, 130), List(120, 110, 130), racer) -> Verdict.Wait),
    "curl to a server that never answers" -> (call(7, "execute_terminal_command", """{"command":"curl http://localhost:8080/4"}""", 240, 240,
      Nil, List(16, 15), racer) -> Verdict.Stop),
    "an edit that hangs" -> (call(8, "apply_patch", """{"patch":"*** Begin Patch ..."}""", 180, 180, Nil, List(0, 0), bug) -> Verdict.Stop),
    "a dependency download, first time" -> (call(9, "execute_terminal_command", """{"command":"./gradlew dependencies --refresh-dependencies"}""",
      150, 150, Nil, List(15), racer) -> Verdict.Wait),
    "a test run waited for twice already, still far past" -> (call(10, "execute_terminal_command", """{"command":"./gradlew test --tests MainTest.scenario2"}""",
      600, 600, List(20, 18), List(20, 18, 16), racer, waited = 2) -> Verdict.Stop),
  )

  private val jev: ZLayer[Client, Throwable, DecisionModel] =
    ZLayer.fromZIO(ZIO.succeed(Option(java.lang.System.getenv("TYPESAFE_API_KEY")).getOrElse("unset")))
      .flatMap(key => TypeSafe.layer(TypeSafe.ApiKey(key.get), TypeSafe.JevLatest))

  def spec = suite("overseer")(
    test("Jev stops stalled calls that are stuck and waits for those that are slow") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
        rounds  <- ZIO.foreach((1 to samples).toList)(_ => Overseer.judge(cases.map(_._2._1)))
        lines    = cases.zipWithIndex.map: (c, i) =>
                     val (name, (_, expected)) = c
                     val got = rounds.map(_(i))
                     val right = got.count(_.verdict == expected)
                     (right, got.size, f"${if right == got.size then "PASS" else "FAIL"} $right/${got.size} $name%-48s expected $expected%-4s " +
                       s"[stop ${got.map(j => f"${j.stopProbability}%.2f").mkString(" ")}]")
        _       <- ZIO.foreachDiscard(lines)(l => Console.printLine(l._3))
        right    = lines.map(_._1).sum
        all      = lines.map(_._2).sum
        _       <- Console.printLine(s"overseer: $right/$all")
      yield assertTrue(right * 10 >= all * 8)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes),
  ).provideShared(Client.default, jev)
