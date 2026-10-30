package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ParallelLoop
import zio.*
import zio.json.ast.Json
import zio.test.*

/**
 * The validation service ([[SharedTools.coordinated]]) under simulated parallel parts, in simulated time: each part
 * thinks (the models' time), edits, and runs its own test until it passes; a test run costs a fixed start (the build
 * tool, the fixtures) plus each selected test's time, a right part's test a few seconds and a wrong one's its timeout.
 * Runs are one at a time, as one build tool on one project is. The specs compare the service's policies by when the last
 * part is done: what bounds a parallel turn's speedup.
 */
object ValidationSimSpec extends ZIOSpecDefault:

  /** A part: how long each attempt's thinking takes, and on which attempt its code is right. */
  final case class SimPart(name: String, think: Duration, rightOn: Int)

  /** The cost model: a run's start, a passing test's time, a failing (hanging) test's. */
  final case class Costs(start: Duration = 15.seconds, pass: Duration = 2.seconds, fail: Duration = 15.seconds)

  /** When the last part was done (in simulated seconds), and how many runs were made. */
  final case class Outcome(seconds: Long, runs: Int)

  private def testOf(part: String) = s"MainTest.$part"

  def simulate(parts: List[SimPart], policy: Option[SharedTools.Policy], costs: Costs = Costs(), gather: Duration = 5.seconds): UIO[Outcome] =
    for
      t0     <- Clock.instant.map(_.getEpochSecond)
      edits  <- Ref.make(Map.empty[String, Int])
      runs   <- Ref.make(0)
      lock   <- Semaphore.make(1)
      toolset = new Toolset:
                  val tools = Chunk.empty
                  def call(name: String, arguments: Json.Obj): Task[Json] =
                    val part = arguments.get("part").flatMap(_.asString).getOrElse("")
                    name match
                      case "apply_patch" => edits.update(m => m.updated(part, m.getOrElse(part, 0) + 1)).as(Json.Str("applied"))
                      case _ =>
                        val command = arguments.get("command").flatMap(_.asString).getOrElse("")
                        val selected = SharedTools.Selection.of(command).toList.flatMap(_.tests)
                        // without the service, runs are still one at a time: one build tool on one project
                        lock.withPermit:
                          for
                            _      <- runs.update(_ + 1)
                            done   <- edits.get
                            results = selected.map: t =>
                                        val p = parts.find(sp => testOf(sp.name) == t)
                                        t -> p.exists(sp => done.getOrElse(sp.name, 0) >= sp.rightOn)
                            _      <- ZIO.sleep(costs.start + results.map((_, ok) => if ok then costs.pass else costs.fail).foldLeft(Duration.Zero)(_ + _))
                          yield Json.Str(results.map((t, ok) =>
                            s"${t.replace(".", " > ")}() ${if ok then "PASSED" else "FAILED"}").mkString("\n"))
      seconds <- ZIO.scoped:
                   for
                     shared <- policy.fold(ZIO.succeed(toolset))(p => SharedTools.coordinated(toolset, gather, p))
                     ends   <- ZIO.foreachPar(parts): part =>
                                 ParallelLoop.currentLoop.locally(Some(part.name)):
                                   def attempt(n: Int): Task[Unit] =
                                     for
                                       _      <- ZIO.sleep(part.think)
                                       _      <- shared.call("apply_patch", Json.Obj("part" -> Json.Str(part.name)))
                                       result <- shared.call("execute_terminal_command",
                                                   Json.Obj("command" -> Json.Str(s"./gradlew test --tests ${testOf(part.name)}")))
                                       text    = SharedTools.textOf(result.asObject.flatMap(_.get("result")).getOrElse(result))
                                       _      <- ZIO.unless(SharedTools.testStatus(text, testOf(part.name)).contains("PASSED"))(attempt(n + 1))
                                     yield ()
                                   attempt(1) *> Clock.instant.map(_.getEpochSecond)
                   yield ends.max
                 .orDie
      n      <- runs.get
    yield Outcome(seconds - t0, n)

  /** Runs `simulate` while moving the test clock forward a second at a time. */
  private def timed(parts: List[SimPart], policy: Option[SharedTools.Policy], costs: Costs = Costs(), gather: Duration = 5.seconds): UIO[Outcome] =
    for
      fiber <- simulate(parts, policy, costs, gather).fork
      _     <- (TestClock.adjust(1.second) *> fiber.poll).repeatUntil(_.isDefined)
      out   <- fiber.join
    yield out

  private val eleven = List(
    SimPart("scenario1", 20.seconds, 1), SimPart("scenario2", 30.seconds, 2), SimPart("scenario3", 60.seconds, 3),
    SimPart("scenario4", 25.seconds, 1), SimPart("scenario5", 20.seconds, 1), SimPart("scenario6", 40.seconds, 2),
    SimPart("scenario7", 20.seconds, 1), SimPart("scenario8", 45.seconds, 2), SimPart("scenario9", 30.seconds, 1),
    SimPart("scenario10", 60.seconds, 3), SimPart("scenario11", 40.seconds, 2),
  )

  private val policies: List[(String, Option[SharedTools.Policy])] = List(
    "one run per call" -> None,
    "merge" -> Some(SharedTools.Policy()),
    "merge+widen" -> Some(SharedTools.Policy(widen = true)),
    "merge+widen with failing" -> Some(SharedTools.Policy(widen = true, widenFailing = true)),
    "merge+widen, ≤2 tests a run" -> Some(SharedTools.Policy(widen = true, maxTests = Some(2))),
    "merge+widen, ≤3 tests a run" -> Some(SharedTools.Policy(widen = true, maxTests = Some(3))),
    "merge+widen, ≤5 tests a run" -> Some(SharedTools.Policy(widen = true, maxTests = Some(5))),
  )

  def spec = suite("validation service, simulated")(
    test("eleven parts: merging runs beats one run per call; widening without failing tests beats widening with them") {
      for
        outcomes <- ZIO.foreach(policies)((name, p) => timed(eleven, p).map(name -> _))
        _        <- ZIO.foreachDiscard(outcomes)((name, o) => Console.printLine(f"$name%-26s last part done at ${o.seconds}%5ds, ${o.runs}%3d runs").orDie)
        by        = outcomes.toMap
      yield assertTrue(
        by("merge").seconds < by("one run per call").seconds,
        by("merge+widen").seconds <= by("merge").seconds,
        by("merge+widen").seconds <= by("merge+widen with failing").seconds,
        by("merge+widen").runs < by("one run per call").runs,
      )
    },
    test("the gathering window: how long a run waits for other parts' requests to join it") {
      for
        outcomes <- ZIO.foreach(List(0, 2, 5, 10, 20))(g => timed(eleven, Some(SharedTools.Policy()), gather = g.seconds).map(g -> _))
        _        <- ZIO.foreachDiscard(outcomes)((g, o) => Console.printLine(f"gather ${g}%2ds: last part done at ${o.seconds}%5ds, ${o.runs}%3d runs").orDie)
      yield assertTrue(outcomes.find(_._1 == 5).exists(o5 => outcomes.forall(o => o5._2.seconds <= o._2.seconds * 11 / 10)))
    },
    test("with no other part at work, a part's run isn't held for the others to join it") {
      for o <- timed(List(SimPart("scenario1", 10.seconds, 1)), Some(SharedTools.Policy()))
      yield assertTrue(o.seconds == 10 + 15 + 2, o.runs == 1)
    },
  )
