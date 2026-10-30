package com.jamesward.soness

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.typesafe.TypeSafe
import com.jamesward.zio_decisions.orchestration.Interjection
import com.jamesward.zio_decisions.orchestration.Interjection.{Route, Running}
import zio.*
import zio.http.Client
import zio.json.*
import zio.test.*

/**
 * How well Jev tells what a message that arrives mid-task is ([[Interjection]]): labelled cases of a coding agent at
 * work and a message the user sends, each routed by the real Jev `SONESS_EVAL_SAMPLES` times (default 3). Prints each
 * case's routes and probabilities, the accuracy per kind and Jev's tokens and latency; appends a record per case to
 * `target/evals/interjections.jsonl` (labelled `SONESS_EVAL_LABEL`). Needs `TYPESAFE_API_KEY`; costs one Jev call
 * per case and sample.
 */
object InterjectionEvalSpec extends ZIOSpecDefault:

  private val bug = Running(1, "Customers are telling us the SAVE10 code doesn't do anything at checkout, they still pay full price. Can you find out why and fix it?",
    25000, List("search_text {\"q\":\"SAVE10\"}", "read_file {\"file_path\":\"src/main/scala/shop/Discounts.scala\"}"))
  private val feature = Running(2, "Add a SPRING20 code: 20% off orders over $50, until the end of May.", 40000,
    List("read_file {\"file_path\":\"src/main/scala/shop/Discounts.scala\"}", "apply_patch {\"patch\":\"*** Begin Patch…\"}"))
  private val racer = Running(1, "Implement EasyRacer scenarios 1, 2, 4 in Java 25: https://github.com/jamesward/easyracer", 90000,
    List("web_fetch {\"url\":\"https://raw.githubusercontent.com/jamesward/easyracer/main/README.md\"}", "read_file {\"file_path\":\"src/main/java/Main.java\"}",
      "apply_patch {\"patch\":\"… scenario1 …\"}", "execute_terminal_command {\"command\":\"./gradlew test --tests MainTest.scenario1\"}"))

  /** A case: what runs, the message, and the route it should get (by kind, and the task it's about). */
  private final case class Case(name: String, running: List[Running], message: String, expected: Route,
      processes: List[Interjection.Process] = Nil)

  /** The processes below the running tasks: a hung Gradle run in the bug fix, and the racer's scenario loops. */
  private val processes = List(
    Interjection.Process(41, "call", "execute_terminal_command", Some(1), 640, 640, Some("{\"command\":\"./gradlew test --tests MainTest\"}")),
    Interjection.Process(52, "loop", "scenario 4", Some(1), 300, 20, None),
    Interjection.Process(53, "loop", "scenario 2", Some(1), 300, 5, None),
    Interjection.Process(54, "call", "read_file", Some(1), 1, 1, Some("{\"file_path\":\"src/main/java/Main.java\"}")),
  )

  private val cases = List(
    Case("constraint on the fix", List(bug), "make it case-insensitive but also ignore surrounding whitespace", Route.Steer(1)),
    Case("don't touch", List(bug), "don't change the tests, only the main code", Route.Steer(1)),
    Case("redirect", List(bug), "actually I think the problem is in Checkout.scala, look there", Route.Steer(1)),
    Case("stop", List(racer), "stop, that's enough for now", Route.Stop(1)),
    Case("never mind", List(bug), "never mind, I'll fix it myself", Route.Stop(1)),
    Case("two tasks: stop one", List(bug, feature), "forget the SPRING20 code, we're not doing it", Route.Stop(2)),
    Case("stop and redo differently", List(racer), "stop using CompletableFuture, rewrite it with structured concurrency", Route.Steer(1)),
    Case("missed detail", List(racer), "scenario 2 needs the second request cancelled when the first wins", Route.Steer(1)),
    Case("use another tool", List(racer), "use virtual threads, not CompletableFuture", Route.Steer(1)),
    Case("unrelated question", List(bug), "when does a customer get free shipping?", Route.NewTask),
    Case("unrelated change", List(bug), "also, can you rename the README to README.md?", Route.NewTask),
    Case("independent feature", List(racer), "separately, add a GitHub Actions workflow that runs the build", Route.NewTask),
    Case("question about other code", List(feature), "what does the Inventory class do?", Route.NewTask),
    Case("commit after", List(bug), "when you're done, commit it with a good message", Route.After(1)),
    Case("test after", List(bug), "then add a test for that fix", Route.After(1)),
    Case("explain after", List(racer), "afterwards, explain how scenario 4's timeout works in your code", Route.After(1)),
    Case("two tasks: steer the second", List(bug, feature), "the SPRING20 minimum should be $60, not $50", Route.Steer(2)),
    Case("two tasks: steer the first", List(bug, feature), "for the SAVE10 fix, keep the codes in upper case", Route.Steer(1)),
    Case("two tasks: new", List(bug, feature), "how many tests does this project have?", Route.NewTask),
    Case("two tasks: after the second", List(bug, feature), "once SPRING20 is in, update the CHANGELOG for it", Route.After(2)),
    Case("status: what's it doing", List(racer), "what are you doing right now?", Route.Status, processes),
    Case("status: stuck?", List(racer), "is anything stuck?", Route.Status, processes),
    Case("status: how far", List(bug, feature), "how far along is the SPRING20 change?", Route.Status, processes),
    Case("status: check-in", List(racer), "how's it going?", Route.Status, processes),
    Case("status: any progress", List(bug, feature), "any progress?", Route.Status, processes),
    Case("stop process: the hung build", List(racer), "kill that gradle run, it's hung", Route.StopProcess(41), processes),
    Case("stop process: an agent", List(racer), "the scenario 4 agent looks stuck, stop it", Route.StopProcess(52), processes),
    Case("steer, with processes", List(racer), "for scenario 2, cancel the loser with Future.cancel(true)", Route.Steer(1), processes),
  )

  private def kind(route: Route): String = route match
    case Route.NewTask  => "new"
    case Route.Steer(_) => "steer"
    case Route.After(_) => "after"
    case Route.Stop(_)  => "stop"
    case Route.Status   => "status"
    case Route.StopProcess(_) => "stop process"

  private final case class Record(label: String, name: String, expected: String, routes: List[String], correct: Int, samples: Int,
      jevInputTokens: Long, latencyMs: Long) derives JsonCodec

  private val jev: ZLayer[Client, Throwable, DecisionModel] =
    // unset, the eval is skipped; the layer is built anyway
    // the live environment's (a test's `System` is the test one)
    ZLayer.fromZIO(ZIO.succeed(Option(java.lang.System.getenv("TYPESAFE_API_KEY")).getOrElse("unset")))
      .flatMap(key => TypeSafe.layer(TypeSafe.ApiKey(key.get), TypeSafe.JevLatest))

  def spec = suite("interjection routing")(
    test("Jev routes mid-task messages as a new task, steering or a follow-up") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
        label   <- System.env("SONESS_EVAL_LABEL").map(_.getOrElse(""))
        results <- ZIO.foreachPar(cases): c =>
                     ZIO.foreach(1 to samples)(_ => Interjection.route(c.message, c.running, None, c.processes).either).map(c -> _.toList)
                   .withParallelism(6)
        records  = results.map: (c, rs) =>
                     val decided = rs.collect { case Right(d) => d }
                     Record(label, c.name, s"${c.expected}", rs.map(_.fold(e => s"error: ${e.getMessage}", d => s"${d.route}")),
                       decided.count(_.route == c.expected), rs.size, decided.map(_.usage.inputTokens.toLong).sum, decided.map(_.latencyMs).sum)
        _       <- ZIO.foreachDiscard(results.zip(records)): (cr, r) =>
                     val (c, rs) = cr
                     val probabilities = rs.collect { case Right(d) => d.probabilities.toList.sortBy(-_._2).take(3).map((k, p) => f"$k $p%.2f").mkString(" ") }
                     Console.printLine(f"${if r.correct == r.samples then "PASS" else "FAIL"} ${r.correct}/${r.samples} ${c.name}%-28s expected ${r.expected}%-9s got ${r.routes.mkString(", ")}  [${probabilities.headOption.getOrElse("")}]")
        byKind   = records.zip(cases).groupBy((_, c) => kind(c.expected)).toList.sortBy(_._1).map: (k, rs) =>
                     f"$k ${rs.map(_._1.correct).sum}/${rs.map(_._1.samples).sum}"
        total    = records.map(_.correct).sum
        all      = records.map(_.samples).sum
        _       <- Console.printLine(s"accuracy $total/$all (${byKind.mkString(", ")}); jev ${records.map(_.jevInputTokens).sum / all} input tokens and ${records.map(_.latencyMs).sum / all} ms per route")
        _       <- ZIO.attempt:
                     val file = java.nio.file.Path.of("target/evals/interjections.jsonl")
                     java.nio.file.Files.createDirectories(file.getParent)
                     java.nio.file.Files.writeString(file, records.map(_.toJson).mkString("", "\n", "\n"),
                       java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
      yield assertTrue(total >= all * 8 / 10)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.timeout(10.minutes) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem,
  ).provideShared(Client.default, jev)
