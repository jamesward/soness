package com.jamesward.soness

import com.jamesward.soness.IntellijSession.{Call, Turn}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.process.Command
import zio.test.*

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/**
 * Live evals of tasks made of several independent parts, each of which needs code written and tests run: the work a
 * parallel agent could do at the same time, which agents (soness included, so far) do one part after another. Each
 * run is checked on disk by the eval's own hidden validation, part by part, and its transcript is split by part, so
 * the report shows both what got done and how much of the turn's time could have overlapped:
 *
 *   - `easyracer`: "implement EasyRacer in Java 25" (https://github.com/jamesward/easyracer) in a Gradle project with
 *     the scenarios of [[EasyRacerScenarios]] (1, 2 and 4) as stubs and one test each; the spec is on the web (web_fetch), the tests run against the
 *     scenario server in Docker (Testcontainers), and the hidden validation runs each scenario against its own server;
 *   - `katas`: six small independent functions (Roman numerals, run-length encoding, …) in one Java file, each a stub
 *     with its own test (`java KatasTest.java <kata>`, no build tool, a second a run): the cheap task, to measure the
 *     orchestration's own cost; hidden validation with more cases per kata;
 *   - `checkout`: three changes to the shop's checkout (the free-shipping threshold, a BULK15 code, an item count on
 *     receipts) with tests for each, in an sbt project whose suite includes a slow integration spec, and hidden
 *     acceptance tests per change.
 *
 * Each task runs through each arm of `SONESS_EVAL_ARMS` (default `soness`; see [[IntellijArms]]) with the mock IDE
 * server, `SONESS_EVAL_SAMPLES` (default 1) times; `SONESS_PARALLEL_TASKS` picks tasks (default both). soness gets
 * `SONESS_MAX_STEPS` 60 (these tasks take far more than the default 12). Records go to `target/evals/parallel-tasks.jsonl`.
 * Needs the soness credentials, Docker, and network for the spec and dependencies; it calls paid APIs.
 *
 * Timeline: each call is attributed to the part its input names (a scenario, a change), to validation (a build or a
 * test run), or to shared work (reading the spec, the project); a call's time is from the previous call's end to its
 * own end, so the thinking before it counts with it. The parallel estimate is shared + validation + the longest part:
 * what the turn could take if the parts ran at once with no coordination cost.
 */
object IntellijParallelEvalSpec extends ZIOSpecDefault:

  /** One part of a task, checked on its own. */
  final case class PartResult(part: String, passed: Boolean, detail: String)

  final case class ParallelTask(
    name: String,
    prompt: String,
    project: String,
    parts: List[(String, scala.util.matching.Regex)],
    validate: Path => Task[List[PartResult]],
    extraChecks: (Path, Turn) => Task[List[(String, Boolean)]],
  )

  private val resources = AcpSubprocess.property("user.dir").map(d => Path.of(d, "src/test/resources/eval-projects"))

  /** A fresh copy of an eval project, its launchers executable; the checkout project gets the sbt launcher. */
  private def fresh(project: String): ZIO[Scope, Throwable, Path] =
    for
      base <- resources
      root <- SkillsSpec.tree().map(_.toRealPath())
      repo <- AcpSubprocess.property("user.dir").map(Path.of(_))
      _    <- ZIO.attemptBlocking:
                val from = base.resolve(project)
                Files.walk(from).iterator.asScala.foreach: p =>
                  val target = root.resolve(from.relativize(p).toString)
                  if Files.isDirectory(p) then Files.createDirectories(target)
                  else Files.copy(p, target, StandardCopyOption.REPLACE_EXISTING)
                if project == "checkout" then Files.copy(repo.resolve("sbt"), root.resolve("sbt"), StandardCopyOption.REPLACE_EXISTING)
                List("sbt", "gradlew").map(root.resolve).filter(Files.exists(_)).foreach(_.toFile.setExecutable(true))
      _    <- ZIO.when(project == "easyracer-java")(easyRacerSources(root))
      _    <- ZIO.addFinalizer(stopBuildServers(root))
    yield root

  /** The run's project sources as it left them, under `target/evals/projects/<name>`, for looking into afterwards. */
  private def keep(root: Path, name: String): Task[Path] =
    AcpSubprocess.property("user.dir").flatMap: dir =>
      ZIO.attemptBlocking:
        val to = Path.of(dir, "target/evals/projects", name)
        if Files.exists(to) then Files.walk(to).iterator.asScala.toList.reverse.foreach(Files.delete)
        Files.walk(root).iterator.asScala
          .filter(p => Files.isRegularFile(p) && !root.relativize(p).iterator.asScala.exists(s => Set("build", "target", ".gradle", "project", ".bsp")(s.toString)))
          .foreach: p =>
            val target = to.resolve(root.relativize(p).toString)
            Files.createDirectories(target.getParent)
            Files.copy(p, target)
        to

  /** sbt's server and Gradle's daemon outlive a run; stopped with the project. */
  private def stopBuildServers(root: Path): UIO[Unit] =
    (ZIO.when(Files.exists(root.resolve("sbt")))(run(root, 60.seconds, "./sbt", "shutdown")) *>
      ZIO.when(Files.exists(root.resolve("gradlew")))(run(root, 60.seconds, "./gradlew", "--stop"))).ignore

  /** A command's output (stdout and stderr together) and exit code. */
  private def run(dir: Path, timeout: Duration, command: String*): Task[(String, Int)] =
    ZIO.scoped:
      Command(command.head, command.tail*).workingDirectory(dir.toFile).redirectErrorStream(true).run.flatMap: process =>
        (process.stdout.string <*> process.exitCode.map(_.code))
          .timeoutFail(RuntimeException(s"${command.mkString(" ")} took more than $timeout"))(timeout)

  private def read(root: Path, file: String): String =
    val path = root.resolve(file)
    if Files.isRegularFile(path) then Files.readString(path) else ""

  // ---- easyracer ----

  /**
   * The Easy Racer scenarios the task asks for: `SONESS_EASYRACER_SCENARIOS` (comma-separated, `all` for 1–11), by
   * default 1, 2 and 4, a short task to iterate on. The project's stubs and tests are written for them.
   */
  val EasyRacerScenarios: List[Int] =
    Option(java.lang.System.getenv("SONESS_EASYRACER_SCENARIOS")).map(_.trim).filter(_.nonEmpty) match
      case Some("all") => (1 to 11).toList
      case Some(list)  => list.split(',').toList.flatMap(_.trim.toIntOption).filter(n => n >= 1 && n <= 11).distinct.sorted
      case None        => List(1, 2, 4)

  /** The starting `Main.java` and `MainTest.java` for [[EasyRacerScenarios]]. */
  private def easyRacerSources(root: Path): Task[Unit] =
    val list = EasyRacerScenarios.mkString(", ")
    val stubs = EasyRacerScenarios.map: n =>
      s"""        public String scenario$n() throws Exception {
         |            throw new UnsupportedOperationException("scenario $n is not implemented yet");
         |        }
         |""".stripMargin
    val main =
      s"""import java.net.URI;
         |import java.util.List;
         |
         |public class Main {
         |
         |    /** Easy Racer scenarios $list, against the scenario server at {@code url}. Each returns the winner's body. */
         |    static class Scenarios {
         |        private final URI url;
         |
         |        Scenarios(URI url) {
         |            this.url = url;
         |        }
         |
         |${stubs.mkString("\n")}
         |        public List<String> results() throws Exception {
         |            return List.of(${EasyRacerScenarios.map(n => s"scenario$n()").mkString(", ")});
         |        }
         |    }
         |
         |    public static void main(String[] args) throws Exception {
         |        var scenarios = new Scenarios(new URI(args.length > 0 ? args[0] : "http://localhost:8080"));
         |        scenarios.results().forEach(System.out::println);
         |    }
         |}
         |""".stripMargin
    ZIO.attemptBlocking:
      Files.writeString(root.resolve("src/main/java/Main.java"), main)
      val test = root.resolve("src/test/java/MainTest.java")
      val text = Files.readString(test)
      val head = text.take(text.indexOf("    @Test"))
      val tests = EasyRacerScenarios.map: n =>
        s"""    @Test
           |    void scenario$n() throws Exception {
           |        assertEquals("right", scenarios.scenario$n());
           |    }
           |""".stripMargin
      Files.writeString(test, head + tests.mkString("\n") + "\n}\n")
    .unit

  private val easyracer = ParallelTask(
    "easyracer",
    s"Implement EasyRacer scenarios ${EasyRacerScenarios.mkString(", ")} in Java 25: https://github.com/jamesward/easyracer\n\n" +
      "This project is the start of the client: each of those scenarios is a stub method of Main.Scenarios, and MainTest has a " +
      "test for each. Implement them as the EasyRacer spec describes them, and run the tests to show they pass.\n\n" +
      "The spec is the repository's README (https://raw.githubusercontent.com/jamesward/easyracer/main/README.md): it's all " +
      "you need. Write the implementation yourself, from the spec; don't look at or copy the repository's existing clients.",
    "easyracer-java",
    EasyRacerScenarios.map(n => s"scenario $n" -> s"""(?i)scenario[ _-]?0?$n(?!\\d)|"/$n[?"]|/$n\\?""".r),
    validateEasyRacer,
    (root, turn) => ZIO.succeed(List(
      "the turn ended with an answer" -> turn.ending.contains("answered"),
      "the agent ran the tests" -> turn.calls.exists(c => c.status == "completed" && """gradlew[^"]*test""".r.findFirstIn(c.input).isDefined),
      // the repository has clients in other languages and Java ones; the task is to write one, not fetch one
      "it wrote its own implementation (fetched no client's source)" -> !turn.calls.exists(c =>
        c.tool == "web_fetch" && """easyracer/(main/)?[a-z]+-[a-z-]+/|contents/[a-z]+-[a-z-]+""".r.findFirstIn(c.input).isDefined),
      "MainTest still tests every scenario" -> EasyRacerScenarios.forall(n => read(root, "src/test/java/MainTest.java").contains(s"scenario$n()")),
    )),
  )

  /** A scenario server for the life of the scope (a container, removed after), and its URL once it answers. */
  private def scenarioServer(root: Path): ZIO[Scope, Throwable, String] =
    (for
      container <- ZIO.acquireRelease(run(root, 2.minutes, "docker", "run", "-d", "-p", "127.0.0.1::8080", "ghcr.io/jamesward/easyracer")
                     .map(_._1.trim))(id => run(root, 1.minute, "docker", "rm", "-f", id).ignore)
      port      <- run(root, 1.minute, "docker", "port", container, "8080").map(_._1.linesIterator.next().split(':').last.trim)
      _         <- run(root, 2.minutes, "bash", "-c", s"for i in $$(seq 1 60); do curl -s -o /dev/null -m 2 http://127.0.0.1:$port/ && exit 0; sleep 1; done; exit 1")
                     .filterOrFail(_._2 == 0)(RuntimeException("the scenario server didn't start"))
    yield s"http://127.0.0.1:$port").retry(Schedule.recurs(3) && Schedule.spaced(5.seconds))

  /** Each scenario of the project's `Main.Scenarios` against a fresh scenario server, one at a time. */
  private def validateEasyRacer(root: Path): Task[List[PartResult]] =
    val failed = (why: String) => EasyRacerScenarios.map(n => PartResult(s"scenario $n", false, why))
    for
      compiled <- run(root, 10.minutes, "./gradlew", "-q", "--console=plain", "compileJava")
      out      <- (if compiled._2 != 0 then ZIO.succeed(failed(s"doesn't compile: ${compiled._1.linesIterator.take(5).mkString(" ")}"))
                  else
                    // a scenario server that doesn't start (Docker having a bad moment) is started again, up to three times:
                    // that's the eval's failure, not the run's
                    ZIO.scoped(
                      for
                        base      <- resources
                        classes   <- ZIO.attemptBlocking(Files.createTempDirectory("easyracer-validate"))
                        _         <- ZIO.addFinalizer(ZIO.attemptBlocking(Files.walk(classes).iterator.asScala.toList.reverse.foreach(Files.delete)).ignore)
                        javac     <- run(root, 2.minutes, "javac", "--release", "25", "--enable-preview", "-cp", "build/classes/java/main",
                                       "-d", classes.toString, base.resolve("easyracer-validate/Validate.java").toString)
                        // a Main without Main.Scenarios (moved into a package, renamed) fails every scenario
                        _         <- ZIO.fail(RuntimeException(s"no Main.Scenarios to run: ${javac._1.linesIterator.take(2).mkString(" ")}"))
                                       .when(javac._2 != 0)
                        container <- ZIO.acquireRelease(run(root, 2.minutes, "docker", "run", "-d", "-p", "127.0.0.1::8080", "ghcr.io/jamesward/easyracer")
                                       .map(_._1.trim))(id => run(root, 1.minute, "docker", "rm", "-f", id).ignore)
                        port      <- run(root, 1.minute, "docker", "port", container, "8080").map(_._1.linesIterator.next().split(':').last.trim)
                        // the server is ready when it answers HTTP (any status), at most a minute after it starts
                        _         <- run(root, 2.minutes, "bash", "-c", s"for i in $$(seq 1 60); do curl -s -o /dev/null -m 2 http://127.0.0.1:$port/ && exit 0; sleep 1; done; exit 1")
                                       .filterOrFail(_._2 == 0)(RuntimeException("the scenario server didn't start"))
                                       .tapError(_ => ZIO.logWarning(s"the scenario server on $port didn't start; trying a new one"))
                        results   <- run(root, 25.minutes, "java", "--enable-preview", "-cp", s"build/classes/java/main:$classes", "Validate",
                                       s"http://127.0.0.1:$port", "90", EasyRacerScenarios.mkString(","))
                      yield
                        val lines = results._1.linesIterator.map(_.trim).filter(_.matches("""\d+ .*""")).map(l => l.takeWhile(_ != ' ').toInt -> l.dropWhile(_ != ' ').trim).toMap
                        EasyRacerScenarios.map: n =>
                          val line = lines.getOrElse(n, "not run")
                          PartResult(s"scenario $n", line == "right", line.take(160))
                    ).retry(Schedule.recurs(3) && Schedule.spaced(10.seconds) &&
                      Schedule.recurWhile[Throwable](e => String.valueOf(e.getMessage).contains("scenario server didn't start")))
                  ).catchAll(e => ZIO.succeed(failed(e.getMessage.take(200))))
    yield out

  // ---- katas ----

  private val KataNames = List("roman", "runLength", "balanced", "wordFrequencies", "luhn", "spiral")

  private val katas = ParallelTask(
    "katas",
    "Implement the six katas in Katas.java, as README.md describes them, and run each one's test to show it passes.",
    "katas",
    KataNames.map(k => k -> s"(?i)\\b$k\\b".r),
    validateKatas,
    (root, turn) => ZIO.succeed(List(
      "the turn ended with an answer" -> turn.ending.contains("answered"),
      "the agent ran the tests" -> turn.calls.exists(c => c.status == "completed" && c.input.contains("KatasTest")),
      "KatasTest.java is unchanged" -> (read(root, "KatasTest.java") == read(Path.of(java.lang.System.getProperty("user.dir"),
        "src/test/resources/eval-projects/katas"), "KatasTest.java")),
    )),
  )

  /** The hidden cases, with the project's Katas.java: one result per kata. */
  private def validateKatas(root: Path): Task[List[PartResult]] =
    ZIO.scoped:
      for
        base   <- resources
        dir    <- ZIO.attemptBlocking(Files.createTempDirectory("katas-validate"))
        _      <- ZIO.addFinalizer(ZIO.attemptBlocking(Files.walk(dir).iterator.asScala.toList.reverse.foreach(Files.delete)).ignore)
        _      <- ZIO.attemptBlocking:
                    Files.copy(root.resolve("Katas.java"), dir.resolve("Katas.java"))
                    Files.copy(base.resolve("katas-validate/KatasValidate.java"), dir.resolve("KatasValidate.java"))
        out    <- run(dir, 2.minutes, "java", "KatasValidate.java")
      yield
        val lines = out._1.linesIterator.map(_.trim).toList
        KataNames.map: k =>
          lines.find(l => l == s"PASS $k" || l.startsWith(s"FAIL $k:")) match
            case Some(l) => PartResult(k, l.startsWith("PASS"), l.take(200))
            case None    => PartResult(k, false, s"not run: ${lines.take(3).mkString(" | ").take(200)}")
    .catchAll(e => ZIO.succeed(KataNames.map(k => PartResult(k, false, String.valueOf(e.getMessage).take(200)))))

  // ---- checkout ----

  private val checkout = ParallelTask(
    "checkout",
    "A few checkout changes for this release:\n" +
      "1. Free shipping should start at 75 instead of 50.\n" +
      "2. Add a BULK15 code: 15% off, but only for orders of 10 or more items in total.\n" +
      "3. Receipts should include the total number of items in the order, as `itemCount`.\n" +
      "Add tests for each change, and make sure the whole test suite passes.",
    "checkout",
    List(
      "shipping" -> """(?i)shipping|FreeAbove|\b75\b""".r,
      "bulk" -> """(?i)bulk""".r,
      "itemCount" -> """(?i)itemCount|item count""".r,
    ),
    validateCheckout,
    (root, turn) =>
      val tests = Files.walk(root.resolve("src/test")).iterator.asScala.filter(_.toString.endsWith(".scala")).map(p => Files.readString(p)).mkString("\n")
      def added(word: String) = """test\("[^"]*""".r.findAllIn(tests).size > 10 && tests.toLowerCase.contains(word.toLowerCase)
      ZIO.succeed(List(
        "the turn ended with an answer" -> turn.ending.contains("answered"),
        "tests were added for the shipping threshold" -> (added("75")),
        "tests were added for BULK15" -> added("bulk15"),
        "tests were added for itemCount" -> added("itemCount"),
        "the slow simulation spec is unchanged" -> read(root, "src/test/scala/shop/CheckoutSimulationSpec.scala").contains("Thread.sleep(20000)"),
      )),
  )

  /** The hidden acceptance tests, with the project's own suite: each acceptance test's part, and whether the suite passed. */
  private def validateCheckout(root: Path): Task[List[PartResult]] =
    for
      base   <- resources
      _      <- ZIO.attemptBlocking:
                  Files.createDirectories(root.resolve("src/test/scala/shop/acceptance"))
                  Files.copy(base.resolve("checkout-acceptance/AcceptanceSpec.scala"), root.resolve("src/test/scala/shop/acceptance/AcceptanceSpec.scala"),
                    StandardCopyOption.REPLACE_EXISTING)
      // the acceptance tests alone (a failing project test would hide their results), then the whole suite
      accepted <- run(root, 15.minutes, "./sbt", "-batch", "testOnly shop.acceptance.AcceptanceSpec")
      _      <- ZIO.attemptBlocking(Files.deleteIfExists(root.resolve("src/test/scala/shop/acceptance/AcceptanceSpec.scala")))
      result <- run(root, 15.minutes, "./sbt", "-batch", "test")
    yield
      val output = accepted._1.replaceAll("\u001b\\[[0-9;]*[mJ]", "")
      val suite = result._1.replaceAll("\u001b\\[[0-9;]*[mJ]", "")
      // munit: "  + name 0.01s" passed, "==> X shop.acceptance.AcceptanceSpec.name ..." failed
      val passed = """(?m)^\s+\+ (.+?) [0-9.]+s$""".r.findAllMatchIn(output).map(_.group(1)).toSet
      val names = List(
        "shipping: free from 75", "shipping: charged below 75", "shipping: charged at 50 now", "shipping: FREESHIP is still free",
        "bulk: BULK15 takes 15% off 10 or more items", "bulk: counts items across lines", "bulk: nothing off 9 items",
        "bulk: SAVE10 and WELCOME5 still apply to any order",
        "itemCount: a receipt counts the items", "itemCount: an empty order has none", "itemCount: totals still add up",
      )
      val compiled = !output.contains("Compilation failed")
      val byPart = names.groupBy(_.takeWhile(_ != ':')).toList.sortBy(_._1).map: (part, tests) =>
        val failing = tests.filterNot(passed)
        PartResult(part, compiled && failing.isEmpty,
          if !compiled then "the tests don't compile" else if failing.isEmpty then s"${tests.size} acceptance tests pass" else s"failing: ${failing.mkString("; ")}")
      byPart :+ PartResult("the whole suite", result._2 == 0, if result._2 == 0 then "passes" else
        suite.linesIterator.filter(l => l.contains("==> X") || l.contains("[error]")).take(4).mkString(" | "))

  // ---- the timeline ----

  private val Validation = """(?i)gradlew|\bsbt\b|testQuick|\btest\b|build_project|get_file_problems|execute_run_configuration""".r

  /** Each part's time, validation's, shared work's, and the turn's; plus how many calls each had. */
  final case class Timeline(total: Long, byArea: List[(String, Long, Int)]):
    def time(area: String): Long = byArea.find(_._1 == area).fold(0L)(_._2)
    def parallelEstimate(parts: List[String]): Long =
      time("shared") + time("validation") + time("several parts") + parts.map(time).maxOption.getOrElse(0L)

  def timeline(calls: List[Call], total: Long, parts: List[(String, scala.util.matching.Regex)]): Timeline =
    val timed = calls.filter(_.startMs >= 0).sortBy(_.startMs)
    val (_, spans) = timed.foldLeft((0L, List.empty[(String, Long)])):
      case ((previousEnd, out), call) =>
        val end = call.startMs + call.durationMs.max(0)
        val text = s"${call.tool} ${call.input}"
        val matched = parts.filter(_._2.findFirstIn(text).isDefined).map(_._1)
        val area =
          if Validation.findFirstIn(text).isDefined && !call.tool.toLowerCase.contains("patch") && !call.tool.toLowerCase.contains("edit") &&
            !call.tool.toLowerCase.contains("write") && !call.tool.toLowerCase.contains("create") then "validation"
          else matched match
            case Nil         => "shared"
            case part :: Nil => part
            case _           => "several parts"
        (end max previousEnd, out :+ (area -> (end - previousEnd).max(0)))
    val tail = total - timed.lastOption.fold(0L)(c => c.startMs + c.durationMs.max(0))
    val all = spans :+ ("reply" -> tail.max(0))
    Timeline(total, all.groupBy(_._1).toList.map((area, xs) => (area, xs.map(_._2).sum, xs.size)).sortBy(-_._2))

  /**
   * The turn's loops as the agent reported them (`_meta.soness.loop` on each call: soness's parallel mode): each
   * loop's span, from the turn's start or its first call (the earliest of its calls' starts) to its last call's end,
   * with its calls; and the critical path, the gathering and setup, then the longest part, then the integration.
   */
  final case class Loops(spans: List[(String, Long, Long, Int)]):
    def nonEmpty: Boolean = spans.nonEmpty
    private def phase(name: String) = spans.filter(_._1.split('/').last == name).map(s => s._3 - s._2).sum
    def parts: List[(String, Long, Long, Int)] = spans.filterNot(s => Set("gather", "setup", "integration", "main")(s._1.split('/').last))
    def describe: String =
      val longest = parts.maxByOption(s => s._3 - s._2)
      s"loops: gather ${phase("gather") / 1000}s, setup ${phase("setup") / 1000}s, parts ${parts.size} " +
        s"(longest ${longest.fold("-")(l => s"${l._1.split('/').last} ${(l._3 - l._2) / 1000}s/${l._4} calls")}, " +
        s"median ${parts.map(s => s._3 - s._2).sorted.lift(parts.size / 2).getOrElse(0L) / 1000}s), integration ${phase("integration") / 1000}s" +
        (if phase("main") > 0 then s", one loop ${phase("main") / 1000}s" else "")

  def loops(calls: List[Call]): Loops =
    Loops(calls.filter(c => c.loop.isDefined && c.startMs >= 0).groupBy(_.loop.get).toList.map: (loop, cs) =>
      (loop, cs.map(_.startMs).min, cs.map(c => c.startMs + c.durationMs.max(0)).max, cs.size)
    .sortBy(_._2))

  // ---- the spec ----

  def spec = suite("parallel-work task evals")(
    test("tasks with independent parts, validated part by part, with their parallel potential") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
        names   <- System.env("SONESS_EVAL_ARMS").map(_.getOrElse("soness").split(",").toList.map(_.trim).filter(_.nonEmpty))
        which   <- System.env("SONESS_PARALLEL_TASKS").map(_.getOrElse("easyracer,checkout").split(",").map(_.trim).toSet)
        tasks    = List(easyracer, checkout, katas).filter(t => which(t.name))
        // `SONESS_EVAL_TURN_MINUTES` (default 15): how long a turn may take
        minutes <- System.env("SONESS_EVAL_TURN_MINUTES").map(_.flatMap(_.toIntOption).getOrElse(15))
        // `SONESS_EASYRACER_WARM` (default true): the agent's tests use a scenario server kept running for the turn
        // (`EASYRACER_URL`), so a test run is seconds, as a developer's would be, not a container's start
        warm    <- System.env("SONESS_EASYRACER_WARM").map(_.forall(_ != "false"))
        ide      = IntellijSession.Ide(None, None, None)
        _       <- ZIO.foreach(for t <- tasks; a <- names; n <- 1 to samples yield (t, a, n)): (task, armName, n) =>
                     ZIO.scoped:
                       for
                         root    <- fresh(task.project)
                         server  <- if task.name == "easyracer" && warm then scenarioServer(root).asSome else ZIO.none
                         arm     <- IntellijArms.arm(armName, Map("SONESS_MAX_STEPS" -> "60") ++ server.map("EASYRACER_URL" -> _), minutes.minutes)
                         turn    <- IntellijArms.turn(arm, task.prompt, ide, root)
                         // each run's own directory (its label, and the sample), so a later run can't replace it
                         label   <- System.env("SONESS_EVAL_LABEL").map(_.filter(_.nonEmpty).getOrElse("run"))
                         kept    <- keep(root, s"$label-${task.name}-${arm.name}-$n")
                         parts   <- task.validate(root).catchAll(e => ZIO.succeed(List(PartResult("validation", false, e.getMessage.take(300)))))
                         checks  <- task.extraChecks(root, turn)
                         total    = turn.metric("latencyMs").flatMap(_.asNumber).map(_.value.longValue).getOrElse(0L)
                         line     = timeline(turn.calls, total, task.parts)
                         estimate = line.parallelEstimate(task.parts.map(_._1))
                         extra    = Json.Obj(
                                      "parts" -> Json.Obj(Chunk.from(parts.map(p => p.part -> Json.Bool(p.passed)))),
                                      "timeline" -> Json.Obj(Chunk.from(line.byArea.map((a, ms, n) => a -> Json.Obj("ms" -> Json.Num(ms), "calls" -> Json.Num(n))))),
                                      "parallelEstimateMs" -> Json.Num(estimate),
                                      "loops" -> Json.Obj(Chunk.from(loops(turn.calls).spans.map((l, a, b, c) =>
                                        l -> Json.Obj("startMs" -> Json.Num(a), "endMs" -> Json.Num(b), "calls" -> Json.Num(c))))),
                                    )
                         _       <- IntellijSession.record("parallel-tasks", task.name, ide, turn,
                                      checks ++ parts.map(p => s"${p.part}: ${p.detail}" -> p.passed), arm.name, extra)
                         _       <- Console.printLine(report(task, arm.name, n, turn, parts, checks, line, estimate))
                       yield ()
      yield assertCompletes
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK")
      @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID"),
    test("the validations reject the starting projects, and accept reference solutions (SONESS_PARALLEL_VALIDATORS)") {
      ZIO.scoped:
        for
          er      <- fresh("easyracer-java")
          start   <- validateEasyRacer(er)
          // the easyracer repo's own Java + Loom client, when a checkout of it is next to this one
          ref      = Path.of(java.lang.System.getProperty("user.home"), "projects/easyracer/java-loom/src/main/java/Main.java")
          solved  <- ZIO.when(Files.exists(ref))(ZIO.attemptBlocking(Files.copy(ref, er.resolve("src/main/java/Main.java"),
                       StandardCopyOption.REPLACE_EXISTING)) *> validateEasyRacer(er))
          ka      <- fresh("katas")
          kaStart <- validateKatas(ka)
          kaRef   <- resources.flatMap(b => ZIO.attemptBlocking(Files.copy(b.resolve("katas-reference/Katas.java"), ka.resolve("Katas.java"),
                       StandardCopyOption.REPLACE_EXISTING))) *> validateKatas(ka)
          co      <- fresh("checkout")
          coStart <- validateCheckout(co)
          _       <- ZIO.attemptBlocking:
                       def change(file: String, from: String, to: String) =
                         val p = co.resolve(file)
                         val text = Files.readString(p)
                         require(text.contains(from), s"$file has no: $from")
                         Files.writeString(p, text.replace(from, to))
                       change("src/main/scala/shop/Shipping.scala", "BigDecimal(50)", "BigDecimal(75)")
                       change("src/test/scala/shop/ShippingSpec.scala", "free at 50\"):\n    assertEquals(Shipping.cost(BigDecimal(50)",
                         "free at 75\"):\n    assertEquals(Shipping.cost(BigDecimal(75)")
                       change("src/main/scala/shop/Model.scala", "total: BigDecimal)", "total: BigDecimal, itemCount: Int)")
                       change("src/main/scala/shop/Checkout.scala", "subtotal - off + shipping)", "subtotal - off + shipping, order.lines.map(_.quantity).sum)")
                       change("src/main/scala/shop/Checkout.scala", "val discount = code.flatMap(Discounts.lookup)",
                         "val items = order.lines.map(_.quantity).sum\n    val discount = code.flatMap(c =>\n      if c.trim.equalsIgnoreCase(\"BULK15\") then Option.when(items >= 10)(Discount(\"bulk15\", 15)) else Discounts.lookup(c))")
          coDone  <- validateCheckout(co)
          _       <- Console.printLine(s"easyracer start: ${start.map(_.passed)}\nreference: ${solved.map(_.map(p => p.part -> p.detail))}\n" +
                       s"checkout start: $coStart\nchanged: $coDone")
        yield assertTrue(
          start.forall(!_.passed), start.size == EasyRacerScenarios.size,
          kaStart.forall(!_.passed), kaStart.size == 6, kaRef.forall(_.passed),
          solved.forall(_.forall(_.passed)),
          coStart.filter(_.part != "the whole suite").forall(!_.passed), coStart.find(_.part == "the whole suite").exists(_.passed),
          coDone.forall(_.passed),
        )
    } @@ TestAspect.ifEnv("SONESS_PARALLEL_VALIDATORS")(_ == "true"),
    test("rescore a kept project (SONESS_RESCORE=<dir under target/evals/projects>): its parts, validated again") {
      for
        name  <- System.env("SONESS_RESCORE").someOrFail(IllegalStateException("SONESS_RESCORE is not set"))
        dir   <- AcpSubprocess.property("user.dir").map(d => Path.of(d, "target/evals/projects", name))
        parts <- validateEasyRacer(dir)
        _     <- Console.printLine(s"rescored $name: ${parts.count(_.passed)}/${parts.size}\n" + parts.map(p => s"   ${if p.passed then "✓" else "✗"} ${p.part}: ${p.detail}").mkString("\n"))
      yield assertCompletes
    } @@ TestAspect.ifEnvSet("SONESS_RESCORE"),
    test("the timeline attributes calls to parts, validation and shared work, the thinking before a call with it") {
      val parts = List("scenario 1" -> "(?i)scenario 1(?!\\d)".r, "scenario 2" -> "(?i)scenario 2(?!\\d)".r)
      val calls = List(
        Call("web_fetch", """{"url":"https://x/README.md"}""", "completed", "", 1000, 500),
        Call("apply_patch", """{"input":"... scenario 1 ..."}""", "completed", "", 4000, 100),
        Call("apply_patch", """{"input":"... scenario 2 ..."}""", "completed", "", 6000, 100),
        Call("execute_terminal_command", """{"command":"./gradlew test"}""", "completed", "", 6200, 3000),
      )
      val line = timeline(calls, 10000, parts)
      assertTrue(
        line.time("shared") == 1500, line.time("scenario 1") == 2600, line.time("scenario 2") == 2000,
        line.time("validation") == 3100, line.time("reply") == 800,
        line.parallelEstimate(parts.map(_._1)) == 1500 + 3100 + 2600,
      )
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(6.hours) @@ TestAspect.sequential

  private def report(task: ParallelTask, arm: String, n: Int, turn: Turn, parts: List[PartResult], checks: List[(String, Boolean)],
      line: Timeline, estimate: Long): String =
    def m(name: String) = turn.metric(name).map(j => j.asString.getOrElse(j.toString)).getOrElse("?")
    val done = parts.count(_.passed)
    (List(
      s"== ${task.name} [$arm #$n]: ${done}/${parts.size} parts pass; ending ${m("ending")}, ${line.total / 1000}s, ${turn.calls.size} tool calls " +
        s"(${turn.failedCalls} failed); jev ${m("jevCalls")} calls ${m("jevInputTokens")} in; llm ${m("llmCalls")} calls ${m("llmInputTokens")} in / ${m("llmOutputTokens")} out" +
        turn.metric("costUsd").fold("")(c => s", $$$c"),
      s"   parallel estimate ${estimate / 1000}s (${if estimate > 0 then f"${line.total.toDouble / estimate}%.1fx" else "?"} of the turn): " +
        line.byArea.map((a, ms, c) => s"$a ${ms / 1000}s/$c").mkString(", "),
    ) ++ Option.when(loops(turn.calls).nonEmpty)(s"   ${loops(turn.calls).describe}").toList ++ parts.map(p => s"   ${if p.passed then "✓" else "✗"} ${p.part}: ${p.detail}") ++
      checks.map((c, ok) => s"   ${if ok then "✓" else "✗"} $c") ++
      turn.error.map(e => s"   error: $e") ++
      List(s"   answer: ${turn.answer.replace('\n', ' ').take(300)}")).mkString("\n")
