package com.jamesward.soness

import com.jamesward.soness.dev.GoalRun
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path, StandardOpenOption}
import java.nio.file.attribute.FileTime

/**
 * File tasks a coding agent meets outside the sources (logs, build output, test reports), end to end: soness in this
 * process (its ACP agent driven in memory) on a fixture project with no MCP server, so it has its own tools only: file
 * tools and a shell. Each task has a known answer, checked in the reply, and the eval counts the shell calls the work
 * made: a file task done with `ls`, `cat`, `grep`, `find`, `tail` or `wc` in a shell is one the native file tools should
 * have done. A sample passes when its answer is right and the shell wasn't used.
 *
 * Prints per task the answer check, the shell calls and the file tools used; appends a record per sample to
 * `target/evals/file-tools.jsonl` (`SONESS_EVAL_LABEL`). `SONESS_EVAL_SAMPLES` (default 1). Paid (a decision model and
 * a writer): only with `SONESS_EVAL_FILE_TOOLS=true`.
 */
object FileToolsEvalSpec extends ZIOSpecDefault:

  private val on = if sys.env.get("SONESS_EVAL_FILE_TOOLS").contains("true") then TestAspect.identity else TestAspect.ignore

  private val day = 86_400_000L
  private val t0 = 1_793_000_000_000L

  /** A log of `n` lines, `errors` of them ERROR lines, ending with `last`. */
  private def log(n: Int, errors: Int, last: String): String =
    ((1 until n).map(i => if i <= errors then s"2026-10-0${1 + i % 9} ERROR step $i failed" else s"2026-10-01 INFO step $i ok") :+ last)
      .mkString("", "\n", "\n")

  /** The fixture: sources, three debug logs (the newest isn't the biggest or first by name), a test report, a big build file. */
  private val files: List[(String, String, Long)] = List(
    ("build.sbt", "scalaVersion := \"3.7.0\"\n", t0),
    ("src/main/scala/shop/Main.scala", "package shop\n\n@main def run(): Unit = println(\"shop\")\n", t0),
    ("target/logs/app-2026-10-28.log", log(1200, 3, "2026-10-28 INFO shutdown complete"), t0 - 3 * day),
    ("target/logs/app-2026-10-30.log", log(450, 7, "2026-10-30 WARN disk almost full on /var"), t0 - day),
    ("target/logs/app-2026-10-29.log", log(800, 1, "2026-10-29 INFO shutdown complete"), t0 - 2 * day),
    ("target/test-reports/TEST-shop.CheckoutSpec.xml",
      """<testsuite name="shop.CheckoutSpec" tests="3" failures="1">
        |  <testcase name="applies SAVE10" classname="shop.CheckoutSpec"/>
        |  <testcase name="charges shipping under 50" classname="shop.CheckoutSpec">
        |    <failure message="expected 5.0 but got 0.0">assertion failed</failure>
        |  </testcase>
        |  <testcase name="free shipping over 50" classname="shop.CheckoutSpec"/>
        |</testsuite>
        |""".stripMargin, t0),
    ("target/scala-3.7.0/shop.jar", "x" * 300_000, t0),
    ("target/scala-3.7.0/classes/shop/Main.class", "y" * 2_000, t0),
  )

  /** A task: its request, and what its reply must say. */
  final case class FileTask(name: String, prompt: String, answered: String => Boolean)

  val tasks: List[FileTask] = List(
    FileTask("newest log", "Which log in target/logs is the most recent one, and how many lines does it have?",
      a => a.contains("app-2026-10-30.log") && a.contains("450")),
    FileTask("end of a log", "What's the last line of the most recent log in target/logs?",
      a => a.toLowerCase.contains("disk almost full")),
    FileTask("count in a log", "How many ERROR lines are in target/logs/app-2026-10-28.log?",
      a => """\b3\b""".r.findFirstIn(a).isDefined),
    FileTask("failed test", "The last test run had a failure. Which test failed, and with what message? The reports are under target/.",
      a => a.contains("charges shipping under 50") && a.contains("expected 5.0 but got 0.0")),
    FileTask("largest build file", "What's the largest file under target/, and roughly how big is it?",
      a => a.contains("shop.jar")),
  )

  /** A shell call that does what a file tool does. */
  private val FileCommand = """(^|[;&|(]\s*|\s)(ls|cat|grep|egrep|rg|find|tail|head|wc|du|stat|sort|awk|sed)\b""".r

  final case class Sample(task: String, answered: Boolean, shellCalls: Int, fileShellCalls: Int, tools: List[String],
      answer: String, ms: Long)

  private def project: ZIO[Scope, Throwable, Path] =
    for
      root <- ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("file-tools")))(d =>
                ZIO.attemptBlocking(Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).ignore)
      _    <- ZIO.attemptBlocking:
                files.foreach: (f, text, at) =>
                  val p = root.resolve(f)
                  Files.createDirectories(p.getParent)
                  Files.writeString(p, text)
                  Files.setLastModifiedTime(p, FileTime.fromMillis(at))
    yield root.toRealPath()

  private def sample(task: FileTask, n: Int): ZIO[Client, Throwable, Sample] =
    ZIO.scoped:
      for
        root     <- project
        catalog  <- Soness.catalog.mapError(IllegalStateException(_))
        (agent, text) <- GoalRun.connect(catalog, allow = true)
        started  <- Clock.instant
        result   <- GoalRun.attempt(agent, text, root, task.prompt).either
        ended    <- Clock.instant
        events   <- EventLog.at(root).read
        calls     = events.collect { case c: EventLog.Event.ToolCalled => c }
        shells    = calls.filter(c => Set("run_command", "run_terminal_command")(c.tool))
        fileShell = shells.count(c => c.arguments.fromJson[Json.Obj].toOption.flatMap(_.get("command")).flatMap(_.asString)
                      .exists(FileCommand.findFirstIn(_).isDefined))
        answer    = result.fold(e => s"error: ${e.getMessage}", _._2)
        s         = Sample(task.name, task.answered(answer), shells.size, fileShell, calls.map(_.tool),
                      answer, java.time.Duration.between(started, ended).toMillis)
        _        <- Console.printLine(f"${if s.answered && s.shellCalls == 0 then "PASS" else "FAIL"} ${task.name}%-20s #$n " +
                      f"answer ${if s.answered then "✓" else "✗"}  shell calls ${s.shellCalls} (${s.fileShellCalls} file tasks)  " +
                      f"tools ${s.tools.mkString(", ")}  ${s.ms / 1000}s")
        _        <- ZIO.attemptBlocking:
                      Files.createDirectories(Path.of("target/evals"))
                      Files.writeString(Path.of("target/evals/file-tools.jsonl"), Json.Obj(
                        "at" -> Json.Str(java.time.Instant.now.toString), "task" -> Json.Str(task.name),
                        "label" -> sys.env.get("SONESS_EVAL_LABEL").fold[Json](Json.Null)(Json.Str(_)),
                        "answered" -> Json.Bool(s.answered), "shellCalls" -> Json.Num(s.shellCalls),
                        "fileShellCalls" -> Json.Num(s.fileShellCalls), "tools" -> Json.Arr(Chunk.from(s.tools.map(Json.Str(_)))),
                        "ms" -> Json.Num(s.ms), "answer" -> Json.Str(answer.take(3000))).toJson + "\n",
                        StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      yield s

  def spec = suite("file tools")(
    test("the fixture's answers are as the tasks say") {
      for
        _    <- ZIO.unit
        root <- ZIO.scoped(project.map(r => Files.exists(r.resolve("target/logs/app-2026-10-30.log"))))
      yield assertTrue(root, tasks.size == 5,
        tasks.head.answered("app-2026-10-30.log has 450 lines"), !tasks.head.answered("app-2026-10-28.log has 1200 lines"),
        FileCommand.findFirstIn("ls -lt target/logs").isDefined, FileCommand.findFirstIn("cd x && tail -1 a.log").isDefined,
        FileCommand.findFirstIn("sbt test").isEmpty)
    },
    test("file tasks outside the sources: answered right, with the file tools, not the shell") {
      val samples = sys.env.get("SONESS_EVAL_SAMPLES").flatMap(_.toIntOption).getOrElse(1)
      for
        results <- ZIO.foreach(for t <- tasks; n <- 1 to samples yield (t, n))((t, n) => sample(t, n))
        right    = results.count(_.answered)
        native   = results.count(r => r.answered && r.shellCalls == 0)
        _       <- Console.printLine(s"file tools: ${results.size} samples, $right answered right, $native right without the shell; " +
                     s"shell calls ${results.map(_.shellCalls).sum} (${results.map(_.fileShellCalls).sum} doing file tasks)")
      yield assertTrue(results.nonEmpty)
    } @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(90.minutes) @@ on,
  ).provideShared(Client.default)
