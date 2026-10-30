package com.jamesward.soness.dev

import com.jamesward.soness.*
import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import zio.*
import zio.http.Client
import zio.test.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant

/** The goal run's parts: its arguments, its telemetry, and the goal then the improvement, against a stub harness. */
object GoalRunSpec extends ZIOSpecDefault:

  private val cwd = Path.of("/work/app")
  private val Asked = "Improve soness (this project, the agent harness) from one real run of it."
  private val home = Path.of("/work/soness")

  /** A harness that answers each prompt by its first line, recording where and what it was asked. */
  private final class Recording(asked: Ref[Vector[String]]) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String], mode: HarnessMode,
        steering: UIO[List[String]]): Task[HarnessResult] =
      asked.update(_ :+ prompt).as(HarnessResult(s"did: ${prompt.linesIterator.next()}", ModelUsage(llmCalls = 1, jevCalls = 2)))
    override def judge(situation: String, question: String, yes: String, no: String): Task[Double] = ZIO.succeed(0.25)

  /** Steps that record the rounds and judge each a change that passes. */
  private final class Recorded(dir: Path, made: Ref[Vector[String]]) extends Improve.Steps:
    def prepare(o: Improve.Options, n: Int, log: Option[Path], history: List[String]): Task[Improve.Round] =
      val out = dir.resolve(s"r$n")
      made.update(_ :+ s"prepare $n history=${history.size}") *> ZIO.attemptBlocking(Files.createDirectories(out))
        .as(Improve.Round(n, out, dir, "the observer's prompt"))
    def gate(o: Improve.Options, round: Improve.Round): Task[Improve.Verdict] =
      made.update(_ :+ s"gate ${round.round} ${round.prompt.linesIterator.next()}").as(Improve.Verdict("tests pass (no eval)", files = List("src/main/scala/A.scala")))

  private def call(id: String, tool: String, start: Long, ms: Long, args: String = "{}", failed: Boolean = false) =
    Transcript.Call(id, tool, None, args, Instant.ofEpochMilli(start), Some(ms), Some(if failed then Left("no") else Right("ok")))

  def spec = suite("goal run")(
    test("the goal is the words; options set the project, soness, rounds and gates; bad ones are errors") {
      assertTrue(
        GoalRun.parse(List("fix", "the", "bug"), cwd, home) ==
          Right(GoalRun.Args("fix the bug", cwd, home, 1, improve = true, eval = false, allow = false)),
        GoalRun.parse(List("--project", "../other", "--rounds", "3", "--eval", "--allow", "add", "tests"), cwd, home) ==
          Right(GoalRun.Args("add tests", Path.of("/work/other"), home, 3, improve = true, eval = true, allow = true)),
        GoalRun.parse(List("--no-improve", "x"), cwd, home).exists(!_.improve),
        GoalRun.parse(Nil, cwd, home).left.exists(_.contains("no goal")),
        GoalRun.parse(List("--rounds", "0", "x"), cwd, home).left.exists(_.contains("--rounds")),
        GoalRun.parse(List("--wat", "x"), cwd, home).left.exists(_.contains("unknown option --wat")),
      )
    },
    test("telemetry: the calls' busy time and overlap, parallelism, idle time, failures and repeats; the metrics' tokens") {
      // a 0–1s and a 0.5–2s call overlap (busy 2s, 2 at once); a 5–6s call alone; one repeats another, one failed
      val turn = Transcript.Turn("s", 1, "goal", Instant.ofEpochMilli(0), Some(Instant.ofEpochMilli(10000)), calls = Vector(
        call("c1", "read_file", 0, 1000, """{"p":"A"}"""), call("c2", "search_text", 500, 1500),
        call("c3", "read_file", 5000, 1000, """{"p":"A"}"""), call("c4", "run_command", 6000, 0, failed = true)))
      val metrics = TurnMetrics("m", "answered", 10000, 4, 2, 300, 40, 5, 9000, 0, 200, 3000)
      val t = GoalRun.telemetry(turn, Some(metrics))
      assertTrue(t.toolBusyMs == 3000, t.toolMs == 3500, t.maxConcurrent == 2, t.idleMs == 7000,
        math.abs(t.parallelism - 3500.0 / 3000) < 1e-9, t.failedCalls == 1, t.repeatedCalls == 1,
        t.jevCalls == 5, t.jevInputTokens == 9000, t.llmInputTokens == 300, t.wallMs == 10000,
        t.describe.contains("at most 2 at once"))
    },
    test("the goal is worked on in its project, then soness on itself with the run's telemetry, DESIGN.md and the aims; the gates judge it") {
      for
        root    <- ZIO.attemptBlocking(Files.createTempDirectory("goal-run"))
        project  = root.resolve("app")
        soness   = root.resolve("soness")
        _       <- ZIO.attemptBlocking { Files.createDirectories(project); Files.createDirectories(soness)
                     Files.writeString(soness.resolve("DESIGN.md"), "- Parellize everything possible", UTF_8) }
        asked   <- Ref.make(Vector.empty[String])
        made    <- Ref.make(Vector.empty[String])
        sessions <- Ref.make(Vector.empty[String])
        catalog  = HarnessCatalog.make(List("m" -> Recording(asked)), "m", HarnessMode.Single).toOption.get
        turn     = Transcript.Turn("s", 1, "fix it", Instant.ofEpochMilli(0), Some(Instant.ofEpochMilli(4000)),
                     calls = Vector(call("c1", "read_file", 0, 1000), call("c2", "read_file", 2000, 1000)))
        args     = GoalRun.Args("fix the checkout bug", project, soness, 2, improve = true, eval = false, allow = false)
        out      = root.resolve("record")
        summary <- GoalRun.runWith(args, catalog, Recorded(root.resolve("improve"), made), None, out,
                     (_, s) => sessions.update(_ :+ s).as(Some(turn)))
        prompts <- asked.get
        steps   <- made.get
        goal    <- ZIO.attemptBlocking(Files.readString(out.resolve("goal.json"), UTF_8))
        record  <- ZIO.attemptBlocking(Files.exists(out.resolve("transcript.txt")) && Files.exists(out.resolve("summary.txt")))
      yield
        val improvement = prompts.drop(1)
        assertTrue(
          // the goal itself first, as asked
          prompts.head == "fix the checkout bug",
          // then each round on soness, with how the goal went: its ending, judgment, telemetry, design and aims
          improvement.size == 2,
          improvement.forall(p => p.contains("soness was given this goal") && p.contains("fix the checkout bug") &&
            p.contains("p 0.25") && p.contains("tools: 2 calls") && p.contains("parallelism") &&
            p.contains("Parellize everything possible") && p.contains("token efficiency") && p.contains("time efficiency") &&
            p.contains("alignment with DESIGN.md")),
          improvement.head.contains("round 1"), improvement(1).contains("round 2") && improvement(1).contains("Earlier rounds: tests pass"),
          // the gates judge the round's request (not the observer's), each round after its work
          steps == Vector("prepare 1 history=0", s"gate 1 $Asked", "prepare 2 history=1", s"gate 2 $Asked"),
          goal.contains("\"accomplished\" : 0.25"), goal.contains("\"telemetry\""), record,
          summary.contains("Improvement rounds:\n1. tests pass (no eval)"),
        )
    } @@ TestAspect.withLiveClock,
    test("--no-improve: the goal only, its record, no round") {
      for
        root    <- ZIO.attemptBlocking(Files.createTempDirectory("goal-run"))
        asked   <- Ref.make(Vector.empty[String])
        made    <- Ref.make(Vector.empty[String])
        catalog  = HarnessCatalog.make(List("m" -> Recording(asked)), "m", HarnessMode.Single).toOption.get
        args     = GoalRun.Args("say hello", root, root, 1, improve = false, eval = false, allow = false)
        summary <- GoalRun.runWith(args, catalog, Recorded(root.resolve("improve"), made), None, root.resolve("record"), (_, _) => ZIO.none)
        prompts <- asked.get
        steps   <- made.get
      yield assertTrue(prompts == Vector("say hello"), steps.isEmpty, summary.contains("did: say hello"),
        !summary.contains("Improvement rounds"))
    } @@ TestAspect.withLiveClock,
  ).provideSomeShared[Scope](Client.default)
