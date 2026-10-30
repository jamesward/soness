package com.jamesward.soness

import zio.*
import zio.test.*

import java.nio.file.{Files, Path, StandardOpenOption}

/** A soness debug log read back as turns, diagnosed; and followed as it's written. */
object TranscriptSpec extends ZIOSpecDefault:

  private def fixture(name: String): Iterator[String] =
    scala.io.Source.fromResource(s"transcripts/$name").getLines()

  private val S1 = "11111111-aaaa-4bbb-8ccc-000000000001"
  private val S2 = "22222222-aaaa-4bbb-8ccc-000000000002"
  private var clock = 0
  private def line(level: String, message: String, session: String, loop: Option[String] = None): String =
    clock += 1
    f"timestamp=2026-10-18T00:00:$clock%02d.000000000-06 level=$level thread=zio-fiber-1 message=\"$message\" session=$session" +
      loop.fold("")(l => s" loop=$l")

  /** Two sessions' turns interleaved: one with an unapplied patch, a failure, repeats, a multi-line answer; one failed. */
  private val synthetic: List[String] =
    clock = 0
    val l1 = Some("11111111/task-1/scenario2 (again)")
    List(
      line("INFO", s"session $S1 in /work/shop: 1 MCP servers, 3 tools", "-"),
      line("DEBUG", "prompt (task 1, model m, mode auto): Fix\nthe bug", S1),
      line("DEBUG", "prompt (task 1, model m, mode auto): Other", S2),
      line("DEBUG", """tool call call-1 apply_patch: {"input":"*** Begin Patch"}""", S1, l1),
      line("DEBUG", """tool call call-1 apply_patch returned in 4ms: {"text":"0 out of 1 operations applied."}""", S1, l1),
      line("DEBUG", """tool call call-2 read_file: {"file_path":"A.java"}""", S1, l1),
      line("DEBUG", """tool call call-2 read_file returned in 3ms: {"text":"L1: class A"}""", S1, l1),
      line("DEBUG", """tool call call-3 read_file: {"file_path":"A.java"}""", S1, l1),
      line("DEBUG", """tool call call-3 read_file failed: tool 'read_file' failed: offset exceeds file length""", S1, l1),
      line("INFO", "course correction: rethink after step 3", S1, l1),
      line("DEBUG", "Jev exchange in 300ms: request={} response={}", S1, l1),
      line("DEBUG", "Bedrock m exchange in 2000ms: request={} response={}", S1, l1),
      line("WARN", "prompt turn of " + S2 + " failed\" cause=\"java.lang.RuntimeException: boom\n\tat x.y(Z.scala:1)", S2),
      line("DEBUG", "answer: Fixed it.\nIn A.java.", S1),
      line("DEBUG", """turn metrics: {"model":"m","ending":"answered","latencyMs":13000}""", S1),
    )

  def spec = suite("transcripts")(
    test("a real debug log: the turn, its calls with results, Jev and LLM exchanges, its notes and ending") {
      val turns = Transcript.turns(fixture("katas-turn.log"))
      val t = turns.head
      assertTrue(
        turns.size == 1, t.id == "5e9302ff/task-1", t.request.startsWith("Implement the six katas"), t.ending == "answered",
        t.calls.size == 8, t.calls.forall(_.result.isDefined), t.jevCalls >= 20, t.llmCalls >= 15,
        t.events.exists(_.text.contains("the reply read as planning")), t.answer.nonEmpty,
        Transcript.diagnose(t).exists(_.contains("1 replies rewritten")),
        Transcript.render(t).contains("[gather]"),
      )
    },
    test("interleaved sessions; a loop name with a space; a patch that applied nothing, a failure, a repeat; a failed turn") {
      val turns = Transcript.turns(synthetic.iterator.flatMap(_.split("\n")))
      val one = turns.find(_.session == S1).get
      val two = turns.find(_.session == S2).get
      val found = Transcript.diagnose(one)
      assertTrue(
        one.request == "Fix\nthe bug", one.project.contains("/work/shop"), two.project.isEmpty, one.answer.contains("Fixed it.\nIn A.java."), one.ending == "answered",
        one.calls.map(_.loop) == Vector.fill(3)(Some("11111111/task-1/scenario2 (again)")),
        found.exists(_.startsWith("1 apply_patch calls applied nothing")),
        found.exists(_.contains("read_file ×1 (e.g. tool 'read_file' failed: offset exceeds")),
        found.exists(_.contains("identical calls repeated: read_file ×2")),
        found.exists(_.contains("1 course corrections: rethink after step 3")),
        two.ending == "failed", two.calls.isEmpty, Transcript.diagnose(two).head.startsWith("the turn failed"),
      )
    },
    test("timestamps as zio-logging writes them: hour offsets, a stray colon, UTC") {
      assertTrue(
        Transcript.instant("2026-10-18T08:55:28.02089301-06:").contains(java.time.Instant.parse("2026-10-18T14:55:28.020893010Z")),
        Transcript.instant("2026-10-18T00:55:01.059806828-06").contains(java.time.Instant.parse("2026-10-18T06:55:01.059806828Z")),
        Transcript.instant("2026-10-18T06:55:01Z").contains(java.time.Instant.parse("2026-10-18T06:55:01Z")),
        Transcript.instant("2026-10-18T06:55:01.5+05:30").contains(java.time.Instant.parse("2026-10-18T01:25:01.5Z")),
        Transcript.instant("2026-10-18T12:30:57.7630345-06:0").contains(java.time.Instant.parse("2026-10-18T18:30:57.763034500Z")),
        Transcript.instant("2026-10-18T12:30:57.7630345-06:00").contains(java.time.Instant.parse("2026-10-18T18:30:57.763034500Z")),
        Transcript.instant("nope").isEmpty,
      )
    },
    test("a log being written: each turn comes when it ends, the last entry once the log is quiet") {
      for
        dir   <- ZIO.attemptBlocking(Files.createTempDirectory("watch"))
        file   = dir.resolve("soness.log")
        _     <- ZIO.attemptBlocking(Files.writeString(file, "timestamp=x level=INFO thread=t message=\"before\"\n"))
        ended <- Queue.unbounded[Transcript.Turn]
        _     <- Watch.endedTurns(file).runForeach(ended.offer).forkScoped
        _     <- ZIO.sleep(500.millis)
        write  = (text: String) => ZIO.attemptBlocking(Files.writeString(file, text + "\n", StandardOpenOption.APPEND))
        _     <- ZIO.foreachDiscard(synthetic.take(4))(write)
        none  <- ended.size
        _     <- ZIO.foreachDiscard(synthetic.drop(4))(write)
        first <- ended.take.timeout(10.seconds)
        next  <- ended.take.timeout(10.seconds)
      yield assertTrue(
        none == 0,
        Set(first, next).flatten.map(_.session) == Set(S1, S2),
        (first.toList ++ next).find(_.session == S1).exists(_.calls.size == 3),
      )
    } @@ TestAspect.withLiveClock,
  ) @@ TestAspect.timeout(60.seconds)
