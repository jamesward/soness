package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

/**
 * soness driven as IntelliJ IDEA 2026.2.3 drives it, for the live evals: launched as a subprocess (one Bedrock model,
 * `BEDROCK_MODEL_ID`), initialized with IntelliJ's captured `initialize`, and given each prompt in a fresh session made
 * from IntelliJ's own `session/new` and `session/prompt` (`intellij-idea-2026.2.3-ide-session.jsonl`), with IntelliJ's
 * skill views as the skill root and IntelliJ's MCP server: the real IDE, run headless, when `SONESS_IJ_MCP_PORT`,
 * `SONESS_IJ_PROJECT` and `SONESS_IJ_IDEA` are set (see AGENTS.md), else the mock (`IntellijMcpFixture`).
 * `SONESS_IJ_SKILL_ROOT` replaces the skill root.
 */
object IntellijSession:

  /** The IDE MCP server a session gets: the real IDE on `port` (its project fixed), or the mock over each project. */
  final case class Ide(port: Option[String], project: Option[Path], idea: Option[String]):
    def real: Boolean = port.isDefined
    def describe: String = port.fold("the mock")(p => s"IntelliJ IDEA on $p, ${project.getOrElse("?")}")

  val ide: UIO[Ide] =
    for
      port    <- System.env("SONESS_IJ_MCP_PORT").orDie
      project <- System.env("SONESS_IJ_PROJECT").orDie
      idea    <- System.env("SONESS_IJ_IDEA").orDie
    yield if port.isDefined && project.isDefined && idea.isDefined then Ide(port, project.map(Path.of(_)), idea) else Ide(None, None, None)

  /** One tool call of a turn, as its updates reported it: the tool, its input and output, and how it ended. */
  /** A tool call; `startMs` and `durationMs` (since the turn started) when the client observed them, else -1. */
  final case class Call(tool: String, input: String, status: String, output: String, startMs: Long = -1, durationMs: Long = -1,
      loop: Option[String] = None)

  /** A prompt turn: its tool calls, its answer (or error), and its `_meta.soness` metrics. */
  final case class Turn(calls: List[Call], answer: String, error: Option[String], metrics: Option[Json]):
    def metric(name: String): Option[Json] = metrics.flatMap(field(_, name))
    def ending: Option[String] = metric("ending").flatMap(_.asString)
    def failedCalls: Int = calls.count(_.status == "failed")

  /** soness, initialized, starting a session per prompt. */
  final class Driver(peer: Peer, newSession: String, promptLine: String, ids: Ref[Int], ide: Ide, skillRoot: String):

    /** See [[interjected]]. */
    def both(first: String, second: String, project: Path, afterCalls: Int, timeout: Duration): Task[Interjected] =
      def isResponse(id: Int)(m: Json.Obj) = m.get("method").isEmpty && m.get("id").contains(Json.Num(id))
      def prompt(text: String, sid: String) =
        ids.updateAndGet(_ + 1).tap: pid =>
          peer.send(promptLine.replace("${ID}", pid.toString).replace("${SESSION_ID}", sid)
            .replace("${TEXT}", Json.Str(text).toJson.drop(1).dropRight(1)).replace("${SKILL_ROOT}", skillRoot))
      for
        sid    <- open(project)
        a      <- prompt(first, sid)
        early  <- peer.collect(ms => ms.exists(isResponse(a)) ||
                    ms.count(m => str(m, "params", "update", "sessionUpdate").contains("tool_call")) >= afterCalls, timeout)
        b      <- prompt(second, sid)
        late   <- peer.collect(ms => (early ++ ms).exists(isResponse(a)) && ms.exists(isResponse(b)), timeout)
        all     = early ++ late
        _      <- close(sid)
      yield Interjected(all, all.find(isResponse(a)), all.find(isResponse(b)))

    /**
     * Two prompts in two sessions of one project, at the same time (two chats of one IDE): every message received, and
     * the two responses.
     */
    def twoSessions(first: String, second: String, project: Path, timeout: Duration): Task[Interjected] =
      def isResponse(id: Int)(m: Json.Obj) = m.get("method").isEmpty && m.get("id").contains(Json.Num(id))
      def prompt(text: String, sid: String) =
        ids.updateAndGet(_ + 1).tap: pid =>
          peer.send(promptLine.replace("${ID}", pid.toString).replace("${SESSION_ID}", sid)
            .replace("${TEXT}", Json.Str(text).toJson.drop(1).dropRight(1)).replace("${SKILL_ROOT}", skillRoot))
      for
        a      <- open(project)
        b      <- open(project)
        pa     <- prompt(first, a)
        pb     <- prompt(second, b)
        all    <- peer.collect(ms => ms.exists(isResponse(pa)) && ms.exists(isResponse(pb)), timeout)
        _      <- close(a) *> close(b)
      yield Interjected(all, all.find(isResponse(pa)), all.find(isResponse(pb)))

    private def open(project: Path): Task[String] =
      for
        javaHome  <- property("java.home")
        classpath <- property("soness.testClasspath")
        (command, args) = ide.idea match
                            case Some(idea) => (idea, Json.Arr(Json.Str("stdioMcpServer")).toJson)
                            case None => (s"$javaHome/bin/java",
                                Json.Arr(Json.Str("-cp"), Json.Str(classpath), Json.Str("com.jamesward.soness.IntellijMcpFixture")).toJson)
        id        <- ids.updateAndGet(_ + 1)
        _         <- peer.send(ide.port.fold(newSession)(p => newSession.replace("\"64442\"", s"\"$p\""))
                       .replace("${ID}", id.toString).replace("${CWD}", project.toString)
                       .replace("${IDE_COMMAND}", command).replace("${IDE_ARGS}", args).replace("${SKILL_ROOT}", skillRoot))
        created   <- peer.awaitResponse(id, 2.minutes)
      yield str(created._2, "result", "sessionId").getOrElse("")

    private def close(sid: String): Task[Unit] =
      for
        cid <- ids.updateAndGet(_ + 1)
        _   <- peer.send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(cid), "method" -> Json.Str("session/close"),
                 "params" -> Json.Obj("sessionId" -> Json.Str(sid))).toJson)
        _   <- peer.awaitResponse(cid, 1.minute).ignore
      yield ()

    /** `prompt` in a fresh session over `project` (the real IDE's own, with the real IDE). */
    def turn(prompt: String, project: Path, timeout: Duration = 10.minutes): Task[Turn] =
      for
        javaHome  <- property("java.home")
        classpath <- property("soness.testClasspath")
        (command, args) = ide.idea match
                            case Some(idea) => (idea, Json.Arr(Json.Str("stdioMcpServer")).toJson)
                            case None => (s"$javaHome/bin/java",
                                Json.Arr(Json.Str("-cp"), Json.Str(classpath), Json.Str("com.jamesward.soness.IntellijMcpFixture")).toJson)
        id        <- ids.updateAndGet(_ + 1)
        _         <- peer.send(ide.port.fold(newSession)(p => newSession.replace("\"64442\"", s"\"$p\""))
                       .replace("${ID}", id.toString).replace("${CWD}", project.toString)
                       .replace("${IDE_COMMAND}", command).replace("${IDE_ARGS}", args).replace("${SKILL_ROOT}", skillRoot))
        created   <- peer.awaitResponse(id, 2.minutes)
        sid        = str(created._2, "result", "sessionId").getOrElse("")
        pid       <- ids.updateAndGet(_ + 1)
        _         <- peer.send(promptLine.replace("${ID}", pid.toString).replace("${SESSION_ID}", sid)
                       .replace("${TEXT}", Json.Str(prompt).toJson.drop(1).dropRight(1)).replace("${SKILL_ROOT}", skillRoot))
        turn      <- peer.awaitResponse(pid, timeout).either
        cid       <- ids.updateAndGet(_ + 1)
        _         <- peer.send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(cid), "method" -> Json.Str("session/close"),
                       "params" -> Json.Obj("sessionId" -> Json.Str(sid))).toJson)
        _         <- peer.awaitResponse(cid, 1.minute).ignore
      yield turn match
        case Left(error) => Turn(Nil, "", Some(s"no answer: ${error.getMessage.take(300)}"), None)
        case Right((updates, response)) =>
          val toolUpdates = updates.flatMap(u => field(u, "params", "update")).filter: u =>
            str(u, "sessionUpdate").exists(s => s == "tool_call" || s == "tool_call_update")
          val started = toolUpdates.filter(u => str(u, "sessionUpdate").contains("tool_call"))
          val calls = started.map: start =>
            val callId = str(start, "toolCallId")
            val end = toolUpdates.findLast(u => str(u, "sessionUpdate").contains("tool_call_update") && str(u, "toolCallId") == callId)
            Call(
              str(start, "title").getOrElse("?"),
              field(start, "rawInput").map(_.toJson).getOrElse(""),
              end.flatMap(str(_, "status")).getOrElse("?"),
              end.flatMap(field(_, "rawOutput")).map(j => j.asString.getOrElse(j.toJson)).getOrElse(""),
            )
          Turn(calls, answer(updates), response.get("error").map(_.toJson), field(response, "result", "_meta", "soness"))

  /**
   * Two prompts in one session, the second sent while the first runs (after its `afterCalls`-th tool call, as a user
   * would interject), both answered: every message received, and the two responses.
   */
  final case class Interjected(messages: List[Json.Obj], first: Option[Json.Obj], second: Option[Json.Obj]):
    private def updates = messages.flatMap(m => field(m, "params", "update"))
    def answers: List[String] = updates.filter(u => str(u, "sessionUpdate").contains("agent_message_chunk")).flatMap(u => str(u, "content", "text"))
    def calls: List[String] = updates.filter(u => str(u, "sessionUpdate").contains("tool_call")).flatMap(u => str(u, "title"))
    def route: Option[String] = second.flatMap(r => str(r, "result", "_meta", "soness", "route"))
    def metric(response: Option[Json.Obj], name: String): Option[Json] = response.flatMap(r => field(r, "result", "_meta", "soness", name))

  extension (driver: Driver)
    def interjected(first: String, second: String, project: Path, afterCalls: Int, timeout: Duration = 10.minutes): Task[Interjected] =
      driver.both(first, second, project, afterCalls, timeout)

  /** soness launched and initialized as IntelliJ IDEA does it, for the life of the scope. */
  /** `log`: a debug log of the driven soness (`--debug`, its file `SONESS_LOG_FILE`). */
  def driver(ide: Ide, log: Option[String] = None): ZIO[Scope, Throwable, Driver] =
    for
      skillRoot <- System.env("SONESS_IJ_SKILL_ROOT").someOrElseZIO:
                     property("user.dir").map(d => Path.of(d, "src/test/resources/intellij-skill-views/other").toString)
      templates <- ZStream.fromResource("intellij-idea-2026.2.3-ide-session.jsonl").via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
                     .filter(_.nonEmpty).runCollect
      // one model, as configured in IntelliJ: SONESS_BEDROCK_MODELS is cleared
      peer      <- launch(Map("SONESS_BEDROCK_MODELS" -> "") ++ log.map("SONESS_LOG_FILE" -> _), log.toList.map(_ => "--debug"))
      lines     <- intellijCapture
      _         <- peer.send(lines(0))
      _         <- peer.awaitResponse(1, 2.minutes)
      ids       <- Ref.make(1)
      _         <- Console.printLine(s"IDE MCP server: ${ide.describe}; skills: $skillRoot")
    yield Driver(peer, templates(0), templates(1), ids, ide, skillRoot)

  /** The metrics every eval record carries, from a turn's `_meta.soness`. */
  val MetricNames: List[String] = List(
    "model", "ending", "latencyMs", "toolCalls", "jevCalls", "jevInputTokens", "jevOutputTokens",
    "llmCalls", "llmInputTokens", "llmOutputTokens", "jevLatencyMs", "llmLatencyMs", "toolLatencyMs", "costUsd",
  )

  /**
   * Appends one record per sample to `target/evals/<eval>.jsonl`, so runs can be compared across orchestration changes:
   * when, the eval, task and arm (the orchestration measured), `SONESS_EVAL_LABEL` (what's being tried), the IDE server, whether it passed and each
   * check, the turn's metrics, and its tool calls.
   */
  def record(eval: String, task: String, ide: Ide, turn: Turn, checks: List[(String, Boolean)], arm: String = "jev",
      extra: Json.Obj = Json.Obj()): Task[Unit] =
    for
      label <- System.env("SONESS_EVAL_LABEL")
      dir   <- property("user.dir").map(d => Path.of(d, "target", "evals"))
      line   = Json.Obj(
                 Chunk[(String, Json)](
                   "at" -> Json.Str(Instant.now.toString),
                   "eval" -> Json.Str(eval),
                   "task" -> Json.Str(task),
                   "arm" -> Json.Str(arm),
                   "label" -> label.fold[Json](Json.Null)(Json.Str(_)),
                   "ide" -> Json.Str(if ide.real then "intellij" else "mock"),
                   "passed" -> Json.Bool(turn.error.isEmpty && checks.forall(_._2)),
                   "checks" -> Json.Obj(Chunk.from(checks.map((name, ok) => name -> Json.Bool(ok)))),
                   "failedCalls" -> Json.Num(turn.failedCalls),
                   "error" -> turn.error.fold[Json](Json.Null)(Json.Str(_)),
                 ) ++ Chunk.from(MetricNames.map(n => n -> turn.metric(n).getOrElse(Json.Null))) :+
                   ("extra" -> extra) :+
                   ("calls" -> Json.Arr(Chunk.from(turn.calls.map(c =>
                     Json.Obj("tool" -> Json.Str(c.tool), "input" -> Json.Str(c.input), "status" -> Json.Str(c.status)))))),
               ).toJson
      _     <- ZIO.attemptBlocking:
                 Files.createDirectories(dir)
                 Files.writeString(dir.resolve(s"$eval.jsonl"), line + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    yield ()

  /** A metric's values across samples, as `mean (min–max)`, or `?` when no sample has it. */
  def summary(turns: Seq[Turn], name: String): String =
    val values = turns.flatMap(_.metric(name)).flatMap(_.asNumber).map(_.value.doubleValue)
    if values.isEmpty then "?"
    else
      val mean = values.sum / values.size
      if values.size == 1 then f"$mean%.0f" else f"$mean%.0f (${values.min}%.0f–${values.max}%.0f)"
