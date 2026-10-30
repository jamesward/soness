package com.jamesward.soness

import zio.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Tools on soness's own transcripts (its debug logs, `--debug`), so a session can see what another did:
 * the turns in a log, one turn's diagnosis and timeline, one call's arguments and result whole, and a search of the
 * log's entries. Read-only. And `watch_log`, which starts a managed process that sends the turns of a log, as they end,
 * through the harness ([[SonessAgent]] supplies what it does).
 */
object TranscriptTools:

  private def schema(required: List[String], properties: (String, String, String)*): Json.Obj =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(properties.map((n, t, d) => n -> (Json.Obj("type" -> Json.Str(t), "description" -> Json.Str(d)): Json))*),
      "required" -> Json.Arr(required.map(Json.Str(_))*),
    )

  private val readOnly = Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))
  private val log = ("log", "string", "The soness debug log's path")

  val Turns = ToolSpec("transcript_turns", "Lists the turns in a soness debug log: each turn's id (session/task), request, " +
    "ending, time, calls and failed calls, newest last.", schema(List("log"), log), None, readOnly)
  val Turn = ToolSpec("transcript_turn", "Shows one turn of a soness debug log: its request, a diagnosis (what failed, repeated, " +
    "ran slow, was corrected), its answer, and its timeline of tool calls and notes, with times from its start.",
    schema(List("log", "turn"), log, ("turn", "string", "The turn's id (session/task, as transcript_turns lists it), or `last`")),
    None, readOnly)
  val CallTool = ToolSpec("transcript_call", "Shows one tool call of a turn whole: its arguments and its result or error.",
    schema(List("log", "turn", "call"), log, ("turn", "string", "The turn's id, or `last`"), ("call", "string", "The call's id (call-N)")),
    None, readOnly)
  val Search = ToolSpec("transcript_search", "Searches a soness debug log's entries for a regular expression (Jev and LLM " +
    "exchanges, warnings, any message): each matching entry, cut, with its time, level, session and loop.",
    schema(List("log", "pattern"), log, ("pattern", "string", "The regular expression (Java syntax)"),
      ("turn", "string", "Only this turn's session")), None, readOnly)

  val WatchLog = ToolSpec("watch_log", "Starts watching a soness debug log (one soness writes with --debug, in its build's target/logs): a " +
    "managed process that, each time a turn in it ends, has that turn judged against the instructions and, when it " +
    "shows something to act on, works on it as a task of this session (with the transcript tools). It returns at once " +
    "and runs until stopped (/stop with its id, or a request to stop watching). For requests like \"watch /tmp/soness.log " +
    "and improve soness from what you see\".",
    schema(List("log", "instructions"), log, ("instructions", "string", "What to do with what's seen, in the user's words")),
    None)

  val Reading: Chunk[ToolSpec] = Chunk(Turns, Turn, CallTool, Search)

  /** The transcript tools, and `watch_log` calling `watch(path, instructions)` (when given). */
  def toolset(root: Option[Path], watch: Option[(Path, String) => Task[String]]): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = Reading ++ watch.map(_ => WatchLog)
      def call(name: String, arguments: Json.Obj): Task[Json] =
        def str(k: String) = arguments.get(k).flatMap(_.asString).filter(_.trim.nonEmpty)
        def need(k: String) = ZIO.fromOption(str(k)).orElseFail(IllegalArgumentException(s"$k is required"))
        val file = need("log").map(p => root.fold(Path.of(p))(_.resolve(p)).toAbsolutePath.normalize)
        def turns(f: Path) = ZIO.attemptBlocking(Files.lines(f, UTF_8)).acquireReleaseWithAuto(lines => ZIO.attemptBlocking(Transcript.turns(lines.iterator.asScala)))
        def turn(f: Path) = need("turn").flatMap: id =>
          turns(f).flatMap(ts => ZIO.fromOption(if id == "last" then ts.findLast(_.endedAt.isDefined).orElse(ts.lastOption) else ts.find(_.id == id))
            .orElseFail(IllegalArgumentException(s"no turn $id in the log; transcript_turns lists them")))
        name match
          case "transcript_turns" =>
            file.flatMap(turns).map: ts =>
              Json.Obj("turns" -> Json.Arr(ts.takeRight(50).map(t => Json.Str(
                s"${t.id}: ${t.ending}, ${t.wallMs.fold("running")(ms => s"${ms / 1000}s")}, ${t.calls.size} calls (${t.calls.count(_.failed)} failed): " +
                  Transcript.cut(t.request, 160)))*), "count" -> Json.Num(ts.size))
          case "transcript_turn" => file.flatMap(turn).map(t => Json.Obj("text" -> Json.Str(Transcript.render(t))))
          case "transcript_call" =>
            (file.flatMap(turn) <*> need("call")).flatMap: (t, id) =>
              ZIO.fromOption(t.calls.find(_.id == id)).orElseFail(IllegalArgumentException(s"no $id in turn ${t.id}")).map: c =>
                Json.Obj("tool" -> Json.Str(c.tool), "loop" -> Json.Str(c.loop.getOrElse("")), "arguments" -> Json.Str(c.arguments.take(20000)),
                  "ms" -> c.ms.fold(Json.Null: Json)(Json.Num(_)),
                  "result" -> Json.Str(c.result.fold("(no result: still running)")(_.fold("FAILED: " + _, identity)).take(20000)))
          case "transcript_search" =>
            for
              f     <- file
              p     <- need("pattern")
              regex <- ZIO.attempt(java.util.regex.Pattern.compile(p)).mapError(e => IllegalArgumentException(s"bad pattern: ${e.getMessage}"))
              only   = str("turn").map(_.takeWhile(_ != '/'))
              hits  <- ZIO.attemptBlocking(Files.lines(f, UTF_8)).acquireReleaseWithAuto: lines =>
                         ZIO.attemptBlocking(Transcript.records(lines.iterator.asScala)
                           .filter(r => only.forall(s => r.session.exists(_.startsWith(s))) && regex.matcher(r.message).find())
                           .take(50).map(r => s"${r.at} ${r.level} ${r.session.map(_.take(8)).getOrElse("-")} ${r.loop.getOrElse("")}: ${Transcript.cut(r.message, 1200)}").toList)
            yield Json.Obj("matches" -> Json.Arr(hits.map(Json.Str(_))*), "count" -> Json.Num(hits.size))
          case "watch_log" if watch.isDefined =>
            (file <*> need("instructions")).flatMap((f, i) => watch.get(f, i)).map(text => Json.Obj("text" -> Json.Str(text)))
          case other => ZIO.fail(IllegalArgumentException(s"unknown tool: $other"))
