package com.jamesward.soness

import zio.json.*
import zio.json.ast.Json

import java.time.Instant

/**
 * A soness debug log (`--debug`) read back as what happened: its records (one per log entry, a multi-line
 * message included), grouped into turns (a session's task: its request, tool calls with arguments, results and
 * times, Jev and LLM exchanges, course corrections and warnings, its answer and metrics), and a diagnosis of each turn:
 * what failed, repeated, ran slow or went round in circles. Pure: [[Transcript.step]] folds one record at a time, so a
 * log being written is read incrementally ([[Watch]]), and a whole one at once.
 */
object Transcript:

  /** One log entry. */
  final case class Record(at: Instant, level: String, message: String, session: Option[String], loop: Option[String])

  /** A tool call of a turn, as it started and ended. */
  final case class Call(id: String, tool: String, loop: Option[String], arguments: String, startedAt: Instant,
      ms: Option[Long] = None, result: Option[Either[String, String]] = None):
    def failed: Boolean = result.exists(_.isLeft)

  /** A line of a turn's timeline. */
  final case class Event(at: Instant, loop: Option[String], text: String)

  final case class Turn(
    session: String,
    task: Int,
    request: String,
    startedAt: Instant,
    endedAt: Option[Instant] = None,
    calls: Vector[Call] = Vector.empty,
    events: Vector[Event] = Vector.empty,
    jevCalls: Int = 0,
    jevMs: Long = 0,
    llmCalls: Int = 0,
    llmMs: Long = 0,
    warnings: Vector[String] = Vector.empty,
    answer: Option[String] = None,
    metrics: Option[Json.Obj] = None,
    failure: Option[String] = None,
    project: Option[String] = None,
  ):
    /** `session8/task-N`: how loops name it, and how tools refer to it. */
    def id: String = s"${session.take(8)}/task-$task"
    def ending: String =
      failure.map(_ => "failed").orElse(metrics.flatMap(_.get("ending")).flatMap(_.asString)).getOrElse(if endedAt.isEmpty then "running" else "?")
    def wallMs: Option[Long] = endedAt.map(e => java.time.Duration.between(startedAt, e).toMillis)

  /** The fold's state: the turns so far, and each session's open tasks (oldest first). */
  final case class State(turns: Vector[Turn] = Vector.empty, open: Map[String, Vector[Int]] = Map.empty,
      projects: Map[String, String] = Map.empty)

  private val Prompt = """(?s)prompt \(task (\d+), [^)]*\): (.*)""".r
  private val CallStart = """(?s)tool call (call-\d+) ([\w.-]+): (.*)""".r
  private val CallEnd = """(?s)tool call (call-\d+) ([\w.-]+) returned in (\d+)ms: (.*)""".r
  private val CallFail = """(?s)tool call (call-\d+) ([\w.-]+) failed: (.*)""".r
  private val JevExchange = """(?s)Jev exchange in (\d+)ms.*""".r
  private val LlmExchange = """(?s)Bedrock (\S+) exchange in (\d+)ms.*""".r
  private val Answer = """(?s)answer: (.*)""".r
  private val Metrics = """(?s)turn metrics: (.*)""".r
  private val TurnFailed = """(?s)prompt turn of \S+ failed.*""".r
  private val SessionIn = """(?s)session ([0-9a-f-]{36}) in (\S+?): .*""".r
  private val ProjectDirectory = """Project directory: (\S+?)(?:\\n|\n|")""".r.unanchored
  private val TaskOfLoop = """[0-9a-f]{8}/task-(\d+).*""".r

  /** Messages worth a line in a turn's timeline, besides its calls. */
  private val Noted = List("course correction", "the reply read as planning", "background:", "auto:", "parallel:",
    "recalled", "following the project's skills", "validation:", "escalat", "stalled", "stopping", "the planner")

  /** The records of a log's lines: an entry starts with `timestamp=`; the lines that don't continue the one before. */
  def records(lines: Iterator[String]): Iterator[Record] =
    val grouped = lines.foldLeft((Vector.empty[String], Vector.empty[String])):
      case ((done, current), line) if line.startsWith("timestamp=") => (if current.isEmpty then done else done :+ current.mkString("\n"), Vector(line))
      case ((done, current), line) if current.nonEmpty              => (done, current :+ line)
      case (acc, _)                                                 => acc
    (grouped._1 ++ Option.when(grouped._2.nonEmpty)(grouped._2.mkString("\n"))).iterator.flatMap(record)

  /** One entry's record; none for a line that isn't one (`key=value …` with a quoted message). */
  def record(entry: String): Option[Record] =
    for
      at      <- field(entry, "timestamp").flatMap(instant)
      level   <- field(entry, "level")
      start    = entry.indexOf(" message=\"")
      if start >= 0
    yield
      // the message's own quotes aren't escaped: it ends at the last quote before the trailing annotations
      val body = entry.substring(start + 10)
      val end = List(" session=", " location=", " loop=").flatMap(k => Option(body.lastIndexOf("\"" + k)).filter(_ >= 0)).minOption
        .getOrElse(body.lastIndexOf('"').max(0))
      val tail = body.substring(end)
      Record(at, level, body.substring(0, end), annotation(tail, "session"), annotation(tail, "loop"))

  private val Time = """(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?)(Z|[+-]\d\d)?:?(\d{0,2})""".r

  /**
   * A log timestamp: ISO-8601, as zio-logging writes it, which isn't always parseable as such: its offset is cut to
   * any length (`-06`, `-06:`, `-06:0`), a trailing zero of the fraction trimmed off with it.
   */
  def instant(text: String): Option[Instant] = text match
    case Time(local, offset, minutes) =>
      // the offset's minutes cut to any length (`-06`, `-06:`, `-06:0`, `-06:00`): zio-logging trims the timestamp
      val zone = Option(offset).filter(_ != "Z").fold(java.time.ZoneOffset.UTC)(o =>
        java.time.ZoneOffset.of(o + ":" + Option(minutes).getOrElse("").padTo(2, '0')))
      scala.util.Try(java.time.LocalDateTime.parse(local).toInstant(zone)).toOption
    case _ => None

  private def field(entry: String, key: String): Option[String] =
    s"""(?:^| )$key=(\\S+)""".r.findFirstMatchIn(entry.takeWhile(_ != '\n').take(300)).map(_.group(1))

  /** An annotation's value: up to the next ` key=`, so a loop named `scenario2 (again)` is kept whole. */
  private def annotation(tail: String, key: String): Option[String] =
    val at = tail.indexOf(s" $key=")
    Option.when(at >= 0):
      val rest = tail.substring(at + key.length + 2)
      val next = """ \w+=""".r.findFirstMatchIn(rest).map(_.start).getOrElse(rest.length)
      rest.substring(0, next).trim.stripSuffix("\"")
    .filter(_.nonEmpty)

  /** The state after `record`, and the turns it ended. */
  def step(state: State, record: Record): (State, List[Turn]) =
    def taskOf(r: Record): Option[Int] =
      r.loop.collect { case TaskOfLoop(n) => n.toInt }.orElse(r.session.flatMap(s => state.open.get(s).flatMap(_.lastOption)))
    def update(session: String, task: Int)(f: Turn => Turn): State =
      val i = state.turns.lastIndexWhere(t => t.session == session && t.task == task)
      if i < 0 then state else state.copy(turns = state.turns.updated(i, f(state.turns(i))))
    def event(t: Turn, text: String) = t.copy(events = t.events :+ Event(record.at, record.loop, text))
    val session = record.session
    (record.message, session) match
      case (SessionIn(s, cwd), _) => (state.copy(projects = state.projects.updated(s, cwd)), Nil)
      case (Prompt(task, text), Some(s)) =>
        (state.copy(turns = state.turns :+ Turn(s, task.toInt, text.trim, record.at, project = state.projects.get(s)), open = state.open.updated(s, state.open.getOrElse(s, Vector.empty) :+ task.toInt)), Nil)
      case (CallEnd(id, tool, ms, result), Some(s)) =>
        taskOf(record).fold((state, Nil)): task =>
          (update(s, task)(t => event(t.copy(calls = t.calls.map(c => if c.id == id && c.result.isEmpty then c.copy(ms = Some(ms.toLong), result = Some(Right(result))) else c)),
            s"$id $tool returned in ${ms}ms: ${cut(result, 300)}")), Nil)
      case (CallFail(id, tool, error), Some(s)) =>
        taskOf(record).fold((state, Nil)): task =>
          (update(s, task)(t => event(t.copy(calls = t.calls.map(c => if c.id == id && c.result.isEmpty then
            c.copy(ms = Some(java.time.Duration.between(c.startedAt, record.at).toMillis), result = Some(Left(error))) else c)),
            s"$id $tool FAILED: ${cut(error, 300)}")), Nil)
      case (CallStart(id, tool, arguments), Some(s)) =>
        taskOf(record).fold((state, Nil)): task =>
          (update(s, task)(t => event(t.copy(calls = t.calls :+ Call(id, tool, record.loop, arguments, record.at)), s"$id $tool ${cut(arguments, 300)}")), Nil)
      case (JevExchange(ms), Some(s)) =>
        // an older log names no project at the session's start: its requests' context does
        taskOf(record).fold((state, Nil))(task => (update(s, task)(t => t.copy(jevCalls = t.jevCalls + 1, jevMs = t.jevMs + ms.toLong,
          project = t.project.orElse(ProjectDirectory.findFirstMatchIn(record.message).map(_.group(1))))), Nil))
      case (LlmExchange(_, ms), Some(s)) =>
        taskOf(record).fold((state, Nil))(task => (update(s, task)(t => t.copy(llmCalls = t.llmCalls + 1, llmMs = t.llmMs + ms.toLong)), Nil))
      case (Answer(text), Some(s)) =>
        taskOf(record).fold((state, Nil))(task => (update(s, task)(t => t.copy(answer = Some(text.trim))), Nil))
      case (Metrics(json), Some(s)) => end(state, s, record, _.copy(metrics = json.trim.fromJson[Json.Obj].toOption))
      case (TurnFailed(), Some(s))  => end(state, s, record, t => t.copy(failure = Some(cut(record.message, 1500))))
      case (text, Some(s)) if record.level == "WARN" || record.level == "ERROR" =>
        taskOf(record).fold((state, Nil))(task => (update(s, task)(t => event(t.copy(warnings = t.warnings :+ cut(text, 400)), s"WARN ${cut(text, 300)}")), Nil))
      case (text, Some(s)) if Noted.exists(text.startsWith) =>
        taskOf(record).fold((state, Nil))(task => (update(s, task)(t => event(t, cut(text, 300))), Nil))
      case _ => (state, Nil)

  /** The session's oldest open task ends (metrics name no task; a session's tasks end in the order they're answered). */
  private def end(state: State, session: String, record: Record, f: Turn => Turn): (State, List[Turn]) =
    state.open.get(session).flatMap(_.headOption).fold((state, Nil)): task =>
      val i = state.turns.lastIndexWhere(t => t.session == session && t.task == task)
      if i < 0 then (state, Nil)
      else
        val ended = f(state.turns(i)).copy(endedAt = Some(record.at))
        (State(state.turns.updated(i, ended), state.open.updated(session, state.open(session).tail)), List(ended))

  /** Every turn of a whole log, ended or not. */
  def turns(lines: Iterator[String]): Vector[Turn] =
    records(lines).foldLeft(State())((s, r) => step(s, r)._1).turns

  def cut(text: String, max: Int): String =
    val one = text.replace("\n", "⏎")
    if one.length > max then one.take(max) + "…" else one

  /** What a turn shows went wrong or cost time, most telling first; what's there to see is in its timeline. */
  def diagnose(turn: Turn): List[String] =
    val calls = turn.calls
    val failedByTool = calls.filter(_.failed).groupBy(_.tool).toList.sortBy(-_._2.size)
    val repeats = calls.groupBy(c => (c.tool, c.arguments)).values.filter(_.size > 1).toList.sortBy(-_.size)
    val nothingApplied = calls.filter(c => c.tool == "apply_patch" && c.result.exists(_.fold(_.startsWith("0 out of"), r => r.contains("\"0 out of") || r.trim == "0")))
    val slow = calls.filter(_.ms.exists(_ > 60000)).sortBy(-_.ms.getOrElse(0L))
    val runs = calls.filter(c => ToolCallView.kind(c.tool) == com.jamesward.zioacp.schema.ToolKind.Execute)
    val toolMs = calls.flatMap(_.ms).sum
    def noted(prefix: String) = turn.events.count(e => e.text.startsWith(prefix) || e.text.startsWith("WARN " + prefix))
    List(
      turn.failure.map(f => s"the turn failed: ${cut(f, 400)}"),
      Option.when(turn.endedAt.isEmpty)("the turn hasn't ended"),
      Option.when(Set("unable", "budget_exhausted")(turn.ending))(s"the turn ended ${turn.ending}"),
      Option.when(nothingApplied.nonEmpty)(s"${nothingApplied.size} apply_patch calls applied nothing (${nothingApplied.map(_.id).mkString(", ")})"),
      Option.when(failedByTool.nonEmpty)("failed calls: " + failedByTool.map((t, cs) => s"$t ×${cs.size} (e.g. ${cut(cs.head.result.flatMap(_.left.toOption).getOrElse(""), 160)})").mkString("; ")),
      Option.when(repeats.nonEmpty)(s"identical calls repeated: " + repeats.take(5).map(cs => s"${cs.head.tool} ×${cs.size} (${cs.map(_.id).mkString(", ")})").mkString("; ")),
      Option.when(slow.nonEmpty)("slow calls: " + slow.take(5).map(c => s"${c.id} ${c.tool} ${c.ms.get / 1000}s").mkString(", ")),
      Option.when(noted("course correction") > 0)(s"${noted("course correction")} course corrections: " +
        turn.events.filter(_.text.startsWith("course correction")).map(_.text.stripPrefix("course correction: ")).take(6).mkString("; ")),
      Option.when(noted("the reply read as planning") > 0)(s"${noted("the reply read as planning")} replies rewritten (read as planning)"),
      Option.when(turn.warnings.nonEmpty)(s"${turn.warnings.size} warnings: " + turn.warnings.distinct.take(4).map(cut(_, 160)).mkString("; ")),
      Some(s"time: ${turn.wallMs.fold("?")(ms => s"${ms / 1000}s")} wall; Jev ${turn.jevCalls} calls ${turn.jevMs / 1000}s, LLM ${turn.llmCalls} calls " +
        s"${turn.llmMs / 1000}s, tools ${calls.size} calls ${toolMs / 1000}s (${runs.size} runs ${runs.flatMap(_.ms).sum / 1000}s)"),
    ).flatten

  /**
   * `turn` with `call` added, its start and end in the timeline as a log's would be: a turn recorded as it runs (the
   * session's shared state), not read back from a log.
   */
  def withCall(turn: Turn, call: Call, endedAt: Instant): Turn =
    val end = call.result.map:
      case Right(result) => s"${call.id} ${call.tool} returned in ${call.ms.getOrElse(0L)}ms: ${cut(result, 300)}"
      case Left(error)   => s"${call.id} ${call.tool} FAILED: ${cut(error, 300)}"
    turn.copy(calls = turn.calls :+ call, events = turn.events ++
      (Event(call.startedAt, call.loop, s"${call.id} ${call.tool} ${cut(call.arguments, 300)}") +: end.map(Event(endedAt, call.loop, _)).toVector))

  /** A turn as text: its request, ending, diagnosis and timeline (times from its start), cut to `max` characters. */
  def render(turn: Turn, max: Int = 30000): String =
    def at(i: Instant) = f"+${java.time.Duration.between(turn.startedAt, i).toMillis / 1000.0}%.1fs"
    val head =
      s"""Turn ${turn.id} (session ${turn.session}), ${turn.ending}
         |Project: ${turn.project.getOrElse("?")}
         |Request: ${cut(turn.request, 1500)}
         |Diagnosis:
         |${diagnose(turn).map("- " + _).mkString("\n")}
         |Answer: ${turn.answer.fold("(none)")(cut(_, 1500))}
         |Timeline:""".stripMargin
    val lines = turn.events.map(e => s"${at(e.at)} ${e.loop.map(l => s"[${l.split('/').drop(2).mkString("/")}] ").getOrElse("")}${e.text}")
    val body = lines.mkString("\n")
    val all = head + "\n" + body
    if all.length <= max then all else all.take(max) + s"\n… (cut: ${lines.size} events in all; transcript_call shows any call whole)"
