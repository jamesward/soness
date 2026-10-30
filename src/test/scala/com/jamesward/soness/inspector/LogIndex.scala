package com.jamesward.soness.inspector

import com.jamesward.soness.Transcript
import zio.json.*
import zio.json.ast.Json

import java.time.Instant

/**
 * A soness debug log (`--debug`) as entries to explore: every log record, classified (an ACP message and
 * its peer, a Jev or Bedrock exchange, an ACP writer's writing, a tool call and its result, a prompt, an answer, turn
 * metrics, a warning), with where it went, how long it took and the tokens it used, and the turn it belongs to
 * ([[Transcript]]'s turns). Pure: built from the log's lines, so a spec feeds it lines and the explorer a file.
 */
object LogIndex:

  enum Kind derives CanEqual:
    case Acp, Jev, Llm, Writer, ToolCall, ToolResult, ToolFailed, Prompt, Answer, Metrics, Warning, Info, Debug

  object Kind:
    given JsonEncoder[Kind] = JsonEncoder.string.contramap(_.toString)

  /** One record, summarized: the explorer's table row. */
  final case class Entry(
    i: Int,
    t: Long,
    level: String,
    kind: Kind,
    title: String,
    target: Option[String],
    session: Option[String],
    loop: Option[String],
    turn: Option[String],
    ms: Option[Long],
    inputTokens: Option[Long],
    outputTokens: Option[Long],
    summary: String,
  ) derives JsonEncoder

  /** A part of a record's message: labelled JSON (pretty-printed by the page), or text. */
  final case class Part(label: String, json: Option[Json], text: Option[String]) derives JsonEncoder

  /** One record whole: its entry, its message's parts, and the message as logged. */
  final case class Detail(entry: Entry, parts: List[Part], raw: String) derives JsonEncoder

  /** A turn: its request, ending, time, calls and tokens, from its metrics or, without them, its entries. */
  final case class TurnSummary(
    id: String,
    session: String,
    request: String,
    ending: String,
    t: Long,
    wallMs: Option[Long],
    toolCalls: Int,
    failedCalls: Int,
    jevCalls: Int,
    jevMs: Long,
    jevInputTokens: Long,
    jevOutputTokens: Long,
    llmCalls: Int,
    llmMs: Long,
    llmInputTokens: Long,
    llmOutputTokens: Long,
    toolMs: Long,
    model: Option[String],
    answer: Option[String],
  ) derives JsonEncoder

  /** Sums for one kind of entry: how many, how long, how many tokens. */
  final case class Total(kind: Kind, count: Int, ms: Long, inputTokens: Long, outputTokens: Long) derives JsonEncoder

  /** A log as the explorer serves it. */
  final case class Index(entries: Vector[Entry], details: Vector[Detail], turns: Vector[TurnSummary], totals: List[Total]):
    def summary: Summary = Summary(entries, turns, totals)

  /** What the log's page loads: every entry's row, the turns, the totals. */
  final case class Summary(entries: Vector[Entry], turns: Vector[TurnSummary], totals: List[Total]) derives JsonEncoder

  private val Exchange = """(?s)Jev exchange in (\d+)ms: request=(.*)""".r
  private val LlmExchange = """(?s)Bedrock (\S+) exchange in (\d+)ms: request=(.*)""".r
  private val WriterExchange = """(?s)ACP writer (\S+) in (\d+)ms \(session (\w+), writing (\d+)\): prompt=(.*)""".r
  private val AcpMessage = """(?s)ACP (send|receive): (.*)""".r
  private val CallStart = """(?s)tool call (call-\d+) ([\w.-]+): (.*)""".r
  private val CallEnd = """(?s)tool call (call-\d+) ([\w.-]+) returned in (\d+)ms: (.*)""".r
  private val CallFail = """(?s)tool call (call-\d+) ([\w.-]+) failed: (.*)""".r
  private val Prompt = """(?s)prompt \(task (\d+), ([^)]*)\): (.*)""".r
  private val AnswerText = """(?s)answer: (.*)""".r
  private val MetricsJson = """(?s)turn metrics: (.*)""".r
  private val TaskOfLoop = """([0-9a-f]{8})/task-(\d+).*""".r

  def of(lines: Iterator[String]): Index =
    val records = Transcript.records(lines).toVector
    val turns = records.foldLeft(Transcript.State())((s, r) => Transcript.step(s, r)._1).turns
    val peers = acpPeers(records)
    val details = records.zipWithIndex.map((r, i) => detail(i, r, turnOf(r, turns), peers.getOrElse(i, "?")))
    val entries = details.map(_.entry)
    Index(entries, details, turns.map(summarize(_, entries)), totals(entries))

  /** The turn a record belongs to: its loop's task, or the session's turn running at the time. */
  private def turnOf(r: Transcript.Record, turns: Vector[Transcript.Turn]): Option[String] =
    r.session.flatMap: s =>
      val ofSession = turns.filter(_.session == s)
      r.loop.collect { case TaskOfLoop(_, n) => n.toInt }.flatMap(n => ofSession.find(_.task == n))
        .orElse(ofSession.filter(t => !t.startedAt.isAfter(r.at) && t.endedAt.forall(e => !e.isBefore(r.at))).lastOption)
        .map(_.id)

  /**
   * The peer of each ACP message (by record): `client` (the ACP client soness serves) or `writer` (an ACP agent soness
   * drives, such as Kiro). A session id soness answered `session/new` with is its client's, one it was answered with
   * a writer's; without a session id, what's asked tells (soness is asked `initialize` and `session/new` by its client,
   * and asks them of its writers), and a response goes to the peer whose request had its id.
   */
  private def acpPeers(records: Vector[Transcript.Record]): Map[Int, String] =
    final case class Message(i: Int, sent: Boolean, body: String, obj: Option[Json.Obj]):
      def method: Option[String] = obj.flatMap(_.get("method")).flatMap(_.asString)
      def id: Option[String] = obj.flatMap(_.get("id")).map(_.toJson)
    val messages = records.zipWithIndex.collect:
      case (r, i) if r.message.startsWith("ACP send: ") || r.message.startsWith("ACP receive: ") =>
        val sent = r.message.startsWith("ACP send: ")
        val body = r.message.dropWhile(_ != ':').drop(2)
        Message(i, sent, body, body.fromJson[Json.Obj].toOption)
    val sessions = messages.flatMap: m =>
      m.obj.flatMap(_.get("result")).flatMap(_.asObject).flatMap(_.get("sessionId")).flatMap(_.asString)
        .map(_ -> (if m.sent then "client" else "writer"))
    .toMap
    // requests awaiting their responses, by who asked and the id: the peer they're with
    messages.foldLeft((Map.empty[Int, String], Map.empty[(Boolean, String), String])):
      case ((peers, pending), m) =>
        val bySession = sessions.collectFirst { case (id, p) if m.body.contains(id) => p }
        val byMethod = m.method.collect:
          case "initialize" | "session/new" | "session/load" | "session/resume" | "session/list" => if m.sent then "writer" else "client"
          case kiro if kiro.startsWith("_kiro.dev/") => "writer"
        val byId = Option.when(m.method.isEmpty)(m.id.flatMap(id => pending.get((!m.sent, id)))).flatten
        val peer = bySession.orElse(byMethod).orElse(byId)
        val waiting = (m.method, m.id, peer) match
          case (Some(_), Some(id), Some(p)) => pending.updated((m.sent, id), p)
          case (None, Some(id), _)          => pending.removed((!m.sent, id))
          case _                            => pending
        (peer.fold(peers)(p => peers.updated(m.i, p)), waiting)
    ._1

  private def json(text: String): Option[Json] = text.trim.fromJson[Json].toOption

  /** `text` split at the first ` <label>=` whose sides both read as JSON (a request's text can contain the label). */
  private def splitJson(text: String, label: String): Option[(Json, Json)] =
    val marker = s" $label="
    Iterator.iterate(text.indexOf(marker))(i => text.indexOf(marker, i + 1)).takeWhile(_ >= 0)
      .flatMap(i => json(text.substring(0, i)).zip(json(text.substring(i + marker.length)))).nextOption()

  private def num(j: Option[Json], keys: String*): Option[Long] =
    j.flatMap(_.asObject).flatMap(o => keys.flatMap(k => o.get(k).flatMap(_.asNumber).map(_.value.longValue)).headOption)

  private def usage(response: Json): Option[Json] = response.asObject.flatMap(_.get("usage"))

  private def cut(text: String): String =
    val flat = text.replace('\n', ' ')
    if flat.length <= 220 then flat else flat.take(220) + "…"

  private def detail(i: Int, r: Transcript.Record, turn: Option[String], peer: String): Detail =
    def entry(kind: Kind, title: String, target: Option[String] = None, ms: Option[Long] = None,
        in: Option[Long] = None, out: Option[Long] = None, summary: String = r.message) =
      Entry(i, r.at.toEpochMilli, r.level, kind, title, target, r.session, r.loop, turn, ms, in, out, cut(summary))
    def text(label: String, value: String) = Part(label, None, Some(value))
    def jsonOrText(label: String, value: String) = json(value).fold(text(label, value))(j => Part(label, Some(j), None))
    r.message match
      case Exchange(ms, rest) =>
        val (request, response) = splitJson(rest, "response").fold[(Option[Json], Option[Json])]((None, None))((a, b) => (Some(a), Some(b)))
        val u = response.flatMap(usage)
        val asked = request.flatMap(_.asObject).flatMap(_.get("questions")).flatMap(_.asObject).map(_.keys.mkString(", "))
        Detail(entry(Kind.Jev, "Jev", Some("jev"), Some(ms.toLong), num(u, "input_tokens", "inputTokens"), num(u, "output_tokens", "outputTokens"),
          asked.fold("decision")(q => s"asked: $q")),
          request.map(j => Part("request", Some(j), None)).toList ++ response.map(j => Part("response", Some(j), None)).toList ++
            Option.when(request.isEmpty)(text("exchange", rest)).toList, r.message)
      case LlmExchange(model, ms, rest) =>
        val (request, response) = splitJson(rest, "response").fold[(Option[Json], Option[Json])]((None, None))((a, b) => (Some(a), Some(b)))
        val u = response.flatMap(usage)
        val said = response.map(_.toJson).getOrElse(rest)
        Detail(entry(Kind.Llm, s"Bedrock $model", Some(model), Some(ms.toLong), num(u, "inputTokens", "prompt_tokens", "input_tokens"),
          num(u, "outputTokens", "completion_tokens", "output_tokens"), said),
          request.map(j => Part("request", Some(j), None)).toList ++ response.map(j => Part("response", Some(j), None)).toList ++
            Option.when(request.isEmpty)(text("exchange", rest)).toList, r.message)
      case WriterExchange(agent, ms, session, writing, rest) =>
        val at = rest.lastIndexOf(" answer=")
        val (prompt, answer) = if at < 0 then (rest, "") else (rest.substring(0, at), rest.substring(at + 8))
        Detail(entry(Kind.Writer, s"ACP writer $agent", Some(agent), Some(ms.toLong), summary = s"writing $writing (session $session): $answer"),
          List(text("prompt", prompt), jsonOrText("answer", answer)), r.message)
      case AcpMessage(direction, body) =>
        val message = body.fromJson[Json.Obj].toOption
        val method = message.flatMap(_.get("method")).flatMap(_.asString)
        val update = message.flatMap(_.get("params")).flatMap(_.asObject).flatMap(_.get("update")).flatMap(_.asObject)
          .flatMap(_.get("sessionUpdate")).flatMap(_.asString)
        val what = (method, message.flatMap(_.get("id"))) match
          case (Some(m), _)  => m + update.fold("")(u => s" $u")
          case (None, Some(id)) => s"response ${id.toJson}"
          case _             => "message"
        Detail(entry(Kind.Acp, s"ACP ${if direction == "send" then "→" else "←"} $peer", Some(peer), summary = s"$what ${body.take(200)}"),
          List(jsonOrText(direction, body)), r.message)
      case CallEnd(id, tool, ms, result) =>
        Detail(entry(Kind.ToolResult, s"$tool returned", Some(tool), Some(ms.toLong), summary = s"$id: $result"), List(jsonOrText("result", result)), r.message)
      case CallFail(id, tool, error) =>
        Detail(entry(Kind.ToolFailed, s"$tool failed", Some(tool), summary = s"$id: $error"), List(text("error", error)), r.message)
      case CallStart(id, tool, arguments) =>
        Detail(entry(Kind.ToolCall, tool, Some(tool), summary = s"$id: $arguments"), List(jsonOrText("arguments", arguments)), r.message)
      case Prompt(task, about, request) =>
        Detail(entry(Kind.Prompt, s"prompt (task $task)", summary = request), List(text("about", about), text("request", request)), r.message)
      case AnswerText(answer) => Detail(entry(Kind.Answer, "answer", summary = answer), List(text("answer", answer)), r.message)
      case MetricsJson(metrics) =>
        val m = json(metrics)
        Detail(entry(Kind.Metrics, "turn metrics", ms = num(m, "latencyMs"),
          in = num(m, "llmInputTokens").zip(num(m, "jevInputTokens")).map(_ + _),
          out = num(m, "llmOutputTokens").zip(num(m, "jevOutputTokens")).map(_ + _)), List(jsonOrText("metrics", metrics)), r.message)
      case message =>
        val kind = r.level match
          case "WARN" | "ERROR" => Kind.Warning
          case "INFO"           => Kind.Info
          case _                => Kind.Debug
        Detail(entry(kind, r.level.toLowerCase), List(text("message", message)), r.message)

  private def summarize(t: Transcript.Turn, entries: Vector[Entry]): TurnSummary =
    val mine = entries.filter(_.turn.contains(t.id))
    def sum(k: Kind, f: Entry => Option[Long]) = mine.filter(_.kind == k).flatMap(f).sum
    def metric(key: String) = t.metrics.flatMap(_.get(key)).flatMap(_.asNumber).map(_.value.longValue)
    val llm = mine.filter(e => e.kind == Kind.Llm || e.kind == Kind.Writer)
    TurnSummary(
      id = t.id, session = t.session, request = t.request, ending = t.ending, t = t.startedAt.toEpochMilli, wallMs = t.wallMs,
      toolCalls = t.calls.size, failedCalls = t.calls.count(_.failed),
      jevCalls = metric("jevCalls").map(_.toInt).getOrElse(mine.count(_.kind == Kind.Jev)),
      jevMs = metric("jevLatencyMs").getOrElse(sum(Kind.Jev, _.ms)),
      jevInputTokens = metric("jevInputTokens").getOrElse(sum(Kind.Jev, _.inputTokens)),
      jevOutputTokens = metric("jevOutputTokens").getOrElse(sum(Kind.Jev, _.outputTokens)),
      llmCalls = metric("llmCalls").map(_.toInt).getOrElse(llm.size),
      llmMs = metric("llmLatencyMs").getOrElse(llm.flatMap(_.ms).sum),
      llmInputTokens = metric("llmInputTokens").getOrElse(llm.flatMap(_.inputTokens).sum),
      llmOutputTokens = metric("llmOutputTokens").getOrElse(llm.flatMap(_.outputTokens).sum),
      toolMs = metric("toolLatencyMs").getOrElse(t.calls.flatMap(_.ms).sum),
      model = t.metrics.flatMap(_.get("model")).flatMap(_.asString),
      answer = t.answer,
    )

  private def totals(entries: Vector[Entry]): List[Total] =
    Kind.values.toList.flatMap: k =>
      val of = entries.filter(_.kind == k)
      Option.when(of.nonEmpty)(Total(k, of.size, of.flatMap(_.ms).sum, of.flatMap(_.inputTokens).sum, of.flatMap(_.outputTokens).sum))
