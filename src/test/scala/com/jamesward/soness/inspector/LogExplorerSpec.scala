package com.jamesward.soness.inspector

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files

/** The log explorer: a debug log's records classified, timed and counted, turned into turns; its pages and API. */
object LogExplorerSpec extends ZIOSpecDefault:

  private val session = "c708ba46-45cb-4b57-93c5-8200038c899b"
  private def line(at: String, level: String, message: String, loop: Option[String] = None) =
    s"""timestamp=2026-10-19T15:22:$at-06 level=$level thread=zio-fiber-1 message="$message" session=$session""" +
      loop.fold("")(l => s" loop=$l")

  /** A small log: a session's start, a prompt, a Jev decision, a Bedrock writing, a tool call, the answer and metrics. */
  val log: List[String] = List(
    s"""timestamp=2026-10-19T15:22:35.1-06 level=DEBUG thread=zio-fiber-1 message="ACP send: {"jsonrpc":"2.0","id":2,"result":{"sessionId":"$session"}}" """,
    line("36.0", "INFO", s"session $session in /home/jw/projects/soness: 1 MCP servers, 57 tools"),
    line("40.0", "DEBUG", "prompt (task 1, model openai.gpt-oss-120b, mode auto): fix the SAVE10 code"),
    line("40.5", "DEBUG", """Jev exchange in 274ms: request={"questions":{"next_action":{}},"state":{"newRequest":"fix the SAVE10 code"}} response={"answers":{"next_action":{"type":"choice"}},"usage":{"input_tokens":7333,"output_tokens":384}}""",
      Some(s"${session.take(8)}/task-1/main")),
    line("41.5", "DEBUG", """Bedrock openai.gpt-oss-120b exchange in 977ms: request={"messages":[]} response={"output":{},"usage":{"inputTokens":71,"outputTokens":16,"totalTokens":87}}""",
      Some(s"${session.take(8)}/task-1/main")),
    line("41.6", "DEBUG", """tool call call-1 read_file: {"file_path":"src/Shop.java"}""", Some(s"${session.take(8)}/task-1/main")),
    line("41.7", "DEBUG", """tool call call-1 read_file returned in 56ms: {"text":"class Shop {}"}""", Some(s"${session.take(8)}/task-1/main")),
    s"""timestamp=2026-10-19T15:22:41.8-06 level=DEBUG thread=zio-fiber-1 message="ACP send: {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"$session","update":{"sessionUpdate":"agent_message_chunk"}}}" """,
    line("42.0", "DEBUG", "answer: The codes didn't match in case."),
    line("42.1", "DEBUG", """turn metrics: {"model":"openai.gpt-oss-120b","ending":"answered","latencyMs":2100,"toolCalls":1,"llmCalls":1,"llmInputTokens":71,"llmOutputTokens":16,"jevCalls":1,"jevInputTokens":7333,"jevOutputTokens":384,"jevLatencyMs":274,"llmLatencyMs":977,"toolLatencyMs":56}"""),
  )

  def spec = suite("log explorer")(
    test("each record is classified, with where it went, how long it took and its tokens, and the turn it's in") {
      val index = LogIndex.of(log.iterator)
      val kinds = index.entries.map(_.kind)
      val jev = index.entries.find(_.kind == LogIndex.Kind.Jev)
      val llm = index.entries.find(_.kind == LogIndex.Kind.Llm)
      val chunk = index.entries.filter(_.kind == LogIndex.Kind.Acp).last
      val jevParts = index.details(jev.map(_.i).getOrElse(0)).parts.map(_.label)
      val turn = index.turns.head
      assertTrue(
        kinds == Vector(LogIndex.Kind.Acp, LogIndex.Kind.Info, LogIndex.Kind.Prompt, LogIndex.Kind.Jev, LogIndex.Kind.Llm,
          LogIndex.Kind.ToolCall, LogIndex.Kind.ToolResult, LogIndex.Kind.Acp, LogIndex.Kind.Answer, LogIndex.Kind.Metrics),
        jev.flatMap(_.ms).contains(274L), jev.flatMap(_.inputTokens).contains(7333L), jev.flatMap(_.outputTokens).contains(384L),
        jev.exists(_.summary.contains("next_action")), jevParts == List("request", "response"),
        llm.exists(_.target.contains("openai.gpt-oss-120b")), llm.flatMap(_.inputTokens).contains(71L),
        // the session soness answered session/new with is its client's
        chunk.title == "ACP → client", chunk.summary.startsWith("session/update agent_message_chunk"),
        index.entries.filter(_.kind != LogIndex.Kind.Acp).drop(1).forall(_.turn.contains("c708ba46/task-1")),
        turn.request == "fix the SAVE10 code", turn.ending == "answered", turn.toolCalls == 1,
        turn.jevInputTokens == 7333L, turn.llmMs == 977L, turn.toolMs == 56L,
        index.totals.find(_.kind == LogIndex.Kind.Jev).exists(t => t.count == 1 && t.ms == 274L),
      )
    },
    test("the pages and the API serve only the logs under the explorer's paths, and the page's scripts and Tailwind") {
      for
        dir    <- ZIO.attemptBlocking(Files.createTempDirectory("logs"))
        file    = dir.resolve("soness.log")
        _      <- ZIO.attemptBlocking(Files.writeString(file, log.mkString("\n")))
        cache  <- LogExplorer.Cache.make
        routes  = LogExplorer.routes(List(dir), cache)
        get     = (path: String) => routes.runZIO(Request.get(URL.decode(path).toOption.get)).flatMap(r => r.body.asString.map(r.status.code -> _))
        files  <- get("/")
        named   = "soness.log"
        page   <- get("/soness.log")
        absent <- get("/other.log")
        api    <- get(s"/api/log?file=$named")
        entry  <- get(s"/api/entry?file=$named&i=3")
        other  <- get(s"/api/log?file=${java.net.URLEncoder.encode("/etc/passwd", "UTF-8")}")
        js     <- get("/static/explorer.js")
        css    <- get("/webjars/tailwindcss__browser/4.3.3/dist/index.global.js")
      yield
        val summary = api._2.fromJson[Json.Obj].toOption
        assertTrue(
          files._1 == 200, files._2.contains("soness.log"), files._2.contains("href=\"/soness.log\""),
          files._2.contains("/webjars/tailwindcss__browser/"),
          page._1 == 200, page._2.contains("/static/explorer.js"),
          absent._1 == 404,
          summary.flatMap(_.get("entries")).flatMap(_.asArray).exists(_.size == 10),
          summary.flatMap(_.get("turns")).flatMap(_.asArray).exists(_.size == 1),
          entry._1 == 200, entry._2.contains("\"label\":\"request\""), entry._2.contains("7333"),
          other._1 == 404,
          js._1 == 200, js._2.contains("/api/entry"),
          css._1 == 200, css._2.length > 1000,
        )
    },
  ) @@ TestAspect.timeout(60.seconds)
