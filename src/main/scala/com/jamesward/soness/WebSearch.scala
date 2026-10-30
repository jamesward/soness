package com.jamesward.soness

import zio.*
import zio.json.ast.Json

/** How much search context a backend may gather: Bedrock's `search_context_size` (Kiro has no such setting). */
enum ContextSize(val wire: String) derives CanEqual:
  case Low extends ContextSize("low")
  case Medium extends ContextSize("medium")
  case High extends ContextSize("high")

object ContextSize:
  def of(wire: String): Option[ContextSize] = values.find(_.wire == wire)

/** Where in the answer a citation applies (Bedrock's `start_index`/`end_index`). */
final case class Span(start: Int, end: Int) derives CanEqual

/** A source behind a search answer; its URL must be shown to the user with the answer. */
final case class Citation(url: String, title: Option[String], span: Option[Span]) derives CanEqual

/** A grounded answer: never without at least one citation. */
final case class SearchResult(answer: String, citations: NonEmptyChunk[Citation])

/**
 * A web search backend. Which one a session gets depends on its writer (TOOLS_SPEC.md): Bedrock web search for a
 * Bedrock model with access to it (Mantle, a GPT-5.4 or later model, a Region that has it), Kiro's own `web_search`
 * when Kiro writes, otherwise none.
 */
trait WebSearch:
  def search(query: String, size: ContextSize): Task[SearchResult]

object WebSearch:

  val Name = "web_search"

  val spec: ToolSpec = ToolSpec(
    Name,
    "Searches the web and gives a short answer grounded in what it found, with the sources it used (url and title). " +
      "Use it for current or specialized information: a library's latest version, documentation for a new API, an " +
      "error message. Read a source in full with web_fetch. Always show the sources' links with anything you take " +
      "from the answer.",
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "query" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("What to search the web for")),
        "context_size" -> Json.Obj(
          "type" -> Json.Str("string"),
          "enum" -> Json.Arr(Chunk.fromIterable(ContextSize.values.map(s => Json.Str(s.wire)))),
          "description" -> Json.Str("How much to gather: low for a quick lookup, medium (the default), high for a complex question"),
        ),
      ),
      "required" -> Json.Arr(Json.Str("query")),
    ),
    Some(Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "answer" -> Json.Obj("type" -> Json.Str("string")),
        "citations" -> Json.Obj(
          "type" -> Json.Str("array"),
          "items" -> Json.Obj(
            "type" -> Json.Str("object"),
            "properties" -> Json.Obj(
              "url" -> Json.Obj("type" -> Json.Str("string")),
              "title" -> Json.Obj("type" -> Json.Str("string")),
              "start_index" -> Json.Obj("type" -> Json.Str("integer")),
              "end_index" -> Json.Obj("type" -> Json.Str("integer")),
            ),
          ),
        ),
      ),
    )),
    Some(Json.Obj("readOnlyHint" -> Json.Bool(true), "openWorldHint" -> Json.Bool(true))),
  )

  /** The `web_search` tool, searching with `backend`. */
  def toolset(backend: WebSearch): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = Chunk(spec)
      def call(name: String, arguments: Json.Obj): Task[Json] =
        if name != Name then ZIO.fail(IllegalArgumentException(s"unknown tool: $name"))
        else
          for
            query  <- ZIO.fromOption(arguments.get("query").flatMap(_.asString).map(_.trim).filter(_.nonEmpty))
                        .orElseFail(IllegalArgumentException("query is required"))
            size   <- arguments.get("context_size").flatMap(_.asString) match
                        case None       => ZIO.succeed(ContextSize.Medium)
                        case Some(wire) => ZIO.fromOption(ContextSize.of(wire)).orElseFail(
                          IllegalArgumentException(s"context_size is one of ${ContextSize.values.map(_.wire).mkString(", ")}, not $wire"))
            result <- backend.search(query, size)
          yield json(result)

  def json(result: SearchResult): Json =
    Json.Obj(
      "answer" -> Json.Str(result.answer),
      "citations" -> Json.Arr(result.citations.toChunk.map: c =>
        Json.Obj(Chunk("url" -> Json.Str(c.url)) ++
          Chunk.fromIterable(c.title.map(t => "title" -> Json.Str(t))) ++
          Chunk.fromIterable(c.span.toList.flatMap(s => List("start_index" -> Json.Num(s.start), "end_index" -> Json.Num(s.end)))))),
    )

  /** What Kiro is asked: one search with its own tools, answered with its sources as Markdown links. */
  def kiroPrompt(query: String): String =
    s"Search the web for this, with your web_search tool: $query\n\n" +
      "Answer briefly from what you found, not from memory, and cite every source you used as a Markdown link " +
      "[title](url). Use no tool but web_search: don't fetch, read, write or run anything."

  /**
   * Kiro as the backend: each search is a prompted turn (`ask` sends a prompt to a Kiro session that may use its
   * web_search tool only and gives back its text). Kiro has no context-size setting, so `size` is unused,
   * and its citations come from the links in its answer, without spans.
   */
  def kiro(ask: String => Task[String]): WebSearch =
    new WebSearch:
      def search(query: String, size: ContextSize): Task[SearchResult] =
        ask(kiroPrompt(query)).map(_.trim).flatMap: answer =>
          ZIO.fromOption(NonEmptyChunk.fromChunk(citations(answer)))
            .orElseFail(RuntimeException(s"Kiro's search for \"$query\" cited no sources: ${answer.take(300)}"))
            .map(SearchResult(answer, _))

  /**
   * Bedrock as the backend: the session's model answers with Bedrock's Web Search (zio-bedrock's `MantleWebSearch`),
   * its citations with the spans of the answer they back. An answer that cites nothing is a failure, as Kiro's is.
   */
  def bedrock(ask: (String, ContextSize) => Task[com.jamesward.zio_bedrock.MantleWebSearch.Answer]): WebSearch =
    new WebSearch:
      def search(query: String, size: ContextSize): Task[SearchResult] =
        ask(query, size).flatMap: a =>
          ZIO.fromOption(NonEmptyChunk.fromIterableOption(a.citations.map(c => Citation(c.url, Some(c.title), Some(Span(c.start, c.end))))))
            .orElseFail(RuntimeException(s"Bedrock's search for \"$query\" cited no sources: ${a.text.take(300)}"))
            .map(SearchResult(a.text, _))

  private val MarkdownLink = """\[([^\]]+)\]\((https?://[^)\s]+)\)""".r
  private val BareUrl = """https?://[^\s)\]>"'`]+""".r

  /** The sources an answer links to, in order, once each: Markdown links with their titles, then bare URLs. */
  def citations(text: String): Chunk[Citation] =
    val linked = MarkdownLink.findAllMatchIn(text).map(m => Citation(m.group(2), Some(m.group(1).trim).filter(_.nonEmpty), None)).toList
    val bare = BareUrl.findAllIn(MarkdownLink.replaceAllIn(text, " ")).map(u => Citation(u.stripSuffix(".").stripSuffix(","), None, None)).toList
    Chunk.fromIterable((linked ++ bare).foldLeft(List.empty[Citation]): (kept, c) =>
      if kept.exists(_.url == c.url) then kept else c :: kept
    .reverse)
