package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** `web_search`: the tool's arguments, its result's shape, and Kiro as its backend (a prompted turn, sources from its links). */
object WebSearchSpec extends ZIOSpecDefault:

  /** A backend that records the context size each search was asked with and answers with one source. */
  private def recording(sizes: Ref[Vector[ContextSize]]): WebSearch =
    new WebSearch:
      def search(query: String, size: ContextSize): Task[SearchResult] =
        sizes.update(_ :+ size).as(SearchResult(s"About $query", NonEmptyChunk(Citation("https://zio.dev", Some("ZIO"), Some(Span(0, 5))))))

  def spec = suite("web_search")(
    test("an answer's sources: Markdown links with their titles, then bare URLs, each once, trailing punctuation dropped") {
      val text = "See [ZIO](https://zio.dev) and https://scala-lang.org. Also [ZIO again](https://zio.dev), and https://scala-lang.org,"
      assertTrue(
        WebSearch.citations(text).toList == List(
          Citation("https://zio.dev", Some("ZIO"), None),
          Citation("https://scala-lang.org", None, None),
        ),
        WebSearch.citations("nothing cited here").isEmpty,
      )
    },
    test("Kiro as the backend: the query in a prompt that asks for its web tools and cited links; the reply's links are the citations") {
      for
        asked  <- Ref.make(Vector.empty[String])
        kiro    = WebSearch.kiro(prompt => asked.update(_ :+ prompt).as("  ZIO 2 is current, see [ZIO releases](https://github.com/zio/zio/releases).  "))
        result <- kiro.search("latest ZIO version", ContextSize.Low)
        prompts <- asked.get
      yield assertTrue(
        prompts.size == 1, prompts.head.contains("latest ZIO version"), prompts.head.contains("web_search"),
        prompts.head.contains("Markdown link"),
        result.answer == "ZIO 2 is current, see [ZIO releases](https://github.com/zio/zio/releases).",
        result.citations.toList == List(Citation("https://github.com/zio/zio/releases", Some("ZIO releases"), None)),
      )
    },
    test("Kiro's reply that cites nothing is a failure, not an ungrounded answer") {
      for
        failed <- WebSearch.kiro(_ => ZIO.succeed("I think it's 2.1, from memory.")).search("latest ZIO version", ContextSize.Medium).either
      yield assertTrue(failed.left.exists(_.getMessage.contains("cited no sources")))
    },
    test("the tool: context_size defaults to medium and is checked; query is required; the result has the answer and its citations") {
      for
        sizes   <- Ref.make(Vector.empty[ContextSize])
        tools    = WebSearch.toolset(recording(sizes))
        high    <- tools.call(WebSearch.Name, Json.Obj("query" -> Json.Str("zio"), "context_size" -> Json.Str("high")))
        _       <- tools.call(WebSearch.Name, Json.Obj("query" -> Json.Str("zio")))
        badSize <- tools.call(WebSearch.Name, Json.Obj("query" -> Json.Str("zio"), "context_size" -> Json.Str("huge"))).either
        noQuery <- tools.call(WebSearch.Name, Json.Obj("query" -> Json.Str("  "))).either
        other   <- tools.call("web_fetch", Json.Obj("query" -> Json.Str("zio"))).either
        asked   <- sizes.get
        out      = high.toJson
      yield assertTrue(
        tools.tools.map(_.name).toList == List(WebSearch.Name),
        asked == Vector(ContextSize.High, ContextSize.Medium),
        badSize.left.exists(_.getMessage.contains("context_size")),
        noQuery.left.exists(_.getMessage.contains("query is required")),
        other.left.exists(_.getMessage.contains("unknown tool")),
        out.contains("\"answer\":\"About zio\""), out.contains("\"url\":\"https://zio.dev\""), out.contains("\"title\":\"ZIO\""),
        out.contains("\"start_index\":0"), out.contains("\"end_index\":5"),
      )
    },
  )
