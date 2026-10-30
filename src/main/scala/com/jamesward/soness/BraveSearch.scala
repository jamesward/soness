package com.jamesward.soness

import zio.*
import zio.http.{Client, Header, Headers, Request, URL}
import zio.json.*
import zio.json.ast.Json

/**
 * Brave as a web search backend: the Brave Search API's web search (`GET /res/v1/web/search`, the key in
 * `X-Subscription-Token`), available whenever `BRAVE_API_KEY` is set, whatever the writer. Brave gives ranked results,
 * not a written answer, so the answer is the results themselves (each a Markdown link with its snippet) and every
 * result is a citation, without spans. A search that finds nothing is a failure, as an uncited answer is for Kiro and
 * Bedrock. `SONESS_BRAVE_ENDPOINT` replaces the API's URL (a proxy, or the tests' fake).
 */
object BraveSearch:

  /** The variable the key comes from. */
  val Variable = "BRAVE_API_KEY"

  /** The variable that replaces the API's URL. */
  val EndpointVariable = "SONESS_BRAVE_ENDPOINT"

  val DefaultEndpoint: URL =
    URL.decode("https://api.search.brave.com/res/v1/web/search").fold(e => throw IllegalStateException(e.getMessage), identity)

  /** A Brave Search API key. */
  opaque type ApiKey = String

  object ApiKey:
    def apply(key: String): ApiKey = key

  extension (key: ApiKey) def unwrap: String = key

  /** How many results a search asks for (Brave's `count`, at most 20): its stand-in for the context size. */
  def count(size: ContextSize): Int = size match
    case ContextSize.Low    => 5
    case ContextSize.Medium => 10
    case ContextSize.High   => 20

  /** One web result: its title, its URL and its snippet (Brave's markup reduced to text). */
  final case class Result(title: String, url: String, description: String) derives CanEqual

  /** The web results of a Brave response (`web.results`), those with an http(s) URL, in Brave's order. */
  def results(json: Json): Chunk[Result] =
    val items = json.asObject.flatMap(_.get("web")).flatMap(_.asObject).flatMap(_.get("results")).flatMap(_.asArray)
      .getOrElse(Chunk.empty)
    items.flatMap: item =>
      val fields = item.asObject
      def str(name: String): Option[String] = fields.flatMap(_.get(name)).flatMap(_.asString).map(_.trim).filter(_.nonEmpty)
      Chunk.fromIterable(str("url").filter(u => u.startsWith("http://") || u.startsWith("https://")).map: u =>
        Result(str("title").map(WebFetch.text).filter(_.nonEmpty).getOrElse(u), u, str("description").fold("")(WebFetch.text)))

  /** The answer: each result as a numbered Markdown link, with its snippet. */
  def answer(results: Chunk[Result]): String =
    results.zipWithIndex.map: (r, i) =>
      s"${i + 1}. [${r.title}](${r.url})" + (if r.description.nonEmpty then s": ${r.description}" else "")
    .mkString("\n")

  /** Brave as the backend, searching with `key` through `http` at `endpoint`. */
  def make(key: ApiKey, http: ZEnvironment[Client], endpoint: URL = DefaultEndpoint): WebSearch =
    new WebSearch:
      def search(query: String, size: ContextSize): Task[SearchResult] =
        val url = endpoint.addQueryParam("q", query).addQueryParam("count", count(size).toString)
        val request = Request.get(url).addHeaders(Headers(
          Header.Custom("Accept", "application/json"),
          Header.Custom("X-Subscription-Token", key.unwrap),
          Header.Custom("User-Agent", s"soness/${Soness.version}"),
        ))
        val fetched: Task[(Int, String)] =
          ZIO.scoped(Client.batched(request).flatMap(response => response.body.asString.map(response.status.code -> _)))
            .provideEnvironment(http)
            .timeoutFail(RuntimeException(s"Brave search: no response for \"$query\" within 30 seconds"))(30.seconds)
        fetched.flatMap: (status, body) =>
          if status >= 300 then ZIO.fail(RuntimeException(s"Brave search answered HTTP $status: ${body.take(300)}"))
          else
            ZIO.fromEither(body.fromJson[Json]).mapError(e => RuntimeException(s"Brave search's answer isn't JSON: $e"))
              .flatMap: json =>
                ZIO.fromOption(NonEmptyChunk.fromChunk(results(json)))
                  .orElseFail(RuntimeException(s"Brave's search for \"$query\" found no results"))
                  .map(found => SearchResult(answer(found.toChunk), found.map(r => Citation(r.url, Some(r.title), None))))

  /** Brave's search when `BRAVE_API_KEY` is set (at `SONESS_BRAVE_ENDPOINT` when that is), else none. */
  def fromCredentials(credentials: DecisionModels.Credentials, http: ZEnvironment[Client]): IO[String, Option[WebSearch]] =
    for
      key      <- credentials(Variable)
      endpoint <- credentials(EndpointVariable).flatMap: url =>
                    ZIO.foreach(url)(u => ZIO.fromEither(URL.decode(u)).orElseFail(s"$EndpointVariable is not a URL: $u"))
    yield key.map(k => make(ApiKey(k), http, endpoint.getOrElse(DefaultEndpoint)))
