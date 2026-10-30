package com.jamesward.soness

import zio.*
import zio.http.*
import zio.test.*

/**
 * Brave as the `web_search` backend: against a local fake of the Brave Search API (the key in `X-Subscription-Token`,
 * the context size as `count`, http(s) results as numbered links and citations, failures for no results and for an
 * HTTP error), and live against the real API, one query answered with cited sources. The live test is skipped unless
 * `BRAVE_API_KEY` is set (sbt reads it from `.env`).
 */
object BraveSearchSpec extends ZIOSpecDefault:

  /** What the fake saw of one request: its key and its URL (path and query). */
  private final case class Seen(token: Option[String], url: String) derives CanEqual

  private val Results =
    """{"web":{"results":[""" +
      """{"title":"ZIO","url":"https://zio.dev","description":"Type-safe effects"},""" +
      """{"title":"Not the web","url":"ftp://zio.dev"},""" +
      """{"title":"Scala","url":"https://scala-lang.org"}]}}"""

  /** A fake Brave Search API: `/search` finds results, `/empty` finds none, `/denied` refuses the key. */
  private def serve(seen: Ref[Vector[Seen]]) =
    def record(request: Request) = seen.update(_ :+ Seen(request.rawHeader("X-Subscription-Token"), request.url.encode))
    val routes = Routes(
      Method.GET / "search" -> handler((request: Request) => record(request).as(Response.text(Results))),
      Method.GET / "empty" -> handler((request: Request) => record(request).as(Response.text("""{"web":{"results":[]}}"""))),
      Method.GET / "denied" -> handler((request: Request) =>
        record(request).as(Response(status = Status.Unauthorized, body = Body.fromString("bad key")))),
    )
    for
      server <- Server.defaultWith(_.port(0)).build
      port   <- Server.install(routes).provideEnvironment(server)
    yield s"http://127.0.0.1:$port"

  private def credentials(values: Map[String, String]): DecisionModels.Credentials = name => ZIO.succeed(values.get(name))

  /** Brave's search as soness builds it from credentials, pointed at the fake's `path`. */
  private def brave(base: String, path: String) =
    for
      http   <- ZIO.environment[Client]
      found  <- BraveSearch.fromCredentials(
                  credentials(Map(BraveSearch.Variable -> "dev", BraveSearch.EndpointVariable -> s"$base/$path")), http)
                  .mapError(RuntimeException(_))
      search <- ZIO.fromOption(found).orElseFail(RuntimeException("no Brave search although BRAVE_API_KEY is set"))
    yield search

  def spec = suite("Brave web search")(
    test("against a fake: the key and count are sent, http(s) results become links and citations, failures fail") {
      for
        seen     <- Ref.make(Vector.empty[Seen])
        base     <- serve(seen)
        http     <- ZIO.environment[Client]
        none     <- BraveSearch.fromCredentials(credentials(Map.empty), http).mapError(RuntimeException(_))
        found    <- brave(base, "search").flatMap(_.search("zio", ContextSize.Low))
        empty    <- brave(base, "empty").flatMap(_.search("zio", ContextSize.High)).either
        denied   <- brave(base, "denied").flatMap(_.search("zio", ContextSize.Medium)).either
        requests <- seen.get
      yield assertTrue(
        none.isEmpty,
        found.citations.toList == List(
          Citation("https://zio.dev", Some("ZIO"), None),
          Citation("https://scala-lang.org", Some("Scala"), None),
        ),
        found.answer.contains("1. [ZIO](https://zio.dev)"),
        found.answer.contains("2. [Scala](https://scala-lang.org)"),
        !found.answer.contains("ftp://"),
        requests.size == 3,
        requests.forall(_.token.contains("dev")),
        requests.headOption.exists(r => r.url.contains("q=zio") && r.url.contains("count=5")),
        requests.lift(1).exists(_.url.contains("count=20")),
        requests.lift(2).exists(_.url.contains("count=10")),
        empty.left.exists(_.getMessage.contains("found no results")),
        denied.left.exists(_.getMessage.contains("HTTP 401")),
      )
    },
    test("live: a query is answered from the web, citing at least one source") {
      for
        http   <- ZIO.environment[Client]
        found  <- BraveSearch.fromCredentials(DecisionModels.environment, http).mapError(RuntimeException(_))
        search <- ZIO.fromOption(found).orElseFail(RuntimeException(s"${BraveSearch.Variable} isn't set"))
        result <- search.search("ZIO Scala library for asynchronous programming", ContextSize.Low)
        _      <- ZIO.logInfo(s"answer: ${result.answer}\ncitations: ${result.citations.map(_.url).mkString(", ")}")
      yield assertTrue(
        result.answer.trim.nonEmpty,
        result.citations.forall(c => c.url.startsWith("http://") || c.url.startsWith("https://")),
      )
    } @@ TestAspect.ifEnvSet(BraveSearch.Variable) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(1.minute),
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.sequential
