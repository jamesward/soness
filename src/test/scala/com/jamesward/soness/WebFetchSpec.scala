package com.jamesward.soness

import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object WebFetchSpec extends ZIOSpecDefault:

  private def serve(accepts: Ref[Option[String]]) =
    val routes = Routes(
      Method.GET / "page" -> handler { (request: Request) =>
        accepts.set(request.rawHeader("Accept")).as(Response(body = Body.fromString(
          "<html><head><title>t</title><style>x{}</style></head><body><h1>Easy Racer</h1><p>Race 2 requests&amp;win.</p>" +
            "<script>bad()</script><ul><li>one</li><li>two</li></ul></body></html>"), headers = Headers(Header.ContentType(MediaType.text.html))))
      },
      Method.GET / "long" -> handler(Response.text("abcdefghij" * 10)),
      Method.GET / "missing" -> handler(Response.status(Status.NotFound)),
      Method.GET / "moved" -> handler(Response(status = Status.TemporaryRedirect, headers = Headers(Header.Custom("Location", "/long")))),
      Method.GET / "loop" -> handler(Response(status = Status.TemporaryRedirect, headers = Headers(Header.Custom("Location", "/loop")))),
    )
    for
      server <- Server.defaultWith(_.port(0)).build
      port   <- Server.install(routes).provideEnvironment(server)
    yield s"http://127.0.0.1:$port"

  private def text(json: Json) = json.asObject.flatMap(_.get("text")).flatMap(_.asString).getOrElse("")

  def spec = suite("web_fetch")(
    test("HTML becomes its text, Markdown is asked for first, and a long page comes in parts") {
      for
        accepts <- Ref.make(Option.empty[String])
        base    <- serve(accepts)
        http    <- ZIO.environment[Client]
        tools    = WebFetch.toolset(http)
        page    <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/page")))
        accept  <- accepts.get
        first   <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/long"), "max_length" -> Json.Num(30)))
        next    <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/long"), "start_index" -> Json.Num(30), "max_length" -> Json.Num(30)))
        missing <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/missing"))).either
        file    <- tools.call("web_fetch", Json.Obj("url" -> Json.Str("file:///etc/passwd"))).either
        moved   <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/moved")))
        loop    <- tools.call("web_fetch", Json.Obj("url" -> Json.Str(s"$base/loop"))).either
      yield assertTrue(
        text(page) == "Easy Racer\nRace 2 requests&win.\n\n- one\n- two",
        accept.exists(_.startsWith("text/markdown")),
        text(first).startsWith("abcdefghij" * 3), text(first).contains("fetch with start_index 30"),
        text(next).startsWith("abcdefghij" * 3), first.toJson.contains("\"totalLength\":100"),
        missing.left.exists(_.getMessage.contains("HTTP 404")),
        file.left.exists(_.getMessage.contains("only http and https")),
        // a redirect is followed (a relative location too); one that never ends is a failure, not an empty page
        text(moved).startsWith("abcdefghij"), loop.left.exists(_.getMessage.contains("HTTP 307")),
      )
    },
    test("combined with a session's tools, a server's tool of the same name is kept") {
      val server = new Toolset:
        val tools = Chunk(ToolSpec("web_fetch", "theirs", Json.Obj(), None), ToolSpec("read_file", "r", Json.Obj(), None))
        def call(name: String, arguments: Json.Obj) = ZIO.succeed(Json.Str(s"server $name"))
      for
        http <- ZIO.environment[Client]
        both  = WebFetch.combine(server, WebFetch.toolset(http))
        got  <- both.call("web_fetch", Json.Obj())
      yield assertTrue(both.tools.map(_.description) == Chunk("theirs", "r"), got.toJson == "\"server web_fetch\"")
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
