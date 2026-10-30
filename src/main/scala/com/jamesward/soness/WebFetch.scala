package com.jamesward.soness

import zio.*
import zio.http.{Client, Header, Headers, Request, URL}
import zio.json.ast.Json

/**
 * soness's own `web_fetch` tool, so a request that points at documentation on the web (a spec, a project's README)
 * can be read: an HTTP(S) GET of a URL, the response's text (HTML reduced to its text) from `start_index`, at most
 * `max_length` characters (asking for Markdown first, which agent-friendly sites answer with instead of HTML), with the total length so the rest can be fetched in turn. It fetches whatever URL the model
 * writes, local addresses included, with the agent's network access; `SONESS_WEB_FETCH=false` leaves it out.
 */
object WebFetch:

  val Name = "web_fetch"

  /** At most this many characters per call by default; `max_length` may ask for up to [[MaxLength]]. */
  val DefaultLength = 20000
  val MaxLength = 100000

  /** Responses larger than this are cut before they're reduced to text. */
  private val MaxBytes = 5 * 1024 * 1024

  val spec: ToolSpec = ToolSpec(
    Name,
    "Fetches a web page or file over HTTP(S) and gives its text (HTML is reduced to its text: no markup, scripts or " +
      "styles). Use it to read documentation, specifications or source on the web that the request points to. A long " +
      "page comes in parts: the result says its total length; fetch again with start_index for the next part. For a " +
      "GitHub repository, its README is usually easier to read as raw text, from " +
      "https://raw.githubusercontent.com/<owner>/<repo>/<branch>/README.md.",
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "url" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("The http:// or https:// URL to fetch")),
        "start_index" -> Json.Obj("type" -> Json.Str("integer"), "description" -> Json.Str("Where in the text to start, for the next part of a long page (default 0)")),
        "max_length" -> Json.Obj("type" -> Json.Str("integer"), "description" -> Json.Str(s"At most this many characters (default $DefaultLength, at most $MaxLength)")),
      ),
      "required" -> Json.Arr(Json.Str("url")),
    ),
    Some(Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "url" -> Json.Obj("type" -> Json.Str("string")),
        "status" -> Json.Obj("type" -> Json.Str("integer")),
        "contentType" -> Json.Obj("type" -> Json.Str("string")),
        "totalLength" -> Json.Obj("type" -> Json.Str("integer")),
        "startIndex" -> Json.Obj("type" -> Json.Str("integer")),
        "text" -> Json.Obj("type" -> Json.Str("string")),
      ),
    )),
    Some(Json.Obj("readOnlyHint" -> Json.Bool(true), "openWorldHint" -> Json.Bool(true))),
  )

  /** The tool, fetching with `http`. */
  def toolset(http: ZEnvironment[Client]): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = Chunk(spec)
      def call(name: String, arguments: Json.Obj): Task[Json] =
        if name != Name then ZIO.fail(IllegalArgumentException(s"unknown tool: $name"))
        else fetch(arguments).provideEnvironment(http)

  /** `toolset`'s tools after `first`'s, leaving out any whose name `first` already has; calls go to the tool's owner. */
  def combine(first: Toolset, second: Toolset): Toolset =
    val names = first.tools.map(_.name).toSet
    val added = second.tools.filterNot(t => names(t.name))
    new Toolset:
      val tools: Chunk[ToolSpec] = first.tools ++ added
      def call(name: String, arguments: Json.Obj): Task[Json] =
        if added.exists(_.name == name) then second.call(name, arguments) else first.call(name, arguments)

  private def int(arguments: Json.Obj, name: String): Option[Int] =
    arguments.get(name).flatMap(_.asNumber).map(_.value.intValue)

  /** `url` fetched, a redirect followed to where it points (relative locations resolved), at most `left` times. */
  private def following(url: URL, left: Int): ZIO[Client & Scope, Throwable, zio.http.Response] =
    Client.batched(Request.get(url).addHeaders(Headers(
      Header.Custom("User-Agent", s"soness/${Soness.version}"),
      // markdown first: agent-friendly sites answer with it, more concisely than their HTML
      Header.Custom("Accept", "text/markdown, text/plain;q=0.9, text/html;q=0.8, */*;q=0.5"),
    ))).flatMap: response =>
      val location = Option.when(response.status.isRedirection)(response.rawHeader("Location")).flatten
      location.flatMap(l => URL.decode(l).toOption).map(l => if l.isAbsolute then l else url.resolve(l).getOrElse(l)) match
        case Some(next) if left > 0 => following(next, left - 1)
        case _                      => ZIO.succeed(response)

  private def fetch(arguments: Json.Obj): ZIO[Client, Throwable, Json] =
    for
      raw    <- ZIO.fromOption(arguments.get("url").flatMap(_.asString).map(_.trim).filter(_.nonEmpty))
                  .orElseFail(IllegalArgumentException("url is required"))
      _      <- ZIO.fail(IllegalArgumentException(s"only http and https URLs can be fetched, not $raw"))
                  .unless(raw.startsWith("http://") || raw.startsWith("https://"))
      url    <- ZIO.fromEither(URL.decode(raw)).mapError(_ => IllegalArgumentException(s"not a URL: $raw"))
      start   = int(arguments, "start_index").getOrElse(0).max(0)
      length  = int(arguments, "max_length").getOrElse(DefaultLength).max(1).min(MaxLength)
      result <- ZIO.scoped:
                  // redirects followed (at most 5): a page that moved answers 3xx with no body
                  following(url, 5).flatMap: response =>
                    response.body.asChunk.map: bytes =>
                      val contentType = response.header(Header.ContentType).map(_.renderedValue).getOrElse("")
                      val body = String(bytes.take(MaxBytes).toArray, java.nio.charset.StandardCharsets.UTF_8)
                      (response.status.code, contentType, if contentType.contains("html") then text(body) else body)
                .timeoutFail(RuntimeException(s"no response from $raw within 30 seconds"))(30.seconds)
      (status, contentType, all) = result
      _      <- ZIO.fail(RuntimeException(s"$raw answered HTTP $status: ${all.take(300)}")).when(status >= 300)
    yield Json.Obj(
      "url" -> Json.Str(raw),
      "status" -> Json.Num(status),
      "contentType" -> Json.Str(contentType),
      "totalLength" -> Json.Num(all.length),
      "startIndex" -> Json.Num(start),
      "text" -> Json.Str(all.slice(start, start + length) +
        (if start + length < all.length then s"\n… (${all.length - start - length} more characters; fetch with start_index ${start + length})" else "")),
    )

  /** An HTML page's text: scripts, styles and markup dropped, block elements on lines of their own, entities decoded. */
  def text(html: String): String =
    val dropped = html
      .replaceAll("(?is)<(script|style|noscript|svg|head)[^>]*>.*?</\\1>", " ")
      .replaceAll("(?s)<!--.*?-->", " ")
    val blocks = dropped.replaceAll("(?i)<(br|/p|/div|/tr|/h[1-6]|/pre|/table|/section|/article|hr)[^>]*>", "\n")
      .replaceAll("(?i)<li[^>]*>", "\n- ")
      .replaceAll("(?i)<h([1-6])[^>]*>", "\n\n")
    val plain = blocks.replaceAll("<[^>]+>", "")
    val decoded = plain.replace("&nbsp;", " ").replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"")
      .replace("&#39;", "'").replace("&#x27;", "'").replace("&amp;", "&")
    decoded.linesIterator.map(_.replaceAll("[ \\t]+", " ").trim).toList
      .foldLeft(List.empty[String]): (lines, line) =>
        if line.isEmpty && lines.headOption.contains("") then lines else line :: lines
      .reverse.mkString("\n").trim
