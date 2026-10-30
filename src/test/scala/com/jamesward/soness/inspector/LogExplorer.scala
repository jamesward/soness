package com.jamesward.soness.inspector

import webjars.generated.WebJars
import zio.*
import zio.http.*
import zio.http.template2.*
import zio.json.*

import java.nio.file.{Files, Path as FilePath}

/**
 * A local web app for exploring soness debug logs (`--debug`): each log's turns (request, ending, time,
 * tokens, calls), and every record in order, filterable, with what was sent where (ACP messages and their peer, Jev's
 * and Bedrock's requests and responses, an ACP writer's prompts and answers, tool calls with their arguments and
 * results), how long each took and the tokens it used.
 *
 * Development only (test code), and only on the loopback address, with no authentication: logs hold prompts, tool
 * data and model output. It serves only the `.log` files under the paths it's given:
 *
 * {{{
 * ./sbta logs     # ~Test/runReload: restarted on every change to its code
 * ./sbta "Test/runMain com.jamesward.soness.inspector.LogExplorer [--port 8077] [--static dir] [file-or-directory …]"
 * }}}
 *
 * With no paths: `target/logs` (soness's `--debug` logs) and `target/evals/logs` (the evals'). `--static` serves the page's JS and CSS from that directory instead of
 * the classpath, so an edit to them shows on a refresh, with no restart (`logs` passes the source directory).
 */
object LogExplorer extends ZIOAppDefault:

  val DefaultPort = 8077

  /** The logs under `roots`: each a `.log` file, or a directory's (three levels down), newest first. */
  def logs(roots: List[FilePath]): Task[List[FilePath]] =
    ZIO.attemptBlocking:
      roots.flatMap: root =>
        if Files.isRegularFile(root) then List(root)
        else if Files.isDirectory(root) then
          // a directory it can't read is skipped, not a failure
          val found = scala.collection.mutable.ListBuffer.empty[FilePath]
          Files.walkFileTree(root, java.util.Set.of(), 3, new java.nio.file.SimpleFileVisitor[FilePath]:
            override def visitFile(p: FilePath, attrs: java.nio.file.attribute.BasicFileAttributes) =
              if attrs.isRegularFile && p.getFileName.toString.endsWith(".log") then found += p
              java.nio.file.FileVisitResult.CONTINUE
            override def visitFileFailed(p: FilePath, e: java.io.IOException) = java.nio.file.FileVisitResult.CONTINUE
          )
          found.toList
        else Nil
      .map(_.toAbsolutePath.normalize).distinct.sortBy(p => -Files.getLastModifiedTime(p).toMillis)

  /** The parsed logs, each kept until its file changes (its size or last change). */
  final class Cache(entries: Ref[Map[FilePath, (Long, Long, LogIndex.Index)]]):
    def index(file: FilePath): Task[LogIndex.Index] =
      for
        stamp <- ZIO.attemptBlocking((Files.size(file), Files.getLastModifiedTime(file).toMillis))
        kept  <- entries.get.map(_.get(file).filter(e => e._1 == stamp._1 && e._2 == stamp._2).map(_._3))
        index <- kept.fold(ZIO.attemptBlocking {
                   val source = scala.io.Source.fromFile(file.toFile, "UTF-8")
                   try LogIndex.of(source.getLines()) finally source.close()
                 }.tap(i => entries.update(_.updated(file, (stamp._1, stamp._2, i)))))(ZIO.succeed(_))
      yield index

  object Cache:
    val make: UIO[Cache] = Ref.make(Map.empty[FilePath, (Long, Long, LogIndex.Index)]).map(Cache(_))

  private val tailwind = WebJars.url(WebJars.Artifact.`tailwindcss__browser`, "dist/index.global.js")

  private def page(heading: String, contents: Modifier*): Dom =
    html(
      lang := "en",
      head(
        meta(charset := "UTF-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(s"$heading · soness logs"),
        script(src := tailwind),
        link(rel := "stylesheet", href := "/static/explorer.css"),
      ),
      body(
        `class` := "bg-slate-950 text-slate-200 text-sm antialiased",
        header(
          `class` := "sticky top-0 z-10 border-b border-slate-800 bg-slate-950/95 px-4 py-2 flex items-center gap-4",
          a(href := "/", `class` := "font-bold text-cyan-300", "soness logs"),
          span(`class` := "truncate text-slate-400", heading),
        ),
        zio.http.template2.main(`class` := "px-4 py-3", contents),
      ),
    )

  private def size(bytes: Long): String =
    if bytes < 1024 then s"$bytes B" else if bytes < 1024 * 1024 then f"${bytes / 1024.0}%.0f KB" else f"${bytes / 1048576.0}%.1f MB"

  def filesPage(files: List[FilePath]): Dom =
    page("logs",
      if files.isEmpty then p(`class` := "text-slate-400", "No .log files under the explorer's paths.")
      else
        table(
          `class` := "w-full",
          thead(tr(`class` := "text-left text-slate-400", th("log"), th("size"), th("changed"))),
          tbody(files.map: f =>
            tr(
              `class` := "border-t border-slate-800 hover:bg-slate-900",
              td(`class` := "py-1", a(href := s"/${java.net.URLEncoder.encode(f.getFileName.toString, "UTF-8").replace("+", "%20")}", `class` := "text-cyan-300 hover:underline", f.toString)),
              td(size(Files.size(f))),
              td(`class` := "text-slate-400", Files.getLastModifiedTime(f).toInstant.toString.take(19).replace('T', ' ')),
            )),
        ),
    )

  def logPage(file: FilePath): Dom =
    page(file.toString,
      div(id := "app", data("file") := file.getFileName.toString, `class` := "space-y-4",
        p(`class` := "text-slate-400", "Reading the log…")),
      script(src := "/static/explorer.js", defer),
    )

  private def json[A: JsonEncoder](a: A): Response =
    Response(body = Body.fromString(a.toJson), headers = Headers(Header.ContentType(MediaType.application.json)))

  def routes(roots: List[FilePath], cache: Cache, static: Option[FilePath] = None): Routes[Any, Nothing] =
    // only the logs under the explorer's paths are served, named by their file name (the newest of that name)
    def log(name: String): IO[Response, FilePath] =
      for
        all  <- logs(roots).orElseFail(Response.internalServerError)
        file <- ZIO.fromOption(all.find(_.getFileName.toString == name)).orElseFail(Response.notFound(s"$name isn't one of the explorer's logs"))
      yield file
    def allowed(request: Request): IO[Response, FilePath] =
      ZIO.fromOption(request.queryParam("file")).orElseFail(Response.badRequest("file is required")).flatMap(log)
    def failed(e: Throwable) = Response.internalServerError(e.getMessage)
    // the page's JS and CSS (the classpath's, or `--static`'s directory) and the webjars (Tailwind)
    // served by routes of their own, not middleware
    val assets: Routes[Any, Nothing] =
      static.fold(Routes.serveResources(Path.empty / "static", "log-explorer"))(d => Routes.serveDirectory(Path.empty / "static", d.toFile))
    val webjars: Routes[Any, Nothing] = Routes.serveResources(Path.empty / "webjars", "META-INF/resources/webjars")
    val app = Routes(
      Method.GET / Root -> handler(logs(roots).mapBoth(failed, fs => Response.html(filesPage(fs)))),
      // a log's page is at its file name, with no directory
      Method.GET / string("name") -> handler((name: String, _: Request) => log(name).map(f => Response.html(logPage(f)))),
      Method.GET / "api" / "log" -> handler((request: Request) =>
        allowed(request).flatMap(f => cache.index(f).mapBoth(failed, i => json(i.summary)))),
      Method.GET / "api" / "entry" -> handler((request: Request) =>
        for
          file   <- allowed(request)
          i      <- ZIO.fromOption(request.queryParam("i").flatMap(_.toIntOption)).orElseFail(Response.badRequest("i is required"))
          index  <- cache.index(file).mapError(failed)
          detail <- ZIO.fromOption(index.details.lift(i)).orElseFail(Response.notFound(s"no entry $i"))
        yield json(detail)),
    )
    // the handlers fail with their response (a 400 or a 404): that response is what's sent
    (app ++ assets ++ webjars).handleError(identity)

  def run =
    for
      args  <- getArgs.map(_.toList)
      options = args.sliding(2).collect { case List(k, v) if k.startsWith("--") => k -> v }.toMap
      port   = options.get("--port").flatMap(_.toIntOption).getOrElse(DefaultPort)
      static = options.get("--static").map(FilePath.of(_)).filter(Files.isDirectory(_))
      named  = args.zipWithIndex.filterNot((a, i) => a.startsWith("--") || (i > 0 && args(i - 1).startsWith("--"))).map(_._1)
      roots  = (if named.isEmpty then List("target/logs", "target/evals/logs") else named).map(FilePath.of(_))
      cache <- Cache.make
      _     <- ZIO.logInfo(s"soness log explorer on http://127.0.0.1:$port (no authentication: loopback only), logs under ${roots.mkString(", ")}")
      _     <- Server.serve(routes(roots, cache, static)).provide(Server.defaultWith(_.binding("127.0.0.1", port)))
    yield ()
