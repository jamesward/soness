package com.jamesward.soness

import zio.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * soness's own tools on the project's files and shell, for what the session's other tools don't cover: a client that
 * gives neither an IDE's MCP server nor its files and terminals (ACP's `fs`, `terminal`) still gets a coding agent.
 * Which of them are offered is [[ToolResolver]]'s choice, by capability: an IDE's or client's tool that does the same
 * is always the one used, and these fill only the gaps.
 *
 *  - files: `read_file`, `list_files`, `search_text`, `edit_file` (an exact replacement, which the model writes far
 *    more reliably than a patch), `write_file`;
 *  - `run_command`: a shell command in the project, its end awaited (the process's exit completes it: nothing polls),
 *    killed when stopped or past its timeout.
 *
 * Paths are the project's, and its source dependencies' ([[SourceDependencies]]): one outside them is refused. Where
 * an IDE does the files, [[reaching]] fills its one gap: its edits and listings refuse a source dependency's files.
 * `SONESS_LOCAL_TOOLS=false` leaves them all out.
 */
object LocalTools:

  /** A file read, and a command's output, are cut to this many characters (a command's from the start, keeping its end). */
  val MaxText = 40000

  private def schema(required: List[String], properties: (String, String, String)*): Json.Obj =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(properties.map((n, t, d) => n -> (Json.Obj("type" -> Json.Str(t), "description" -> Json.Str(d)): Json))*),
      "required" -> Json.Arr(required.map(Json.Str(_))*),
    )

  private val readOnly = Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))

  val Read = ToolSpec("read_file", "Reads a text file of the project: each line as its number, a tab, then the line itself. " +
      s"Without line and limit it reads the whole file (up to $MaxText characters): read a range only of a file longer than that.",
    schema(List("path"), ("path", "string", "The file, relative to the project"), ("line", "integer", "The first line to read (1-based)"),
      ("limit", "integer", "How many lines to read")), None, readOnly)
  val ListFiles = ToolSpec("list_files", "Lists the project's files whose paths match a glob (`**/*.scala`, `src/**`), outside " +
      "version-control, IDE and build-output directories.",
    schema(List("glob"), ("glob", "string", "The glob, relative to the project")), None, readOnly)
  val Search = ToolSpec("search_text", "Searches the project's text files for a regular expression (case-sensitive unless it " +
      "starts with (?i)); gives each matching line with its file and number.",
    schema(List("pattern"), ("pattern", "string", "The regular expression (Java syntax)"),
      ("glob", "string", "Only files whose paths match this glob")), None, readOnly)
  val Edit = ToolSpec("edit_file", "Edits a file by replacing an exact piece of its text: `old_text` must occur in the file " +
      "exactly once (give enough surrounding lines to make it unique), unless `replace_all`.",
    schema(List("path", "old_text", "new_text"), ("path", "string", "The file, relative to the project"),
      ("old_text", "string", "The exact text to replace, whitespace included"), ("new_text", "string", "The text to put in its place"),
      ("replace_all", "boolean", "Replace every occurrence")), None)
  val Write = ToolSpec("write_file", "Writes a whole file (created, with its directories, when it doesn't exist): for new files.",
    schema(List("path", "content"), ("path", "string", "The file, relative to the project"), ("content", "string", "The whole content")),
    None)
  val Run = ToolSpec("run_command", "Runs a shell command in the project's directory (a build, the tests, git) and gives its " +
      "exit code and output (stdout and stderr together; a long output keeps its end).",
    schema(List("command"), ("command", "string", "The command, run by bash"),
      ("timeout_seconds", "integer", "Stop it after this many seconds (default 600)")), None)

  private val FileTools = Chunk(Read, ListFiles, Search, Edit, Write)

  private val directory = ("directory", "string", "A source dependency's directory (absolute) to work in instead of the project's")

  /** The local tools over the project at `root` and its source `dependencies` (the resolver picks which are offered). */
  def toolset(root: Path, dependencies: List[Path] = Nil): Toolset =
    val roots = dependencies.map(_.toAbsolutePath.normalize)
    val offered =
      (if roots.isEmpty then FileTools
       else FileTools.map(t => (if t.name == ListFiles.name || t.name == Search.name then withDirectory(t) else t)
         .copy(description = t.description + s" Also the project's source dependencies (${roots.mkString(", ")}): give their " +
           "files' absolute paths."))) :+ Run
    val base = root.toAbsolutePath.normalize
    new Toolset:
      val tools: Chunk[ToolSpec] = offered
      def call(name: String, arguments: Json.Obj): Task[Json] =
        def str(k: String) = arguments.get(k).flatMap(_.asString)
        def need(k: String) = ZIO.fromOption(str(k)).orElseFail(IllegalArgumentException(s"$k is required"))
        def int(k: String) = arguments.get(k).flatMap(_.asNumber).map(_.value.intValue)
        def path(k: String) = need(k).flatMap(p => inside(base, p, roots))
        // the directory a listing or search is of: the project's, or a source dependency's
        def dir(required: Boolean) = str("directory").filter(_.trim.nonEmpty) match
          case Some(d) => ZIO.fromOption(roots.find(_.equals(base.resolve(d).normalize)))
                            .orElseFail(IllegalArgumentException(s"$d isn't one of the source dependencies: ${roots.mkString(", ")}"))
          case None    => if required then ZIO.fail(IllegalArgumentException("directory is required")) else ZIO.succeed(base)
        def relativeTo(p: Path) = roots.find(p.startsWith).fold(base.relativize(p))(_ => p).toString
        // a source dependency's files by their absolute paths (relative ones are the project's)
        def shown(d: Path)(f: String) = if d.equals(base) then f else d.resolve(f).toString
        def listed(d: Path, g: String) = list(d, Some(g)).map(fs => Json.Obj("files" -> Json.Arr(fs.map(f => Json.Str(shown(d)(f)))*)))
        name match
          case "read_file"   => path("path").flatMap(read(_, int("line"), int("limit")))
          case "list_files"  => (dir(false) <*> need("glob")).flatMap(listed)
          case "search_text" => (dir(false) <*> need("pattern")).flatMap((d, p) => search(d, p, str("glob"), shown(d)))
          case "edit_file"   =>
            (path("path") <*> need("old_text") <*> need("new_text")).flatMap((p, o, n) =>
              edit(p, o, n, arguments.get("replace_all").contains(Json.Bool(true))).as(Json.Obj("edited" -> Json.Str(relativeTo(p)))))
          case "write_file"  =>
            (path("path") <*> need("content")).flatMap((p, c) => ZIO.attemptBlocking { Files.createDirectories(p.getParent); Files.writeString(p, c, UTF_8) }
              .as(Json.Obj("written" -> Json.Str(relativeTo(p)))))
          case "run_command" => need("command").flatMap(c => run(base, c, int("timeout_seconds").getOrElse(600)))
          case other         => ZIO.fail(IllegalArgumentException(s"unknown tool: $other"))

  /** An IDE tool's arguments naming a file or directory (IntelliJ's names). */
  private val PathKeys = List("pathInProject", "file_path", "filePath", "directoryPath")

  /**
   * The session's `toolset` (an IDE's) with its file tools reaching the source `dependencies` too. An IDE's tools refuse
   * a path outside its project (`../zio-bedrock/…`, or absolute) for its edits and listings (its `read_file` reads
   * any source root itself), so a call of `create_new_file`, `list_directory_tree` or `apply_patch` on a dependency's
   * files is run here instead, as the IDE runs it ([[IntellijPatch]]); every other call (reads, searches, the project's
   * own files) is the IDE's. A `file://` URL given as a path is taken as its path: the IDE takes it as a relative one,
   * and makes a `file:` directory in the project.
   */
  def reaching(root: Path, toolset: Toolset, dependencies: List[Path]): Toolset =
    val base = root.toAbsolutePath.normalize
    val roots = dependencies.map(_.toAbsolutePath.normalize)
    val names = toolset.tools.map(_.name).toSet
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        val args = Json.Obj(arguments.fields.map:
          case (k, Json.Str(s)) if PathKeys.contains(k) => k -> Json.Str(withoutUrl(s))
          case field                                    => field)
        def str(k: String) = args.get(k).flatMap(_.asString)
        def int(k: String) = args.get(k).flatMap(_.asNumber).map(_.value.intValue)
        def resolved(p: String) = base.resolve(p).normalize
        val patch = str("input").orElse(str("patch")).filter(_ => name == "apply_patch")
        val targets = patch.fold(PathKeys.flatMap(str))(p => scala.util.Try(IntellijPatch.paths(p)).getOrElse(Nil))
        val dependency = targets.map(resolved).find(p => roots.exists(p.startsWith))
        (dependency, names(name)) match
          // only what the IDE refuses outside its project is done here: the rest (its read reaches every source root) is its
          case (Some(target), true) if Set("create_new_file", "list_directory_tree", "apply_patch")(name) =>
            name match
              case "create_new_file" =>
                ZIO.attemptBlocking(Files.exists(target)).flatMap: exists =>
                  if exists && !args.get("overwrite").contains(Json.Bool(true)) then
                    ZIO.fail(IllegalArgumentException(s"File already exists: $target. Specify 'overwrite=true' to overwrite it"))
                  else ZIO.attemptBlocking { Files.createDirectories(target.getParent); Files.writeString(target, str("text").getOrElse(""), UTF_8) }
                    .as(Json.Obj("text" -> Json.Str("[success]")))
              case "list_directory_tree" =>
                val depth = int("maxDepth").getOrElse(5).max(1)
                list(target, None).map(fs => Json.Obj("directory" -> Json.Str(target.toString),
                  "files" -> Json.Arr(fs.filter(_.count(_ == '/') < depth).map(f => Json.Str(target.resolve(f).toString))*)))
              case "apply_patch" =>
                targets.map(resolved).find(p => !p.startsWith(base) && !roots.exists(p.startsWith)) match
                  case Some(outside) => ZIO.fail(IllegalArgumentException(s"$outside is outside the project and its source dependencies"))
                  case None =>
                    ZIO.attemptBlocking(IntellijPatch.apply(base, patch.map("input" -> _).toMap)).flatMap:
                      case Left(error) => ZIO.fail(IllegalArgumentException(error))
                      case Right(text) =>
                        // an operation that failed fails the call, as the IDE's own result is taken ([[RoutedTools.checked]])
                        val result = Json.Obj("text" -> Json.Str(text))
                        ZIO.fromEither(RoutedTools.checked(name, result)).mapError(IllegalArgumentException(_))
          case _ => toolset.call(name, args)

  /** A `file:` URL as its path; anything else as it is. */
  private def withoutUrl(path: String): String =
    if !path.startsWith("file:") then path
    else scala.util.Try(Path.of(java.net.URI(path)).toString).getOrElse(path.stripPrefix("file://").stripPrefix("file:"))

  /**
   * `path` resolved in the project; one outside it (`../`, an absolute path elsewhere) is refused, unless it's in one of
   * the source dependencies `also`.
   */
  def inside(base: Path, path: String, also: List[Path] = Nil): Task[Path] =
    val resolved = base.resolve(path).normalize
    if resolved.startsWith(base) || also.exists(resolved.startsWith(_)) then ZIO.succeed(resolved)
    else ZIO.fail(IllegalArgumentException(s"$path is outside the project" +
      (if also.isEmpty then "" else s" and its source dependencies (${also.mkString(", ")})")))

  /** A listing or search tool, with a source dependency's directory as an alternative to the project's. */
  private def withDirectory(tool: ToolSpec): ToolSpec =
    val props = tool.inputSchema.get("properties").flatMap(_.asObject).getOrElse(Json.Obj())
    tool.copy(inputSchema = tool.inputSchema.add("properties", props.add(directory._1,
      Json.Obj("type" -> Json.Str(directory._2), "description" -> Json.Str(directory._3)))))

  private def read(file: Path, line: Option[Int], limit: Option[Int]): Task[Json] =
    missing(file).flatMap(ZIO.foreachDiscard(_)(why => ZIO.fail(IllegalArgumentException(why)))) *>
    ZIO.attemptBlocking(Files.readAllLines(file, UTF_8).asScala.toVector).map: lines =>
      val from = line.getOrElse(1).max(1)
      val shown = lines.drop(from - 1).take(limit.getOrElse(Int.MaxValue))
      // a tab after the number: the line's own indentation starts right after it, unambiguously
      val text = shown.zipWithIndex.map((l, i) => f"${from + i}%6d\t$l").mkString("\n")
      Json.Obj("text" -> Json.Str(if text.length > MaxText then text.take(MaxText) + s"\n… (cut; read on from a later line)" else text),
        "lines" -> Json.Num(lines.size))

  /**
   * Why `file` can't be read, when it can't: it doesn't exist (with the files of the nearest directory that does, the
   * likeliest first: a guessed path is corrected from them rather than guessed again), or it's a directory (with its
   * files). None when it's a file.
   */
  def missing(file: Path): UIO[Option[String]] =
    ZIO.attemptBlocking:
      if Files.isRegularFile(file) then None
      else
        val dir = if Files.isDirectory(file) then Some(file)
                  else Iterator.iterate(Option(file.getParent))(_.flatMap(p => Option(p.getParent))).takeWhile(_.isDefined).flatten.find(Files.isDirectory(_))
        val entries = dir.toList.flatMap: d =>
          val s = Files.list(d)
          try s.iterator.asScala.toList.map(p => if Files.isDirectory(p) then s"${p.getFileName}/" else p.getFileName.toString) finally s.close()
        // the names most like the one asked for first (shared words: SessionUpdate is like Sessions)
        val wanted = file.getFileName.toString.toLowerCase.split("(?=[A-Z])|[^a-z0-9]+").filter(_.length > 2).toSet ++
          file.getFileName.toString.split("(?=[A-Z])").map(_.toLowerCase.replaceAll("[^a-z0-9]", "")).filter(_.length > 2)
        def score(n: String) = -wanted.count(w => n.toLowerCase.contains(w))
        val shown = entries.sortBy(n => (score(n), n)).take(40).mkString(", ")
        Some(
          if Files.isDirectory(file) then s"$file is a directory, not a file; it has: $shown"
          else s"no such file: $file" + dir.fold("")(d => s". ${if d.equals(file.getParent) then "Its directory" else s"The nearest directory that exists, $d,"} has: $shown"))
    .orElseSucceed(None)

  private def edit(file: Path, oldText: String, newText: String, all: Boolean): Task[Unit] =
    ZIO.attemptBlocking(Files.readString(file, UTF_8)).flatMap: text =>
      ZIO.fromEither(replaced(text, oldText, newText, all)).mapError(IllegalArgumentException(_))
        .flatMap(edited => ZIO.attemptBlocking(Files.writeString(file, edited, UTF_8)).unit)

  /**
   * `text` with `oldText` replaced by `newText`: it must occur exactly once (every occurrence with `all`); one whose
   * lines match but for indentation (a model miscounts spaces) is replaced re-indented. What's wrong, otherwise.
   */
  def replaced(text: String, oldText: String, newText: String, all: Boolean): Either[String, String] =
    val count = if oldText.isEmpty then 0 else occurrences(text, oldText)
    if oldText.isEmpty then Left("old_text is empty: use write_file for a new file")
    else if count == 0 then reindented(text, oldText, newText).toRight("old_text isn't in the file (whitespace and all): read it again")
    else if count > 1 && !all then Left(s"old_text occurs $count times: give more surrounding text, or replace_all")
    else Right(if all then text.replace(oldText, newText) else replaceOnce(text, oldText, newText))

  private def occurrences(text: String, piece: String): Int =
    Iterator.iterate(text.indexOf(piece))(i => text.indexOf(piece, i + 1)).takeWhile(_ >= 0).size

  /**
   * `text` with `oldText`'s lines replaced by `newText`'s when they match exactly one block of `text`'s lines but for
   * leading whitespace; `newText` shifted by the indentation the block has over `oldText`'s first line.
   */
  def reindented(text: String, oldText: String, newText: String): Option[String] =
    val lines = text.split("\n", -1).toVector
    val old = oldText.split("\n", -1).toVector.dropWhile(_.isBlank).reverse.dropWhile(_.isBlank).reverse
    if old.isEmpty then None
    else
      val at = lines.indices.filter(i => i + old.size <= lines.size && old.indices.forall(k => lines(i + k).trim == old(k).trim))
      at match
        case Seq(i) =>
          def indent(l: String) = l.takeWhile(c => c == ' ' || c == '\t')
          val (from, to) = (indent(old.head), indent(lines(i)))
          val shifted = newText.split("\n", -1).toVector.map(l => if l.startsWith(from) then to + l.drop(from.length) else l)
          Some((lines.take(i) ++ shifted ++ lines.drop(i + old.size)).mkString("\n"))
        case _ => None

  private def replaceOnce(text: String, oldText: String, newText: String): String =
    val i = text.indexOf(oldText)
    text.substring(0, i) + newText + text.substring(i + oldText.length)

  /**
   * A glob over paths relative to `base`, as models and IDEs write them: a `**` directory also matches no directory at
   * all (so `src/main/` globs match under a `**` prefix too), and a glob without a `/` matches a file's name in any
   * directory (`*.scala`), where Java's glob alone matches neither.
   */
  def globMatcher(base: Path, glob: String): Path => Boolean =
    val g = glob.trim.stripPrefix("./")
    // each `**/` kept or dropped (at most the first four)
    val parts = g.split("\\*\\*/", -1).toList
    val (kept, rest) = parts.tail.splitAt(4)
    val variants = kept.foldLeft(List(parts.head))((acc, p) => acc.flatMap(a => List(a + "**/" + p, a + p))).map(_ + rest.map("**/" + _).mkString)
    val matchers = variants.distinct.map(v => base.getFileSystem.getPathMatcher(s"glob:$v"))
    val named = !g.contains('/')
    rel => matchers.exists(_.matches(rel)) || (named && Option(rel.getFileName).exists(n => matchers.exists(_.matches(n))))

  /** The project's files (relative), outside skipped directories, matching `glob` when given; at most 2000. */
  private def list(base: Path, glob: Option[String]): Task[List[String]] =
    ZIO.attemptBlocking:
      val matcher = glob.map(globMatcher(base, _))
      val found = List.newBuilder[String]
      var count = 0
      Files.walkFileTree(base, new java.nio.file.SimpleFileVisitor[Path]:
        override def preVisitDirectory(dir: Path, attrs: java.nio.file.attribute.BasicFileAttributes) =
          if !dir.equals(base) && ProjectContext.Skipped(dir.getFileName.toString) then java.nio.file.FileVisitResult.SKIP_SUBTREE
          else java.nio.file.FileVisitResult.CONTINUE
        override def visitFile(file: Path, attrs: java.nio.file.attribute.BasicFileAttributes) =
          val rel = base.relativize(file)
          if attrs.isRegularFile && matcher.forall(_(rel)) then
            found += rel.toString
            count += 1
          if count >= 2000 then java.nio.file.FileVisitResult.TERMINATE else java.nio.file.FileVisitResult.CONTINUE)
      found.result().sorted

  private def search(base: Path, pattern: String, glob: Option[String], shown: String => String = identity): Task[Json] =
    for
      regex <- ZIO.attempt(java.util.regex.Pattern.compile(pattern)).mapError(e => IllegalArgumentException(s"bad pattern: ${e.getMessage}"))
      files <- list(base, glob)
      hits  <- ZIO.attemptBlocking:
                 files.iterator.flatMap: f =>
                   scala.util.Try(Files.readAllLines(base.resolve(f), UTF_8).asScala).toOption.iterator.flatMap: lines =>
                     lines.iterator.zipWithIndex.collect { case (l, i) if regex.matcher(l).find() => s"${shown(f)}:${i + 1}: ${l.take(300)}" }
                 .take(200).toList
    yield Json.Obj("matches" -> Json.Arr(hits.map(Json.Str(_))*), "count" -> Json.Num(hits.size),
      "note" -> Json.Str(if hits.size >= 200 then "the first 200 matches; narrow the pattern or glob" else ""))

  /**
   * `command` by bash in `base`: its output to a temporary file (no pipe to drain), its exit awaited through the
   * process's own exit future, killed (with its children) when interrupted or past `timeout`.
   */
  def run(base: Path, command: String, timeout: Int): Task[Json] =
    ZIO.acquireReleaseWith(ZIO.attemptBlocking(Files.createTempFile("soness-run", ".log")))(f => ZIO.attemptBlocking(Files.deleteIfExists(f)).ignore): out =>
      ZIO.acquireReleaseExitWith(
        ZIO.attemptBlocking(ProcessBuilder("bash", "-c", command).directory(base.toFile).redirectErrorStream(true)
          .redirectOutput(out.toFile).redirectInput(ProcessBuilder.Redirect.from(java.io.File("/dev/null"))).start()),
      )((p, exit) => ZIO.succeed { p.descendants().forEach(_.destroyForcibly()); p.destroyForcibly() }.unless(p.isAlive == false && exit.isSuccess).unit):
        p =>
          ZIO.fromCompletableFuture(p.onExit()).timeout(timeout.max(1).seconds).flatMap: exited =>
            ZIO.attemptBlocking(Files.readString(out, UTF_8)).map: text =>
              val kept = if text.length > MaxText then "… (the start cut)\n" + text.takeRight(MaxText) else text
              exited.fold(Json.Obj("timed_out" -> Json.Bool(true), "output" -> Json.Str(kept)))(e =>
                Json.Obj("exit_code" -> Json.Num(e.exitValue), "output" -> Json.Str(kept)))
