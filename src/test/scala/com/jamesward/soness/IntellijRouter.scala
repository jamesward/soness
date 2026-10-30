package com.jamesward.soness

import zio.json.*
import zio.json.ast.Json

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * IntelliJ IDEA 2026.2.3's MCP server as it serves ACP agents: router-only, its one tool `execute_tool` taking a command
 * line (`list_directory_tree --directoryPath src`). Everything here is taken from the real server: the tool's
 * description and schema and the routed tool names from its `tools/list` and errors (in soness's debug logs), the
 * routed tools' parameters, defaults, output shapes and error messages from the MCP server plugin
 * (`mcpserver.jar`), and `IntellijRouterSpec` replays the real exchanges captured in
 * `intellij-mcp-router-2026.2.3.jsonl` against it. The routed tools that read the project work on a directory on disk;
 * the others answer as unavailable.
 */
object IntellijRouter:

  /** `execute_tool` as the real server lists it. */
  val tool: Json.Obj = Json.Obj(
    "name" -> Json.Str("execute_tool"),
    "inputSchema" -> Json.Obj(
      "properties" -> Json.Obj("command" -> Json.Obj(
        "type" -> Json.Str("string"),
        "description" -> Json.Str("Command-line string with tool name and arguments"),
      )),
      "required" -> Json.Arr(Json.Str("command")),
      "type" -> Json.Str("object"),
    ),
    "description" -> Json.Str("Universal tool executor that can invoke specific IDE MCP tool dynamically."),
  )

  /** The tools the real router routes to, in the order its "not found" error lists them. */
  val routed: List[String] = List(
    "air_set_session_title", "analyze_calls", "apply_patch", "build_project", "cancel_sql_query",
    "configure_python_interpreter", "create_database_connection", "create_new_file", "edit_database_connection",
    "execute_run_configuration", "execute_sql_query", "execute_terminal_command", "execute_tool", "fetch_query_result",
    "get_all_open_file_paths", "get_database_object_description", "get_file_problems", "get_project_dependencies",
    "get_project_modules", "get_python_environment", "get_repositories", "get_run_configurations", "get_symbol_info",
    "git_status", "introspect_schema", "lint_files", "list_database_connections", "list_database_schemas",
    "list_directory_tree", "list_recent_sql_queries", "list_schema_object_kinds", "list_schema_objects",
    "open_file_in_editor", "preview_table_data", "read_file", "reformat_file", "rename_refactoring", "search_file",
    "search_regex", "search_symbol", "search_text", "test_database_connection", "xdebug_control_session",
    "xdebug_disassemble", "xdebug_evaluate_expression", "xdebug_get_debugger_status", "xdebug_get_frame_values",
    "xdebug_get_registers", "xdebug_get_stack", "xdebug_get_threads", "xdebug_get_value_by_path",
    "xdebug_list_breakpoints", "xdebug_read_memory", "xdebug_remove_breakpoint", "xdebug_run_to_line",
    "xdebug_set_breakpoint", "xdebug_set_variable", "xdebug_start_debugger_session",
  )

  /**
   * The command's words, split as a shell would: whitespace separates, quotes (single or double) group and are
   * removed, adjacent pieces join, an unclosed quote runs to the end. The real router does this: the command
   * `list_directory_tree" ,","directoryPath":"src` is the one word `list_directory_tree ,,directoryPath:src`.
   */
  def words(command: String): List[String] =
    val out = List.newBuilder[String]
    val word = StringBuilder()
    var started = false
    var quote: Option[Char] = None
    command.foreach: c =>
      quote match
        case Some(q) if c == q => quote = None
        case Some(_)           => word += c
        case None if c == '"' || c == '\'' => quote = Some(c); started = true
        case None if c.isWhitespace =>
          if started then { out += word.result(); word.clear(); started = false }
        case None => word += c; started = true
    if started then out += word.result()
    out.result()

  /** `--name value` pairs; the error for the first malformed one, worded as the real router words it. */
  private def parameters(arguments: List[String]): Either[String, Map[String, String]] =
    arguments match
      case Nil => Right(Map.empty)
      case flag :: rest if flag.startsWith("--") =>
        rest match
          // the next word is the value even when it starts with `--` (a unified diff, say), as the real router reads it
          case value :: more => parameters(more).map(_.updated(flag.drop(2), value))
          case _                                         => Left(s"Parameter '${flag.drop(2)}' requires a value")
      case other :: _ =>
        Left(s"Invalid argument format: '$other'. Expected '--paramName value' format. For object/array parameters pass a JSON value, e.g. --findings '[{...}]'.")

  private def required(params: Map[String, String], names: String*): Either[String, Unit] =
    val missing = names.filterNot(params.contains)
    if missing.isEmpty then Right(()) else Left(s"Missing required parameters: ${missing.mkString(", ")}")

  /** Runs `command` against the project at `project`: the tool's text result, or the error the router answers with. */
  def run(command: String, project: Path): Either[String, String] =
    words(command) match
      case Nil => Left("Command is empty")
      case name :: arguments =>
        if !routed.contains(name) then Left(s"Tool '$name' not found. Available tools: ${routed.mkString(", ")}")
        else
          parameters(arguments).flatMap: params =>
            name match
              case "list_directory_tree" =>
                required(params, "directoryPath").flatMap: _ =>
                  tree(project, params("directoryPath"), params.get("maxDepth").flatMap(_.toIntOption).getOrElse(5))
              case "read_file" =>
                required(params, "file_path").flatMap: _ =>
                  read(project, params("file_path"), params.get("offset").flatMap(_.toIntOption).getOrElse(1),
                    params.get("limit").flatMap(_.toIntOption).getOrElse(2000))
              case "search_text" =>
                required(params, "q").flatMap(_ => searchText(project, params("q"), paths(params), limit(params)))
              case "search_file" =>
                required(params, "q").flatMap(_ => searchFile(project, params("q"), paths(params), limit(params)))
              case "search_symbol" =>
                required(params, "q").flatMap(_ => searchSymbol(project, params("q"), paths(params), limit(params)))
              case "execute_terminal_command" =>
                required(params, "command").flatMap(_ => terminal(project, params("command"), params.get("timeout").flatMap(_.toIntOption).getOrElse(120000)))
              case "search_regex" =>
                required(params, "q").flatMap(_ => searchRegex(project, params("q"), paths(params), limit(params)))
              // as the IDE does, the build compiles the project (main and test sources) with its build tool; a project
              // with neither Gradle nor sbt builds trivially
              case "build_project" => build(project)
              // the file's errors from the build (the mock has no inspections beyond the compiler)
              case "get_file_problems" =>
                required(params, "filePath").flatMap: _ =>
                  val file = params("filePath")
                  build(project).map: out =>
                    val errors = out.fromJson[Json.Obj].toOption.flatMap(_.get("problems")).flatMap(_.asArray).toList.flatten
                      .filter(_.toJson.contains(file.split('/').last))
                    Json.Obj("filePath" -> Json.Str(file), "errors" -> Json.Arr(errors*)).toJson
              // there's no editor to open a file in; the IDE answers as it does when it opens one
              case "open_file_in_editor" =>
                required(params, "filePath").flatMap: _ =>
                  if Files.isRegularFile(resolve(project, params("filePath"))) then Right("[success]")
                  else Left(s"File ${params("filePath")} doesn't exist or can't be opened")
              case "apply_patch"     => IntellijPatch.apply(project, params)
              case "create_new_file" => IntellijPatch.create(project, params)
              case "get_project_modules" =>
                Right(Json.Obj("modules" -> Json.Arr(Json.Obj("name" -> Json.Str(project.getFileName.toString), "type" -> Json.Str("JAVA_MODULE")))).toJson)
              case other => Left(s"Tool execution failed: '$other' is not available in this test project")

  /**
   * The real `execute_terminal_command`, run as a process (`executeInShell` false, IntelliJ's default) in the project:
   * `command_exit_code` and `command_output`, or `is_timed_out`. The real IDE may first ask the user to allow the
   * command; the mock always runs it.
   */
  /**
   * The commands the mock's terminal runs: the project's build and test tools and read-only inspection, each command of
   * a pipeline or list. Anything else (docker, rm, package managers, network tools) is refused, as a user would refuse
   * it in the IDE's confirmation: the mock runs on the eval's host, where an agent once ran `docker rm -f $(docker ps -aq)`.
   */
  private val Allowed = Set("./gradlew", "gradle", "./sbt", "sbt", "java", "javac", "ls", "cat", "head", "tail", "grep", "find",
    "wc", "pwd", "echo", "true", "sort", "uniq", "diff", "tree", "sed", "awk")

  private[soness] def allowed(command: String): Boolean =
    val noSubstitution = !command.contains("$(") && !command.contains("`")
    val parts = command.split("&&|\\|\\||;|\\|").map(_.trim).filter(_.nonEmpty)
    noSubstitution && parts.nonEmpty && parts.forall: part =>
      val word = part.split("\\s+").dropWhile(_.contains("=")).headOption.getOrElse("")
      // curl only to this machine (a server the project's tests started, say): what a developer allows an agent to probe
      val localCurl = word == "curl" && """https?://(localhost|127\.0\.0\.1)[:/]""".r.findFirstIn(part).isDefined &&
        !part.contains(" -o ") && !part.contains(" -O") && !part.contains("--output")
      (Allowed(word) || localCurl) && !(word == "sed" && part.contains(" -i")) && !(word == "find" && (part.contains("-delete") || part.contains("-exec")))

  private def terminal(project: Path, command: String, timeoutMs: Int): Either[String, String] =
    if !allowed(command) then
      Left(s"The user declined to run this command in the IDE's terminal: $command (this terminal runs only the project's " +
        "build and test tools and read-only commands)")
    else
      val process = ProcessBuilder("bash", "-c", command).directory(project.toFile).redirectErrorStream(true).start()
      val finished = process.waitFor(timeoutMs.toLong, java.util.concurrent.TimeUnit.MILLISECONDS)
      if !finished then
        process.destroyForcibly()
        Right(Json.Obj("is_timed_out" -> Json.Bool(true)).toJson)
      else
        val output = String(process.getInputStream.readAllBytes()).linesIterator.take(2000).mkString("\n")
        Right(Json.Obj("command_exit_code" -> Json.Num(process.exitValue()), "command_output" -> Json.Str(output)).toJson)

  /** `build_project`: Gradle's or sbt's compile of main and test sources; its errors as the IDE's problems. */
  private def build(project: Path): Either[String, String] =
    val command =
      if Files.exists(project.resolve("gradlew")) then Some(List("./gradlew", "-q", "--console=plain", "compileJava", "compileTestJava"))
      else if Files.exists(project.resolve("sbt")) then Some(List("./sbt", "-batch", "Test/compile"))
      else
        // a plain Java project (no build tool): the IDE compiles its sources itself, as javac does
        val sources = Files.walk(project).iterator.asScala.filter(p => p.toString.endsWith(".java") && !p.toString.contains("/build/"))
          .map(p => project.relativize(p).toString).toList.sorted
        Option.when(sources.nonEmpty):
          val out = Files.createTempDirectory("ide-build").toString
          List("javac", "-d", out) ++ sources
    command.fold(Right(Json.Obj("isSuccess" -> Json.Bool(true), "problems" -> Json.Arr()).toJson)): cmd =>
      val process = ProcessBuilder(cmd*).directory(project.toFile).redirectErrorStream(true).start()
      val output = String(process.getInputStream.readAllBytes()).replaceAll("\u001b\\[[0-9;]*[mJ]", "")
      val ok = process.waitFor() == 0
      // each error with its details (javac's source line, `symbol:` and `location:` lines; scalac's lines after it), as
      // the IDE's problems carry the compiler's whole message
      val lines = output.linesIterator.toVector
      val starts = lines.indices.filter(i => lines(i).contains(" error: ") || lines(i).matches("""\[error\] -- .*Error:.*"""))
      val problems = starts.take(30).map: i =>
        val rest = lines.drop(i + 1).takeWhile(l => !l.contains(" error: ") && !l.matches("""\[error\] -- .*""") &&
          !l.startsWith("> Task") && !l.matches("""\d+ errors?""") && !l.startsWith("FAILURE")).take(8)
        val (file, line) = """(\S+?):(\d+):""".r.findFirstMatchIn(lines(i)).fold(("", 0))(m => (m.group(1), m.group(2).toInt))
        Json.Obj(
          "severity" -> Json.Str("ERROR"),
          "file" -> Json.Str(Option(project.relativize(Path.of(file).toAbsolutePath)).filter(_ => file.nonEmpty).fold("")(_.toString)),
          "line" -> Json.Num(line),
          "description" -> Json.Str((lines(i) +: rest).map(_.trim).filter(_.nonEmpty).mkString("\n")),
        )
      Right(Json.Obj("isSuccess" -> Json.Bool(ok), "problems" -> Json.Arr(problems*)).toJson)

  private def limit(params: Map[String, String]): Int = params.get("limit").flatMap(_.toIntOption).getOrElse(1000)

  /** The `paths` filter: a JSON array of globs on the command line (`--paths '["src/main/"]'`), or one glob. */
  private def paths(params: Map[String, String]): List[String] =
    params.get("paths").toList.flatMap: value =>
      value.fromJson[List[String]].getOrElse(List(value))
    .filter(_.nonEmpty)

  /** A project-relative glob as the real tools read one: a trailing `/` means `**` under it, no `/` means any depth. */
  private def glob(project: Path, pattern: String): java.nio.file.PathMatcher =
    val expanded = if pattern.endsWith("/") then pattern + "**" else if !pattern.contains("/") then s"**/$pattern" else pattern
    val matcher = project.getFileSystem.getPathMatcher(s"glob:$expanded")
    val top = if !pattern.contains("/") then Some(project.getFileSystem.getPathMatcher(s"glob:$pattern")) else None
    p => matcher.matches(p) || top.exists(_.matches(p))

  /** Whether a project-relative file passes the `paths` filter: some include matches (or there is none), no `!` exclude does. */
  private def included(project: Path, file: Path, filters: List[String]): Boolean =
    val (excludes, includes) = filters.partition(_.startsWith("!"))
    (includes.isEmpty || includes.exists(glob(project, _).matches(file))) && !excludes.exists(e => glob(project, e.drop(1)).matches(file))

  private def resolve(project: Path, path: String): Path =
    val p = Path.of(path)
    (if p.isAbsolute then p else project.resolve(path)).normalize

  /** The real `list_directory_tree`: the directory's absolute path, then entries `maxDepth - 1` levels deep. */
  private def tree(project: Path, directoryPath: String, maxDepth: Int): Either[String, String] =
    val dir = resolve(project, directoryPath)
    if !Files.exists(dir) then Left(s"No such directory: $dir")
    else if !Files.isDirectory(dir) then Left(s"Not a directory: $dir")
    else
      val out = StringBuilder(s"$dir/\n")
      def walk(d: Path, prefix: String, depth: Int): Unit =
        if depth < maxDepth then
          val children = Files.list(d).iterator().asScala.toList.sortBy(c => (!Files.isDirectory(c), c.getFileName.toString))
          children.zipWithIndex.foreach: (child, i) =>
            val last = i == children.size - 1
            val isDir = Files.isDirectory(child)
            out ++= s"$prefix${if last then "└── " else "├── "}${child.getFileName}${if isDir then "/" else ""}\n"
            if isDir then walk(child, prefix + (if last then "    " else "│   "), depth + 1)
      walk(dir, "    ", 1)
      Right(Json.Obj("traversedDirectory" -> Json.Str(directoryPath), "tree" -> Json.Str(out.result()), "errors" -> Json.Arr()).toJson)

  /**
   * The real `read_file`: `L<n>: <line>` lines from `offset` (1-based; the text after a final newline is a line too),
   * at most `limit` of them (5000 at most): beyond that the first half and the last half, the lines between replaced by
   * `…<n> lines truncated…`.
   */
  private def read(project: Path, filePath: String, offset: Int, limit: Int): Either[String, String] =
    val file = resolve(project, filePath)
    if !Files.isRegularFile(file) then Left(s"File $filePath doesn't exist or can't be opened")
    else
      val lines = Files.readString(file).split("\n", -1).toList.zipWithIndex.map((l, i) => s"L${i + 1}: $l")
      if offset < 1 then Left("offset must be > 0")
      else if offset > lines.size then Left("offset exceeds file length")
      else
        val shown = lines.drop(offset - 1)
        val max = limit.min(5000)
        if shown.size <= max then Right(shown.mkString("\n"))
        else
          val (head, tail) = (max / 2, max - max / 2)
          Right((shown.take(head) ++ List(s"…${shown.size - max} lines truncated…") ++ shown.takeRight(tail)).mkString("\n"))

  /** Directories the IDE excludes from search: its own, VCS metadata, and build output (sbt's `target`, at any depth). */
  private val Excluded = Set(".idea", ".bsp", ".git", "target")

  /** The project's text files the IDE searches: outside excluded directories, and readable as UTF-8. */
  private def projectFiles(project: Path): List[Path] =
    Files.walk(project).iterator().asScala
      .filter(p => Files.isRegularFile(p) && !project.relativize(p).iterator.asScala.exists(part => Excluded(part.toString)))
      .filter(p => scala.util.Try(java.nio.charset.StandardCharsets.UTF_8.newDecoder.decode(java.nio.ByteBuffer.wrap(Files.readAllBytes(p)))).isSuccess)
      .toList.sortBy(_.toString)

  /** The real `search_text`: `items` with each match's file and 1-based start and end (end exclusive). */
  private def searchText(project: Path, q: String, filters: List[String], limit: Int): Either[String, String] =
    val items = projectFiles(project).filter(f => included(project, project.relativize(f), filters)).flatMap: file =>
      Files.readAllLines(file).asScala.toList.zipWithIndex.flatMap: (line, i) =>
        Iterator.iterate(line.indexOf(q))(from => line.indexOf(q, from + 1)).takeWhile(_ >= 0).map: col =>
          Json.Obj(
            "filePath" -> Json.Str(project.relativize(file).toString),
            "startLine" -> Json.Num(i + 1), "startColumn" -> Json.Num(col + 1),
            "endLine" -> Json.Num(i + 1), "endColumn" -> Json.Num(col + 1 + q.length),
          )
    Right(Json.Obj("items" -> Json.Arr(items.take(limit)*)).toJson)

  /** The real `search_regex`: each match of a Java regex, per line, with coordinates; an invalid one matches nothing. */
  private def searchRegex(project: Path, q: String, filters: List[String], limit: Int): Either[String, String] =
    val pattern = scala.util.Try(java.util.regex.Pattern.compile(q)).toOption
    val items = pattern.toList.flatMap: p =>
      projectFiles(project).filter(f => included(project, project.relativize(f), filters)).flatMap: file =>
        Files.readAllLines(file).asScala.toList.zipWithIndex.flatMap: (line, i) =>
          val m = p.matcher(line)
          Iterator.continually(m).takeWhile(_.find()).map: m =>
            Json.Obj(
              "filePath" -> Json.Str(project.relativize(file).toString),
              "startLine" -> Json.Num(i + 1), "startColumn" -> Json.Num(m.start + 1),
              "endLine" -> Json.Num(i + 1), "endColumn" -> Json.Num(m.end + 1),
            )
          .toList
    Right(Json.Obj("items" -> Json.Arr(items.take(limit)*)).toJson)

  /** The real `search_file`: the project files matching a glob (one without `/` at any depth), with `paths` filtering. */
  private def searchFile(project: Path, q: String, filters: List[String], limit: Int): Either[String, String] =
    val matcher = glob(project, q)
    val items = projectFiles(project).map(project.relativize).filter(p => matcher.matches(p) && included(project, p, filters))
      .map(p => Json.Obj("filePath" -> Json.Str(p.toString)))
    Right(Json.Obj("items" -> Json.Arr(items.take(limit)*)).toJson)

  private val Definition = """\b(def|val|var|class|object|trait|enum|type|given)\s+([A-Za-z_][A-Za-z0-9_]*)""".r

  /**
   * The real `search_symbol`: the definitions (classes, methods, fields) whose name contains `q`, with coordinates; a
   * qualified `q` (`Greeter.greet`) matches the definitions named by its last part in a file defining the rest.
   */
  private def searchSymbol(project: Path, q: String, filters: List[String], limit: Int): Either[String, String] =
    val (container, name) = q.lastIndexOf('.') match
      case -1 => (None, q)
      case i  => (Some(q.take(i).split('.').last), q.drop(i + 1))
    val items = projectFiles(project).filter(f => included(project, project.relativize(f), filters)).flatMap: file =>
      val lines = Files.readAllLines(file).asScala.toList
      val defines = container.forall(c => lines.exists(l => Definition.findAllMatchIn(l).exists(_.group(2) == c)))
      if !defines then Nil
      else lines.zipWithIndex.flatMap: (line, i) =>
        Definition.findAllMatchIn(line).filter(_.group(2).contains(name)).map: m =>
          Json.Obj(
            "filePath" -> Json.Str(project.relativize(file).toString),
            "startLine" -> Json.Num(i + 1), "startColumn" -> Json.Num(m.start(2) + 1),
            "endLine" -> Json.Num(i + 1), "endColumn" -> Json.Num(m.end(2) + 1),
          )
    Right(Json.Obj("items" -> Json.Arr(items.take(limit)*)).toJson)
