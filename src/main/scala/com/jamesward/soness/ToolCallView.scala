package com.jamesward.soness

import com.jamesward.zioacp.schema.*
import zio.json.ast.Json

/**
 * How a tool call is shown to the ACP client (its `tool_call` and `tool_call_update` updates): its kind (read, edit,
 * search, execute, fetch, think, other: what clients pick icons by), a title that says what it does ("Read
 * src/Main.java", "Run ./gradlew test"), the file locations it works with (for follow-along), and, for an edit that
 * says what it changes, the change as a diff. Read from the tool's name, annotations and arguments, for any toolset.
 */
object ToolCallView:

  /** The tool's kind, by its annotations and name. */
  def kind(tool: String, annotations: Option[Json.Obj] = None): ToolKind =
    val n = tool.toLowerCase
    def has(words: String*) = words.exists(n.contains)
    if has("search", "find", "grep", "glob", "list_directory", "symbol") then ToolKind.Search
    else if has("fetch", "http", "web") then ToolKind.Fetch
    else if has("delete", "remove") then ToolKind.Delete
    else if has("rename", "move") then ToolKind.Move
    // "editor" isn't an edit: open_file_in_editor shows a file to the user and changes nothing
    else if has("patch", "write", "create", "replace", "reformat", "insert") || n.replace("editor", "").contains("edit") then ToolKind.Edit
    else if has("terminal", "command", "exec", "run", "build", "test", "problems", "lint", "shell") then ToolKind.Execute
    else if has("read", "open", "get", "view", "cat", "show", "info") then ToolKind.Read
    else if annotations.exists(_.get("readOnlyHint").contains(Json.Bool(true))) then ToolKind.Read
    else ToolKind.Other

  /**
   * The call's arguments as the client is shown them (ACP's `rawInput`). An execute call whose arguments name no
   * `command` (a build server's `test` with its `tests`) gets one, the call as a command line (`test
   * com.example.FooSpec`): IntelliJ IDEA shows an execute call as a terminal block with `rawInput.command` as its
   * command, and "Command unavailable" without one. The call itself is made with the arguments as they are.
   */
  def shownInput(tool: String, arguments: Json.Obj, annotations: Option[Json.Obj] = None): Json.Obj =
    val named = arguments.get("command").exists:
      case Json.Str(c) => c.trim.nonEmpty
      case Json.Arr(cs) => cs.nonEmpty
      case _ => false
    if kind(tool, annotations) != ToolKind.Execute || named then arguments
    else
      def value(j: Json): List[String] = j match
        case Json.Str(s)  => List(s)
        case Json.Arr(xs) => xs.toList.flatMap(value)
        case Json.Null    => Nil
        case other        => List(other.toString)
      val words = arguments.fields.toList.flatMap: (k, v) =>
        v match
          case Json.Bool(true)            => List(s"--$k")
          case Json.Bool(false) | Json.Null => Nil
          // the arguments a command is about (`tests`, `project`, `task`) as they are; the rest as options
          case _ if Set("tests", "test", "task", "project", "target", "args", "arguments")(k) => value(v)
          case _                          => s"--$k" :: value(v)
      Json.Obj((("command" -> (Json.Str((ToolResolver.baseName(tool) :: words).mkString(" ")): Json)) +: arguments.fields)*)

  /** The file paths the call works with: path-like arguments, and the files a Codex patch names. */
  def locations(arguments: Json.Obj): List[ToolCallLocation] =
    val keys = Set("path", "file_path", "filepath", "pathinproject", "file", "filename", "directorypath", "uri")
    val direct = arguments.fields.toList.collect:
      case (k, Json.Str(v)) if keys(k.toLowerCase.replace("_", "")) || keys(k.toLowerCase) => v
    val lines = arguments.fields.toList.collectFirst { case (k, Json.Num(v)) if k.toLowerCase.contains("line") => v.longValue }
    val patched = arguments.fields.toList.collect { case (_, Json.Str(v)) if v.contains("*** ") => v }.flatMap: patch =>
      """(?m)^\*\*\* (?:Update|Add|Delete) File: (.+)$""".r.findAllMatchIn(patch).map(_.group(1).trim).toList
    (direct ++ patched).filter(_.nonEmpty).distinct.take(8).map(p => ToolCallLocation(p, if direct.contains(p) then lines else None))

  /** A title for the call: what it does, in a few words. */
  def title(tool: String, arguments: Json.Obj, annotations: Option[Json.Obj] = None): String =
    def str(keys: String*) = keys.iterator.flatMap(k => arguments.get(k).flatMap(_.asString)).nextOption()
    val where = locations(arguments).map(_.path)
    kind(tool, annotations) match
      case ToolKind.Execute =>
        str("command", "cmd").fold(s"$tool")(c => s"Run ${c.take(80)}")
      case ToolKind.Search =>
        str("q", "query", "pattern", "nameKeyword", "symbol").fold(tool)(q => s"Search for ${q.take(60)}") +
          where.headOption.fold("")(w => s" in $w")
      case ToolKind.Fetch => str("url").fold(tool)(u => s"Fetch ${u.take(80)}")
      case ToolKind.Edit  => where.headOption.fold(tool)(w => s"Edit ${where.take(3).mkString(", ")}")
      case ToolKind.Read  => where.headOption.fold(tool)(w => s"Read $w")
      case _              => tool

  /**
   * The title as a client that names the kind itself shows it (IntelliJ IDEA writes "Edited", "Read", "Fetched" before
   * the title): without the verb, so it doesn't say it twice ("Edited Edit src/A.scala"). A command keeps its own, and a
   * call of an unknown kind its tool's name.
   */
  def bareTitle(tool: String, arguments: Json.Obj, annotations: Option[Json.Obj] = None): String =
    val t = title(tool, arguments, annotations)
    List("Edit ", "Read ", "Fetch ", "Search for ").find(t.startsWith).fold(t)(v => t.drop(v.length))

  /** The locations with their paths absolute, as ACP's `ToolCallLocation` wants them (a client opens them by it). */
  def absolute(locations: List[ToolCallLocation], cwd: Option[java.nio.file.Path]): List[ToolCallLocation] =
    locations.map: l =>
      val p = l.path.stripPrefix("file://")
      cwd.filter(_ => !java.nio.file.Paths.get(p).isAbsolute).fold(l.copy(path = p))(root => l.copy(path = root.resolve(p).normalize.toString))

  /**
   * The change an edit makes, as ACP diffs, when its arguments say it whole: a new file's text, or a Codex patch's
   * added files. A patch's update hunks are shown as the patch itself (no old file is read here).
   */
  /**
   * What an edit changed, from the files as they were before it and as they are after (path, text before, text
   * after; none for a file that didn't exist): a diff per file that changed, and a line saying how much each did
   * (`src/A.scala: +3 −1`). A tool's own result (IntelliJ's "4 out of 4 operations applied.", "[success]") says
   * neither what nor where.
   */
  def changes(files: List[(String, Option[String], Option[String])]): (List[ToolCallContent], String) =
    val changed = files.filter((_, before, after) => before != after)
    val diffs = changed.map((p, before, after) => ToolCallContent.Diff(p, after.getOrElse(""), before))
    val summary = changed.map: (p, before, after) =>
      val (added, removed) = lineCounts(before.getOrElse(""), after.getOrElse(""))
      val what = (before, after) match
        case (None, Some(_)) => s"created, ${after.get.linesIterator.size} lines"
        case (Some(_), None) => "deleted"
        case _               => s"+$added −$removed"
      s"$p: $what"
    (diffs, if changed.isEmpty then "no file changed on disk" else summary.mkString("\n"))

  /** Lines added and removed between two texts: what's left of each after their common lines are taken out. */
  def lineCounts(before: String, after: String): (Int, Int) =
    val b = before.linesIterator.toVector
    val a = after.linesIterator.toVector
    // the common prefix and suffix, then the rest compared as multisets
    val prefix = b.zip(a).takeWhile(_ == _).size
    val suffix = b.drop(prefix).reverse.zip(a.drop(prefix).reverse).takeWhile(_ == _).size
    val (bm, am) = (b.slice(prefix, b.size - suffix), a.slice(prefix, a.size - suffix))
    val common = bm.groupBy(identity).map((l, ls) => ls.size min am.count(_ == l)).sum
    (am.size - common, bm.size - common)

  def diffs(tool: String, arguments: Json.Obj): List[ToolCallContent] =
    val path = List("pathInProject", "path", "file_path").flatMap(k => arguments.get(k).flatMap(_.asString)).headOption
    val text = List("text", "content", "newText").flatMap(k => arguments.get(k).flatMap(_.asString)).headOption
    (path, text) match
      case (Some(p), Some(t)) if kind(tool) == ToolKind.Edit => List(ToolCallContent.Diff(p, t, None))
      case _ =>
        arguments.fields.toList.collect { case (_, Json.Str(v)) if v.contains("*** Begin Patch") => v }.flatMap: patch =>
          """(?s)\*\*\* Add File: ([^\n]+)\n(.*?)(?=\n\*\*\* |\z)""".r.findAllMatchIn(patch).map: m =>
            val added = m.group(2).linesIterator.filter(_.startsWith("+")).map(_.drop(1)).mkString("\n")
            ToolCallContent.Diff(m.group(1).trim, added, None)
          .toList
