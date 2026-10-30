package com.jamesward.soness

import java.nio.file.{Files, Path}

/**
 * IntelliJ IDEA 2026.2.3's file-editing tools as its router runs them, on a project on disk; the behavior and wording are
 * the real server's (checked against a headless IDE): `apply_patch` takes a Codex `apply_patch` patch or a unified diff,
 * applies each file operation it can and reports the rest in a successful result ("1 out of 2 operations applied. 1
 * operations failed to be applied due to errors: …"), matching a hunk's lines exactly, then ignoring trailing, then
 * surrounding whitespace; `create_new_file` refuses to replace a file unless `overwrite` is true. soness runs them
 * itself on files the IDE can't reach (a source dependency's, [[LocalTools.reaching]]); the tests' mock router runs them
 * on its project.
 */
object IntellijPatch:

  /** The files a patch (`input`, Codex or unified) works on, as written: each operation's path and its move target. */
  def paths(input: String): List[String] =
    val lines = input.replace("\r\n", "\n").split("\n", -1).toList
    val ops = if lines.exists(_.trim == "*** Begin Patch") then codex(lines) else unified(lines)
    ops.flatMap:
      case Operation.Update(path, moveTo, _) => path :: moveTo.toList
      case op                                => List(op.path)
    .filterNot(_ == "/dev/null")

  private enum Operation(val path: String):
    case Add(override val path: String, lines: List[String]) extends Operation(path)
    case Delete(override val path: String) extends Operation(path)
    case Update(override val path: String, moveTo: Option[String], hunks: List[Hunk]) extends Operation(path)

  /** A hunk: the lines to find (context and removed) and what replaces them (context and added), after `anchor`. */
  private final case class Hunk(anchor: Option[String], before: List[String], after: List[String])

  /** `apply_patch` with its `input` (or its alias `patch`). */
  def apply(project: Path, params: Map[String, String]): Either[String, String] =
    params.get("input").orElse(params.get("patch")).filter(_.trim.nonEmpty) match
      case None => Left("input must be a non-empty string")
      case Some(input) =>
        val lines = input.replace("\r\n", "\n").split("\n", -1).toList
        val operations =
          if lines.exists(_.trim == "*** Begin Patch") then Right(codex(lines))
          else if lines.exists(_.startsWith("--- ")) && lines.exists(_.startsWith("+++ ")) then Right(unified(lines))
          else Left("patch must include *** Begin Patch")
        operations.map: ops =>
          val errors = ops.flatMap(op => run(project, op).left.toOption.map(e => s"${op.path}: $e"))
          val applied = ops.size - errors.size
          if errors.isEmpty then s"$applied out of ${ops.size} operations applied."
          else s"$applied out of ${ops.size} operations applied. ${errors.size} operations failed to be applied due to errors:\n" +
            errors.mkString("\n")

  /** `create_new_file`: `pathInProject`, its `text`, replacing an existing file only with `overwrite`. */
  def create(project: Path, params: Map[String, String]): Either[String, String] =
    params.get("pathInProject") match
      case None => Left("Missing required parameters: pathInProject")
      case Some(path) =>
        val file = project.resolve(path).normalize
        if Files.exists(file) && !params.get("overwrite").contains("true") then
          Left(s"File already exists: $path. Specify 'overwrite=true' to overwrite it")
        else
          Files.createDirectories(file.getParent)
          Files.writeString(file, params.getOrElse("text", ""))
          Right("[success]")

  private def codex(lines: List[String]): List[Operation] =
    val body = lines.dropWhile(_.trim != "*** Begin Patch").drop(1).takeWhile(_.trim != "*** End Patch")
    def header(line: String) = line.startsWith("*** Add File: ") || line.startsWith("*** Delete File: ") ||
      line.startsWith("*** Update File: ")
    def loop(rest: List[String]): List[Operation] = rest match
      case Nil => Nil
      case line :: more if line.startsWith("*** Add File: ") =>
        val (content, next) = more.span(l => !header(l))
        Operation.Add(line.stripPrefix("*** Add File: ").trim, content.filter(_.startsWith("+")).map(_.drop(1))) :: loop(next)
      case line :: more if line.startsWith("*** Delete File: ") =>
        Operation.Delete(line.stripPrefix("*** Delete File: ").trim) :: loop(more)
      case line :: more if line.startsWith("*** Update File: ") =>
        val (content, next) = more.span(l => !header(l))
        val moveTo = content.headOption.filter(_.startsWith("*** Move to: ")).map(_.stripPrefix("*** Move to: ").trim)
        Operation.Update(line.stripPrefix("*** Update File: ").trim, moveTo, hunks(content.drop(moveTo.size), "@@")) :: loop(next)
      case _ :: more => loop(more)
    loop(body)

  private def unified(lines: List[String]): List[Operation] =
    def path(line: String) = line.drop(4).trim.split("\t").head.replaceFirst("^[ab]/", "")
    def loop(rest: List[String]): List[Operation] = rest match
      case minus :: plus :: more if minus.startsWith("--- ") && plus.startsWith("+++ ") =>
        val (content, next) = more.span(l => !l.startsWith("--- "))
        val (from, to) = (path(minus), path(plus))
        val op =
          if from == "/dev/null" then Operation.Add(to, content.filter(l => l.startsWith("+")).map(_.drop(1)))
          else if to == "/dev/null" then Operation.Delete(from)
          else Operation.Update(from, Option.when(to != from)(to), hunks(content, "@@"))
        op :: loop(next)
      case _ :: more => loop(more)
      case Nil       => Nil
    loop(lines)

  /** The hunks of a file operation: each starts at a line beginning with `marker` (Codex's may name an anchor line). */
  private def hunks(content: List[String], marker: String): List[Hunk] =
    def hunk(anchor: Option[String], body: List[String]) =
      // a patch's final newline isn't a blank context line
      val kept = body.filterNot(l => l.startsWith("\\") || l.trim == "*** End of File").reverse.dropWhile(_.isEmpty).reverse
      Hunk(
        anchor.filter(_.nonEmpty),
        kept.collect { case l if l.startsWith(" ") || l.startsWith("-") => l.drop(1); case "" => "" },
        kept.collect { case l if l.startsWith(" ") || l.startsWith("+") => l.drop(1); case "" => "" },
      )
    def loop(rest: List[String]): List[Hunk] = rest match
      case line :: more if line.startsWith(marker) =>
        val (body, next) = more.span(l => !l.startsWith(marker))
        val anchor = line.stripPrefix(marker).trim
        // a unified diff's header has line numbers, not an anchor
        hunk(Option.when(!anchor.startsWith("-"))(anchor.stripSuffix(marker).trim), body) :: loop(next)
      case Nil => Nil
      case body =>
        val (first, next) = body.span(l => !l.startsWith(marker))
        hunk(None, first) :: loop(next)
    loop(content).filter(h => h.before.nonEmpty || h.after.nonEmpty)

  private def run(project: Path, op: Operation): Either[String, Unit] =
    val file = project.resolve(op.path).normalize
    op match
      case Operation.Add(path, lines) =>
        if Files.exists(file) then Left(s"File already exists: $path")
        else
          Files.createDirectories(file.getParent)
          Right(Files.writeString(file, lines.mkString("\n") + "\n"))
      case Operation.Delete(path) =>
        if !Files.isRegularFile(file) then Left(s"File not found: $path") else Right(Files.delete(file))
      case Operation.Update(path, moveTo, hunks) =>
        if !Files.isRegularFile(file) then Left(s"File not found: $path")
        else
          val text = Files.readString(file)
          val newline = text.endsWith("\n")
          val lines = (if newline then text.dropRight(1) else text).split("\n", -1).toList
          hunks.foldLeft[Either[String, (List[String], Int)]](Right((lines, 0))):
            case (Right((current, from)), hunk) => replace(current, from, hunk).toRight("Hunk context not found")
            case (failed, _)                    => failed
          .map: (updated, _) =>
            val target = moveTo.map(m => project.resolve(m).normalize).getOrElse(file)
            Files.createDirectories(target.getParent)
            Files.writeString(target, updated.mkString("\n") + (if newline then "\n" else ""))
            if !target.equals(file) then Files.delete(file)

  /** The lines with the hunk applied at its first match from `from` (after its anchor), and where the match ended. */
  private def replace(lines: List[String], from: Int, hunk: Hunk): Option[(List[String], Int)] =
    val start = hunk.anchor.flatMap(a => (from until lines.size).find(i => lines(i).trim == a.trim)).map(_ + 1).getOrElse(from)
    val equalities: List[(String, String) => Boolean] = List(_ == _, _.stripTrailing == _.stripTrailing, _.trim == _.trim)
    if hunk.before.isEmpty then Some((lines.patch(lines.size, hunk.after, 0), lines.size + hunk.after.size))
    else
      equalities.view.flatMap: same =>
        (start to lines.size - hunk.before.size).find(i => hunk.before.indices.forall(j => same(lines(i + j), hunk.before(j))))
      .headOption.map(i => (lines.patch(i, hunk.after, hunk.before.size), i + hunk.after.size))
