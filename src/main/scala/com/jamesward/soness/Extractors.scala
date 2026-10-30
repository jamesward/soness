package com.jamesward.soness

import zio.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.matching.Regex

/**
 * Extractors: tools the agent calls to take what it needs of a large file, instead of having the file cut for it. A
 * read is never cut by soness; to look at less than a whole file, the agent asks for it, by
 *
 *  - `extract_lines`: a range of lines;
 *  - `grep_file`: the lines that match a regular expression, each with the lines around it;
 *  - `outline_file`: the file's declarations and headings, each with its line number, to choose the ranges to read.
 *
 * Every line comes with its number, so a later extract can name the lines around it. Paths are the project's and its
 * source dependencies', as [[LocalTools]]' are.
 */
object Extractors:

  /** A line of a text: its number (1-based) and its text. */
  final case class Line(number: Int, text: String) derives CanEqual:
    def shown: String = f"$number%6d\t$text"

  /** A run of a text's lines, from `from` to `to`, both included (1-based). */
  final case class Region(from: Int, to: Int) derives CanEqual:
    def contains(n: Int): Boolean = n >= from && n <= to
    /** Whether `next`, which starts no earlier, overlaps this region or follows it at once. */
    def joins(next: Region): Boolean = next.from <= to + 1

  /** A text's lines, numbered. */
  def numbered(text: String): Vector[Line] =
    text.linesIterator.toVector.zipWithIndex.map((l, i) => Line(i + 1, l))

  /** The lines of `wanted`. */
  def region(lines: Vector[Line], wanted: Region): Vector[Line] = lines.filter(l => wanted.contains(l.number))

  /** The regions around the lines that match `pattern`, `context` lines on each side, overlapping ones joined. */
  def matching(lines: Vector[Line], pattern: Regex, context: Int): List[Region] =
    val around = lines.filter(l => pattern.findFirstIn(l.text).isDefined)
      .map(l => Region((l.number - context).max(1), (l.number + context).min(lines.size)))
    around.foldLeft(List.empty[Region]):
      case (last :: done, r) if last.joins(r) => Region(last.from, last.to.max(r.to)) :: done
      case (done, r)                          => r :: done
    .reverse

  /** A declaration (Scala, Java, Kotlin) or a Markdown heading. */
  private val Declaration =
    """^\s*(?:(?:private|protected|public|final|sealed|abstract|override|implicit|lazy|inline|case|static|open|data)\b\S*\s+)*(?:def|val|var|class|object|trait|enum|type|given|extension|interface|record|fun)\b|^#{1,6}\s""".r

  /** The lines that declare something or head a section. */
  def outline(lines: Vector[Line]): Vector[Line] = lines.filter(l => Declaration.findFirstIn(l.text).isDefined)

  /** Lines as the tools give them: each its number, a tab, then the line. */
  def render(lines: Vector[Line]): String = lines.map(_.shown).mkString("\n")

  /** Regions of `lines`, each rendered, with `…` between them. */
  def render(lines: Vector[Line], regions: List[Region]): String =
    regions.map(r => render(region(lines, r))).mkString("\n…\n")

  private def schema(required: List[String], properties: (String, String, String)*): Json.Obj =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(properties.map((n, t, d) => n -> (Json.Obj("type" -> Json.Str(t), "description" -> Json.Str(d)): Json))*),
      "required" -> Json.Arr(required.map(Json.Str(_))*),
    )

  private val readOnly = Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))

  private val file = ("path", "string", "The file, relative to the project (a source dependency's by its absolute path)")

  val ExtractLines = ToolSpec("extract_lines", "Gives lines `from` to `to` of a text file, each as its number, a tab, then the line: " +
      "the part of a large file a step needs, whole lines, nothing cut.",
    schema(List("path", "from", "to"), file, ("from", "integer", "The first line (1-based)"), ("to", "integer", "The last line, included")),
    None, readOnly)
  val GrepFile = ToolSpec("grep_file", "Gives the lines of one text file that match a regular expression (Java syntax; (?i) for " +
      "any case), each with `context` lines around it (default 3) and every line's number: to find where in a large file " +
      "something is, and read it there.",
    schema(List("path", "pattern"), file, ("pattern", "string", "The regular expression"),
      ("context", "integer", "Lines to show before and after each match (default 3)")), None, readOnly)
  val OutlineFile = ToolSpec("outline_file", "Gives a text file's outline: its declarations (classes, objects, methods, values) and " +
      "headings, each with its line number, and the file's length in lines: to choose which lines of a large file to extract.",
    schema(List("path"), file), None, readOnly)

  val DefaultContext = 3

  /** The extractors over the project at `root` and its source `dependencies`. */
  def toolset(root: Path, dependencies: List[Path] = Nil): Toolset =
    val base = root.toAbsolutePath.normalize
    val roots = dependencies.map(_.toAbsolutePath.normalize)
    new Toolset:
      val tools: Chunk[ToolSpec] = Chunk(ExtractLines, GrepFile, OutlineFile)
      def call(name: String, arguments: Json.Obj): Task[Json] =
        def int(k: String) = arguments.get(k).flatMap(_.asNumber).map(_.value.intValue)
        def need[A](k: String, value: Option[A]) = ZIO.fromOption(value).orElseFail(IllegalArgumentException(s"$k is required"))
        val lines = need("path", arguments.get("path").flatMap(_.asString)).flatMap(p => LocalTools.inside(base, p, roots)).flatMap(read)
        name match
          case "extract_lines" =>
            (need("from", int("from")) <*> need("to", int("to")) <*> lines).map: (from, to, all) =>
              Json.Obj("text" -> Json.Str(render(region(all, Region(from, to)))), "lines" -> Json.Num(all.size))
          case "grep_file" =>
            for
              pattern <- need("pattern", arguments.get("pattern").flatMap(_.asString))
              regex   <- ZIO.attempt(pattern.r).mapError(e => IllegalArgumentException(s"bad pattern: ${e.getMessage}"))
              all     <- lines
              regions  = matching(all, regex, int("context").getOrElse(DefaultContext).max(0))
            yield Json.Obj("text" -> Json.Str(render(all, regions)), "regions" -> Json.Num(regions.size), "lines" -> Json.Num(all.size))
          case "outline_file" =>
            lines.map(all => Json.Obj("text" -> Json.Str(render(outline(all))), "lines" -> Json.Num(all.size)))
          case other => ZIO.fail(IllegalArgumentException(s"unknown tool: $other"))

  private def read(file: Path): Task[Vector[Line]] =
    LocalTools.missing(file).flatMap(ZIO.foreachDiscard(_)(why => ZIO.fail(IllegalArgumentException(why)))) *>
      ZIO.attemptBlocking(numbered(Files.readString(file, UTF_8)))
