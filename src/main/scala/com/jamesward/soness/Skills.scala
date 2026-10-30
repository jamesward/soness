package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.{Document, DocumentContent}
import zio.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Skills in the [Agent Skills](https://agentskills.io) layout, found in a session's workspace roots (its `cwd` and
 * additional directories) and the user's home, in the locations the Agent Skills client guide names
 * (https://agentskills.io/client-implementation/adding-skills-support): `.agents/skills/<name>/SKILL.md`, the
 * cross-client convention, and `.claude/skills/<name>/SKILL.md`, for compatibility. A project's skill wins over the
 * user's of the same name.
 * Each skill is a document Jev can read, titled and described by its front matter; reading it loads the whole file, and
 * the files it links to inside its own directory (`[reference/tools.md](reference/tools.md)`) become documents in turn,
 * each described by the line it is linked from. So a skill's references load progressively, one file at a time.
 */
object Skills:

  private val skillDirectories = List(".agents/skills", ".claude/skills")

  /** The metadata key a skill the project's instructions name carries ([[required]]). */
  val Required = "required"

  /**
   * `skills`, with those the project's agent instructions name (`Follow the zen-of-scala skill`, `` `zen-of-scala` ``)
   * marked [[Required]]: when Jev chooses which skills a request needs (zio-decisions' `SkillChoice`, from names and
   * descriptions only), these are marked as the ones the instructions say to follow. A name matches as a whole word, case
   * aside.
   */
  def required(skills: List[Document], instructions: String): List[Document] =
    val text = instructions.toLowerCase
    skills.map: skill =>
      val name = java.util.regex.Pattern.quote(skill.title.toLowerCase)
      if s"(?<![\\w-])$name(?![\\w-])".r.findFirstIn(text).isDefined then skill.copy(metadata = skill.metadata.updated(Required, "true"))
      else skill

  /** Whether the project's instructions name `skill` ([[required]]). */
  def isRequired(skill: Document): Boolean = skill.metadata.get(Required).contains("true")

  /** The skills under `roots`, ordered by root and then name; a skill found under several roots is listed once. */
  def discover(roots: Seq[Path]): UIO[List[Document]] =
    ZIO.foreach(roots.distinct.toList)(skillsUnder).map(_.flatten.distinctBy(_.title))

  private def skillsUnder(root: Path): UIO[List[Document]] =
    ZIO.foreach(skillDirectories.map(root.resolve)): dir =>
      list(dir).flatMap(entries => ZIO.foreach(entries.sorted)(skill).map(_.flatten))
    .map(_.flatten)

  private def list(dir: Path): UIO[List[Path]] =
    ZIO.attemptBlocking(if Files.isDirectory(dir) then Files.list(dir).iterator().asScala.toList else Nil)
      .catchAll(e => ZIO.logDebug(s"skills: can't list $dir: ${e.getMessage}").as(Nil))

  private def skill(dir: Path): UIO[Option[Document]] =
    (for
      home <- ZIO.attemptBlocking(dir.toRealPath())
      // a directory without a SKILL.md (another client's own, such as Claude's `synced`) isn't a skill
      _    <- ZIO.attemptBlocking(Files.isRegularFile(home.resolve("SKILL.md"))).filterOrFail(identity)(java.util.NoSuchElementException())
      text <- readText(home.resolve("SKILL.md"))
    yield
      val (front, _) = frontMatter(text)
      val name = front.getOrElse("name", dir.getFileName.toString)
      Document(
        s"skill:$name",
        name,
        front.getOrElse("description", s"The $name skill"),
        content(name, home, home.resolve("SKILL.md")),
        front.removedAll(List("name", "description")),
      )
    ).option

  /** A file's text and the files of its skill it links to. */
  private def content(skill: String, home: Path, file: Path): Task[DocumentContent] =
    readText(file).map: text =>
      val linked = links(text).flatMap: (target, title, line) =>
        val resolved = file.getParent.resolve(target).normalize
        Option.when(resolved.startsWith(home) && Files.isRegularFile(resolved)):
          Document(
            s"skill:$skill/${home.relativize(resolved)}",
            s"$skill: $title",
            line,
            content(skill, home, resolved),
          )
      DocumentContent(text, linked.distinctBy(_.id))

  private val Link = """\[([^\]]+)\]\(([^)\s#]+)(#[^)]*)?\)""".r

  /** Relative links in `text`: target, link text, and the line it is on (the link's description). */
  private def links(text: String): List[(String, String, String)] =
    text.linesIterator.toList.flatMap: line =>
      Link.findAllMatchIn(line).toList.collect:
        case m if !m.group(2).contains("://") && !m.group(2).startsWith("/") && !m.group(2).startsWith("mailto:") =>
          (m.group(2), m.group(1), line.trim.stripPrefix("- ").take(300))

  /** The `key: value` pairs of a `---` front matter block, and the text after it. */
  def frontMatter(text: String): (Map[String, String], String) =
    val lines = text.linesIterator.toList
    lines match
      case "---" :: rest =>
        val (block, after) = rest.span(_.trim != "---")
        val pairs = block.flatMap: line =>
          line.split(":", 2) match
            case Array(key, value) if !key.startsWith(" ") && key.trim.nonEmpty =>
              Some(key.trim -> value.trim.stripPrefix("\"").stripSuffix("\"").stripPrefix("'").stripSuffix("'"))
            case _ => None
        (pairs.toMap, after.drop(1).mkString("\n"))
      case _ => (Map.empty, text)

  private def readText(file: Path): Task[String] =
    ZIO.attemptBlocking(Files.readString(file, java.nio.charset.StandardCharsets.UTF_8))
