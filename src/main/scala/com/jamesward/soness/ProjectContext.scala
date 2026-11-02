package com.jamesward.soness

import zio.*
import zio.stream.{ZPipeline, ZStream}

import java.nio.file.{Files, Path}

/**
 * What an agent should know about the project a request is about before it starts, as coding harnesses tell their
 * models: the session's working directory, its files (outside version-control, IDE and build-output directories), the
 * build files and languages they show, and the project's agent instructions (`AGENTS.md`). Read from the
 * session's `cwd` at each prompt, so it's current; a `cwd` soness can't read gives no context.
 */
object ProjectContext:

  /** At most this many files are listed; a larger project's listing says how many more there are. */
  val MaxFiles = 300

  /** Agent instructions are cut to this many characters. */
  val MaxInstructions = 6000

  /** Directories left out of the listing: version control, IDE metadata, dependencies and build output. */
  private[soness] val Skipped = Set(".git", ".hg", ".svn", ".idea", ".bsp", ".bloop", ".metals", ".vscode", ".gradle", ".kiro",
    ".claude", ".agents", "node_modules", "target", "build", "dist", "out", "__pycache__", ".venv", "venv", ".soness")

  /** What's never listed, even when build output is asked for: version control and IDE metadata, installed dependencies. */
  private[soness] val Unlisted = Set(".git", ".hg", ".svn", ".idea", ".bsp", ".bloop", ".metals", ".vscode", ".gradle",
    "node_modules", "__pycache__", ".venv", "venv")

  private val BuildFiles = List(
    "build.sbt" -> "sbt (Scala)", "pom.xml" -> "Maven", "build.gradle" -> "Gradle", "build.gradle.kts" -> "Gradle (Kotlin)",
    "package.json" -> "npm", "Cargo.toml" -> "Cargo (Rust)", "go.mod" -> "Go modules", "pyproject.toml" -> "Python",
    "requirements.txt" -> "Python (pip)", "Gemfile" -> "Bundler (Ruby)", "mix.exs" -> "Mix (Elixir)", "deno.json" -> "Deno",
  )

  private val Languages = Map(
    "scala" -> "Scala", "sc" -> "Scala", "java" -> "Java", "kt" -> "Kotlin", "kts" -> "Kotlin", "py" -> "Python",
    "js" -> "JavaScript", "mjs" -> "JavaScript", "jsx" -> "JavaScript", "ts" -> "TypeScript", "tsx" -> "TypeScript",
    "go" -> "Go", "rs" -> "Rust", "rb" -> "Ruby", "php" -> "PHP", "cs" -> "C#", "swift" -> "Swift", "c" -> "C",
    "h" -> "C", "cpp" -> "C++", "hpp" -> "C++", "ex" -> "Elixir", "clj" -> "Clojure", "hs" -> "Haskell",
  )

  /** The standard agent instructions file (https://agents.md). */
  private val InstructionFiles = List("AGENTS.md")

  /** What the agent instructions start with in the context (the writer's prompts leave them out: [[WritingPrompts.project]]). */
  val InstructionsHeading = "\n\nThe project's agent instructions ("

  /** The project's agent instructions (`AGENTS.md` at `cwd`), whole; empty when there are none. */
  def instructions(cwd: Path): UIO[String] =
    ZIO.foreach(InstructionFiles.map(cwd.resolve))(f => ZIO.whenZIO(ZIO.attemptBlocking(Files.isRegularFile(f)))(readText(f)))
      .map(_.flatten.mkString("\n\n")).orElseSucceed("")

  /** MCP server instructions are cut to this many characters each. */
  val MaxServerInstructions = 4000

  /**
   * The context with what each MCP server said about using its tools (its `initialize` `instructions`), as the
   * project's agent instructions are: base knowledge for every decision and every writing.
   */
  def withServers(context: Option[String], servers: List[(String, String)]): Option[String] =
    if servers.isEmpty then context
    else
      val notes = servers.map: (name, text) =>
        val cut = if text.length > MaxServerInstructions then text.take(MaxServerInstructions) + "\n… (cut)" else text
        s"The MCP server $name's instructions (for its tools):\n$cut"
      Some((context.toList ++ notes).mkString("\n\n"))

  /** The context of the project at `cwd`, or None when it can't be read. */
  def describe(cwd: Path): UIO[Option[String]] =
    (for
      root   <- ZIO.attemptBlocking(cwd.toRealPath())
      files  <- ZIO.attemptBlocking(listFiles(root))
      notes  <- ZIO.foreach(InstructionFiles.filter(f => files.contains(f)))(f => readText(root.resolve(f)).map(f -> _))
      deps   <- ZIO.attemptBlocking(SourceDependencies.of(root))
    yield Option.when(files.nonEmpty || notes.nonEmpty)(render(root, files, notes) + SourceDependencies.describe(deps).fold("")("\n" + _)))
      .catchAll(e => ZIO.logDebug(s"no project context for $cwd: ${e.getMessage}").as(None))

  private def render(root: Path, files: List[String], notes: List[(String, String)]): String =
    val builds = BuildFiles.collect { case (file, tool) if files.exists(f => f == file || f.endsWith(s"/$file")) => s"$tool ($file)" }
    val languages = files.flatMap(f => f.split('.').lastOption.filter(_ => f.contains('.')).flatMap(e => Languages.get(e.toLowerCase)))
      .groupBy(identity).toList.sortBy(-_._2.size).map((language, all) => s"$language (${all.size} files)")
    val listed = files.take(MaxFiles).mkString("\n")
    val more = if files.size > MaxFiles then s"\n… and ${files.size - MaxFiles} more files" else ""
    val instructions = notes.map: (file, text) =>
      val cut = if text.length > MaxInstructions then text.take(MaxInstructions) + "\n… (cut)" else text
      s"$InstructionsHeading$file):\n$cut"
    s"""Project directory: $root
       |Build: ${if builds.isEmpty then "none recognized" else builds.mkString(", ")}
       |Languages: ${if languages.isEmpty then "none recognized" else languages.mkString(", ")}
       |Files (paths relative to the project directory; version-control, IDE and build-output directories left out):
       |$listed$more${instructions.mkString}""".stripMargin

  /** Walking stops after this many files (a listing cuts to [[MaxFiles]] anyway). */
  private val MaxWalked = 5000

  /** The regular files under `root`, relative and sorted, not descending into [[Skipped]] directories. */
  private def listFiles(root: Path): List[String] =
    val found = List.newBuilder[String]
    var count = 0
    Files.walkFileTree(root, new java.nio.file.SimpleFileVisitor[Path]:
      override def preVisitDirectory(dir: Path, attrs: java.nio.file.attribute.BasicFileAttributes) =
        if !dir.equals(root) && Skipped(dir.getFileName.toString) then java.nio.file.FileVisitResult.SKIP_SUBTREE
        else java.nio.file.FileVisitResult.CONTINUE
      override def visitFile(file: Path, attrs: java.nio.file.attribute.BasicFileAttributes) =
        if attrs.isRegularFile then
          found += root.relativize(file).toString.replace(java.io.File.separatorChar, '/')
          count += 1
        if count >= MaxWalked then java.nio.file.FileVisitResult.TERMINATE else java.nio.file.FileVisitResult.CONTINUE
      override def visitFileFailed(file: Path, e: java.io.IOException) = java.nio.file.FileVisitResult.CONTINUE
    )
    found.result().sorted

  private def readText(file: Path): Task[String] =
    ZStream.fromPath(file).via(ZPipeline.utf8Decode).runCollect.map(_.mkString)
