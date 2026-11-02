package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.{Schema, derived}

/**
 * A session's tools, resolved by capability (TOOLS_SPEC.md): every MCP server's tool is always offered; soness's own
 * (the ACP client's files and terminal, [[LocalTools]], `web_fetch`) are implicit, offered only for a capability no
 * better source covers. A shell of soness's own is offered only when some required capability is covered by nothing
 * else and no MCP server has a shell, its description naming what it's for.
 */
object ToolResolver:

  /** The capabilities every session should have. */
  /**
   * What a session's tools can do. `Command` is running a program or a command line (to measure, reproduce, or look at
   * what's outside the project's sources: logs, build output): only a shell covers it.
   */
  enum Capability derives Schema, CanEqual:
    case WebSearch, WebFetch, FileRead, FileWrite, FilePatch, FileFind, DirectoryList, TextSearch, Build, Command

  /** Capabilities a session may lack without a shell standing in for them. */
  val AllowedGaps: Set[Capability] = Set(Capability.WebSearch, Capability.DirectoryList)

  enum ToolSource derives CanEqual:
    /** An IDE's MCP server (a JetBrains IDE's, direct or routed). */
    case IntellijMcp
    /** A server of the project's `.mcp.json`. */
    case ProjectMcp
    /** A server the client gave the session. */
    case UserMcp
    /** The ACP client's files and terminal ([[ClientTools]]). */
    case AcpClient
    /** soness's own file tools and command ([[LocalTools]]). */
    case Local
    /** soness's built-in tools (`web_fetch`). */
    case Builtin

    def alwaysOffered: Boolean = this == IntellijMcp || this == ProjectMcp || this == UserMcp

  /** A tool from a source: its spec, its source, the family the catalog knows it by, and what serves it. */
  final case class Candidate(spec: ToolSpec, source: ToolSource, family: String, via: Toolset):
    def name: String = spec.name

  /** What resolution decided: each covered capability's tool, the tools offered, those hidden (and by what), the shell's job. */
  final case class Resolution(
    coverage: Map[Capability, Candidate],
    offered: List[Candidate],
    hidden: List[(Candidate, String)],
    fallback: Option[Set[Capability]],
  ):
    /** The offered tools as one toolset: a call goes to the source that offered the tool. */
    def toolset: Toolset =
      val byName = offered.map(c => c.name -> c).toMap
      new Toolset:
        val tools: Chunk[ToolSpec] = Chunk.from(offered.map(_.spec))
        def call(name: String, arguments: Json.Obj): Task[Json] =
          ZIO.fromOption(byName.get(name)).orElseFail(IllegalArgumentException(s"unknown tool: $name")).flatMap(_.via.call(name, arguments))

    /** The coverage as a line of the project context and the log: what does each capability, what's missing. */
    def report: String =
      val covered = Capability.values.toList.map: c =>
        coverage.get(c).fold(s"$c: ${if AllowedGaps(c) then "not provided" else "none"}")(t => s"$c: ${t.name}")
      s"The session's tools by capability: ${covered.mkString("; ")}." +
        fallback.fold("")(f => s" The shell stands in for: ${f.mkString(", ")}.")

  /** The catalog: by family, the capabilities of each known tool, and its shells. */
  final case class Family(serverInfoSuffix: Option[String] = None, names: List[String] = Nil, tools: Map[String, List[String]] = Map.empty,
      shells: List[String] = Nil, groups: List[Group] = Nil) derives JsonDecoder

  /** A tier of a family's tools ([[grouped]]): offered as one option until opened. */
  final case class Group(name: String, description: String, tools: List[String]) derives JsonDecoder
  final case class Catalog(families: Map[String, Family]) derives JsonDecoder:
    def capabilities(family: String, tool: String): Set[Capability] =
      families.get(family).flatMap(_.tools.get(baseName(tool))).toList.flatten.flatMap(c => Capability.values.find(_.toString == c)).toSet
    def shell(family: String, tool: String): Boolean = families.get(family).exists(_.shells.contains(baseName(tool)))
    /** The group `family` puts `tool` in, if any. */
    def groupOf(family: String, tool: String): Option[Group] =
      families.get(family).flatMap(_.groups.find(_.tools.contains(baseName(tool))))

    /** The tools of `family` that cover any of `wanted`, sorted. */
    def covering(family: String, wanted: Set[Capability]): List[String] =
      families.get(family).toList.flatMap(_.tools.keys).filter(t => capabilities(family, t).exists(wanted)).sorted

    /** The family of an MCP server, by its serverInfo name or its endpoint name; `mcp` for one the catalog doesn't know. */
    def familyOf(serverInfo: String, endpoint: String): String =
      families.collectFirst:
        case (f, d) if d.serverInfoSuffix.exists(serverInfo.endsWith) ||
            d.names.exists(n => serverInfo.toLowerCase.contains(n) || endpoint.toLowerCase.contains(n)) => f
      .getOrElse("mcp")

  lazy val catalog: Catalog =
    val text = scala.io.Source.fromResource("tool-catalog.json", getClass.getClassLoader).mkString
    text.fromJson[Catalog].fold(e => throw IllegalStateException(s"tool-catalog.json: $e"), identity)

  /** A tool's name without the server prefix a clash gave it (`sbta.compile` is `compile`). */
  def baseName(name: String): String = name.substring(name.lastIndexOf('.') + 1)

  /** How a source ranks for a capability, lower first: the IDE for files, a build-aware server for builds. */
  private def rank(capability: Capability, source: ToolSource): Int =
    import ToolSource.*
    capability match
      case Capability.Build => source match
        case ProjectMcp => 0; case IntellijMcp => 1; case UserMcp => 2; case AcpClient => 3; case Local => 4; case Builtin => 5
      case _ => source match
        case IntellijMcp => 0; case ProjectMcp => 1; case UserMcp => 1; case AcpClient => 2; case Local => 4; case Builtin => 5

  /**
   * Resolves `candidates` (in catalog order within a source): each capability bound to its best tool; every MCP tool
   * offered; an implicit tool offered when it's a capability's binding or covers no capability (an extra), hidden
   * otherwise; an implicit shell (the ACP client's first, then the local one) offered when no MCP server has a shell:
   * work needs to run things (a measurement, a reproduction, a look at logs) that no other tool does, so `Command` is
   * always covered, by an MCP server's shell or by it. An implicit tool named as an offered MCP tool is dropped.
   */
  def resolve(candidates: List[Candidate], c: Catalog = catalog): Resolution =
    val caps = candidates.map(t => t -> c.capabilities(t.family, t.name)).toMap
    val coverage0 = Capability.values.toList.filterNot(_ == Capability.Command).flatMap: cap =>
      candidates.zipWithIndex.filter((t, _) => caps(t)(cap)).sortBy((t, i) => (rank(cap, t.source), i)).headOption.map(cap -> _._1)
    .toMap
    val bound = coverage0.values.toSet
    val mcp = candidates.filter(_.source.alwaysOffered)
    val mcpNames = mcp.map(_.name).toSet
    val mcpShells = mcp.filter(t => c.shell(t.family, t.name))
    val mcpShell = mcpShells.nonEmpty
    val uncovered = Capability.values.toSet -- coverage0.keySet -- AllowedGaps -- (if mcpShell then Set(Capability.Command) else Set.empty)
    val implicitShells = candidates.filter(t => !t.source.alwaysOffered && c.shell(t.family, t.name))
    val shell = Option.when(uncovered.nonEmpty && !mcpShell)(implicitShells.sortBy(_.source.ordinal).headOption).flatten
    val coverage = coverage0 ++ (mcpShells.headOption orElse shell).map(Capability.Command -> _)
    val decided = candidates.filterNot(_.source.alwaysOffered).map: t =>
      if mcpNames(t.name) then Left(t -> s"an MCP server's tool has its name")
      else if c.shell(t.family, t.name) then
        if shell.contains(t) then Right(t.copy(spec = t.spec.copy(description = t.spec.description +
          s" Use it only for what no other tool does: ${uncovered.toList.sortBy(_.ordinal).map(shellFor).mkString(", ")}. " +
          "Listing, reading, searching and counting in files (logs and build output too) is the file tools' work, not the shell's.")))
        else Left(t -> (if mcpShell then "an MCP server has a shell" else if uncovered.isEmpty then "every capability is covered" else "another shell is offered"))
      else if caps(t).isEmpty || bound(t) then Right(t)
      else Left(t -> s"covered by ${caps(t).toList.sortBy(_.ordinal).flatMap(coverage.get).map(_.name).distinct.mkString(", ")}")
    // an implicit tool offered under a name another implicit tool already has keeps the first (catalog order)
    val implicitOffered = decided.collect { case Right(t) => t }.foldLeft(List.empty[Candidate])((acc, t) =>
      if acc.exists(_.name == t.name) then acc else acc :+ t)
    Resolution(coverage, mcp ++ implicitOffered, decided.collect { case Left(h) => h }, Option.when(shell.isDefined)(uncovered))

  /** What the shell does for a capability, in its description. */
  private def shellFor(c: Capability): String = c match
    case Capability.Command => "running programs and commands (to measure or time something, reproduce a problem, or look at logs and build output)"
    case other              => other.toString

  /** A server with more tools than this whose family the catalog doesn't group has them all in one group. */
  val GroupServerAbove = 8

  /**
   * The tools tiered for a large catalog: a tool the catalog's family puts in a group is in it; an unknown server's
   * with more than [[GroupServerAbove]] tools are in one group of that server's, but for those that cover a required
   * capability (a read, an edit, a build: what most requests need). The loop offers a grouped tool as its group until
   * it's opened (zio-decisions' `Config.groupAbove`).
   */
  def grouped(candidates: List[Candidate], server: Candidate => Option[String], sizes: Map[Option[String], Int],
      c: Catalog = catalog): List[Candidate] =
    candidates.map: t =>
      val group = c.groupOf(t.family, t.name).map(g => g.name -> g.description).orElse:
        server(t).filter(s => t.family == "mcp" && sizes.getOrElse(Some(s), 0) > GroupServerAbove &&
          c.capabilities(t.family, t.name).isEmpty && !c.shell(t.family, t.name))
          .map(s => s"$s tools" -> s"The tools of the MCP server $s.")
      t.copy(spec = t.spec.copy(group = group))

  /** Candidates of one source, from a toolset. */
  def candidates(toolset: Toolset, source: ToolSource, family: String): List[Candidate] =
    toolset.tools.toList.map(t => Candidate(t, source, family, toolset))

  /** The Toolset of soness's resolution of `mcp` (each tool with its server's source and family) and `implicit` tools. */
  def resolved(mcp: McpToolset.Connected, project: Set[String], implicitTools: List[(Toolset, ToolSource, String)]): Resolution =
    val fromMcp = mcp.toolset.tools.toList.map: t =>
      val origin = mcp.origins.get(t.name)
      val family = origin.fold("mcp")(o => catalog.familyOf(o.serverInfo, o.server))
      val source = if family == "intellij" then ToolSource.IntellijMcp
                   else if origin.exists(o => project(o.server)) then ToolSource.ProjectMcp else ToolSource.UserMcp
      Candidate(t, source, family, mcp.toolset)
    val servers = fromMcp.groupBy(c => mcp.origins.get(c.name).map(_.server))
    resolve(grouped(fromMcp, c => mcp.origins.get(c.name).map(_.server), servers.view.mapValues(_.size).toMap) ++
      implicitTools.flatMap(candidates.tupled))
