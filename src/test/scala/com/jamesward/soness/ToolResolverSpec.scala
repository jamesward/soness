package com.jamesward.soness

import com.jamesward.soness.ToolResolver.{Candidate, Capability, ToolSource}
import com.jamesward.soness.ToolResolver.Capability.*
import zio.*
import zio.json.ast.Json
import zio.test.*

/** The session's tools by capability (TOOLS_SPEC.md): MCP tools always offered, soness's own only where they're needed. */
object ToolResolverSpec extends ZIOSpecDefault:

  private def set(names: String*): Toolset = new Toolset:
    val tools = Chunk.from(names.map(n => ToolSpec(n, s"The $n tool.", Json.Obj(), None)))
    def call(name: String, arguments: Json.Obj): Task[Json] = ZIO.succeed(Json.Str(s"$name called"))

  private def from(source: ToolSource, family: String, names: String*): List[Candidate] =
    ToolResolver.candidates(set(names*), source, family)

  private val intellij = from(ToolSource.IntellijMcp, "intellij", "read_file", "create_new_file", "apply_patch", "search_file",
    "search_text", "get_file_problems", "build_project", "execute_terminal_command", "search_symbol")
  private val sbta = from(ToolSource.ProjectMcp, "sbta", "check", "compile", "test", "inspect")
  private val acp = from(ToolSource.AcpClient, "acp", "read_text_file", "write_text_file", "edit_text_file", "run_terminal_command")
  private val local = from(ToolSource.Local, "local", "read_file", "list_directory", "list_files", "search_text", "edit_file", "write_file", "run_command")
  private val builtin = from(ToolSource.Builtin, "builtin", "web_fetch")

  private def names(r: ToolResolver.Resolution) = r.offered.map(_.name).toSet
  private def shells(r: ToolResolver.Resolution) = names(r).intersect(Set("run_command", "run_terminal_command"))

  def spec = suite("tool resolver")(
    test("IntelliJ, sbta, ACP and soness's own: every MCP tool offered, no implicit file tool, no implicit shell (the IDE's covers Command); builds are sbta's") {
      val r = ToolResolver.resolve(intellij ++ sbta ++ acp ++ local ++ builtin)
      assertTrue(
        (intellij ++ sbta).map(_.name).toSet.subsetOf(names(r)),
        !names(r).exists(Set("read_text_file", "write_text_file", "edit_file", "write_file", "list_files")),
        shells(r).isEmpty, r.fallback.isEmpty, r.coverage(Command).source == ToolSource.IntellijMcp,
        r.coverage(FileRead).source == ToolSource.IntellijMcp, r.coverage(Build).name == "check",
        r.coverage(WebFetch).name == "web_fetch", !r.coverage.contains(WebSearch),
        r.hidden.exists((t, why) => t.name == "read_text_file" && why.contains("read_file")),
      )
    },
    test("IntelliJ only, no web_fetch: its tools, nothing implicit") {
      val r = ToolResolver.resolve(intellij)
      assertTrue(names(r) == intellij.map(_.name).toSet, r.fallback.isDefined == false)
    },
    test("ACP fs and sbta: ACP's read, write and patch (through the editor), soness's find and search; a shell for running things (sbta builds)") {
      val r = ToolResolver.resolve(sbta ++ acp ++ local ++ builtin)
      assertTrue(
        r.coverage(FileRead).name == "read_text_file", r.coverage(FileWrite).name == "write_text_file",
        r.coverage(FilePatch).name == "edit_text_file", r.coverage(FileFind).name == "list_files",
        // one tool each: soness's own are hidden
        !names(r)("write_file"), !names(r)("read_file"), !names(r)("edit_file"),
        // sbta builds and tests; measuring, reproducing or reading logs takes running a command: the ACP client's terminal
        shells(r) == Set("run_terminal_command"), r.fallback.contains(Set(Command)), r.coverage(Command).name == "run_terminal_command",
        r.offered.find(_.name == "run_terminal_command").exists(_.spec.description.contains("measure")),
      )
    },
    test("ACP fs only: no build tool, so one shell stands in for Build, the ACP client's, saying so") {
      val r = ToolResolver.resolve(acp ++ local ++ builtin)
      val shell = r.offered.find(_.name == "run_terminal_command")
      assertTrue(r.fallback.contains(Set(Build, Command)), shells(r) == Set("run_terminal_command"),
        shell.exists(_.spec.description.contains("Build")))
    },
    test("nothing but soness's own: its file tools, web_fetch and run_command for the build") {
      val r = ToolResolver.resolve(local ++ builtin)
      assertTrue(names(r) == Set("read_file", "list_directory", "list_files", "search_text", "edit_file", "write_file", "run_command", "web_fetch"),
        r.fallback.contains(Set(Build, Command)))
    },
    test("a user MCP tool of the same name as one of soness's: the MCP tool is offered, soness's dropped; unknown MCP tools are extras") {
      val user = from(ToolSource.UserMcp, "mcp", "web_fetch", "list_orders")
      val r = ToolResolver.resolve(intellij ++ user ++ builtin)
      assertTrue(r.offered.count(_.name == "web_fetch") == 1, r.offered.find(_.name == "web_fetch").exists(_.source == ToolSource.UserMcp),
        names(r)("list_orders"))
    },
    test("the catalog knows its families by serverInfo and endpoint names; a prefixed tool keeps its capabilities") {
      val c = ToolResolver.catalog
      assertTrue(c.familyOf("IntelliJ IDEA MCP Server", "jetbrains") == "intellij", c.familyOf("sbt", "sbta") == "sbta",
        c.familyOf("orders", "orders") == "mcp", c.capabilities("sbta", "sbta.compile") == Set(Build),
        SharedTools.supersedable("sbta.compile", Json.Obj()))
    },
    test("tiers: the catalog's groups (IntelliJ's debugger, sbta's library docs); a big unknown server's non-core tools in its own group") {
      val ide = from(ToolSource.IntellijMcp, "intellij", "read_file", "xdebug_get_stack", "execute_sql_query", "rename_refactoring")
      val tools = from(ToolSource.ProjectMcp, "sbta", "compile", "get_javadoc_index", "inspect")
      val big = from(ToolSource.UserMcp, "mcp", (1 to 10).map(i => s"thing_$i")*)
      val groups = ToolResolver.grouped(ide ++ tools ++ big, c => Option.when(c.family == "mcp")("orders"), Map(Some("orders") -> 10))
        .map(c => c.name -> c.spec.group.map(_._1)).toMap
      val few = ToolResolver.grouped(big.take(3), _ => Some("orders"), Map(Some("orders") -> 3)).map(_.spec.group)
      assertTrue(
        groups("read_file").isEmpty, groups("compile").isEmpty,
        groups("xdebug_get_stack").contains("IDE debugger"), groups("execute_sql_query").contains("IDE databases"),
        groups("rename_refactoring").contains("IDE code intelligence"),
        groups("get_javadoc_index").contains("library docs (javadocs.dev)"), groups("inspect").contains("Scala symbols"),
        groups("thing_1").contains("orders tools"), few.forall(_.isEmpty),
      )
    },
    test("Kiro's built-ins are in the catalog: its search session gets exactly its web tools") {
      val c = ToolResolver.catalog
      assertTrue(c.covering("kiro", Set(WebSearch, WebFetch)) == List("web_fetch", "web_search"),
        c.capabilities("kiro", "write") == Set(FileWrite, FilePatch), c.shell("kiro", "shell"))
    },
    test("web search follows the writer: Bedrock's for a Mantle GPT-5.4+ model in a Region that has it, else none") {
      import com.jamesward.zio_bedrock.Bedrock.{ApiKey, Region}
      def access(mantle: Boolean, region: Region) = Soness.BedrockAccess(mantle, ApiKey("k"), region, None)
      ZIO.serviceWith[zio.http.Client](http => assertTrue(
        Soness.bedrockSearch(access(true, Region.UsWest2), "openai.gpt-5.6-terra", http).isDefined,
        Soness.bedrockSearch(access(false, Region.UsWest2), "openai.gpt-5.6-terra", http).isEmpty, // Converse
        Soness.bedrockSearch(access(true, Region.UsWest2), "openai.gpt-oss-120b", http).isEmpty,
        Soness.bedrockSearch(access(true, Region.EuWest1), "openai.gpt-5.6-terra", http).isEmpty,
      )).provide(zio.http.Client.default)
    },
    test("Bedrock's answer is a search result with its citations' spans; one citing nothing fails") {
      import com.jamesward.zio_bedrock.MantleWebSearch.{Answer, Citation as Cited}
      for
        ok   <- com.jamesward.soness.WebSearch.bedrock((_, _) => ZIO.succeed(Answer("Scala 3.10", List(Cited("https://scala-lang.org", "Scala", 0, 5)))))
                  .search("q", ContextSize.Low)
        none <- com.jamesward.soness.WebSearch.bedrock((_, _) => ZIO.succeed(Answer("I don't know", Nil))).search("q", ContextSize.Low).flip
      yield assertTrue(ok.citations.head == Citation("https://scala-lang.org", Some("Scala"), Some(Span(0, 5))),
        none.getMessage.contains("cited no sources"))
    },
  )
