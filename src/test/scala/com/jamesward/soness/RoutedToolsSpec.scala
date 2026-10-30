package com.jamesward.soness

import com.jamesward.ziohttp.mcp.{Implementation, ToolDefinition}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object RoutedToolsSpec extends ZIOSpecDefault:

  private val router = Json.Obj(
    "name" -> Json.Str("execute_tool"),
    "inputSchema" -> Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj("command" -> Json.Obj("type" -> Json.Str("string")))),
  ).toJson.fromJson[ToolDefinition].toOption.get

  def spec = suite("RoutedTools")(
    test("a JetBrains IDE's MCP server whose only tool is execute_tool is a router; others aren't") {
      val ide = Implementation("IntelliJ IDEA MCP Server", "2026.2.3")
      val other = router.copy(name = router.name).toJson.replace("execute_tool", "read_file").fromJson[ToolDefinition].toOption.get
      assertTrue(
        RoutedTools.isRouter(ide, Chunk(router)),
        RoutedTools.isRouter(Implementation("PyCharm MCP Server", "2026.2"), Chunk(router)),
        !RoutedTools.isRouter(Implementation("ide-fixture", "1.0.0"), Chunk(router)),
        !RoutedTools.isRouter(ide, Chunk(router, other)),
        !RoutedTools.isRouter(ide, Chunk(other)),
      )
    },
    test("the routed tools are read from the router's not-found answer, without the router") {
      assertTrue(
        RoutedTools.routedNames("Tool 'soness_list_routed_tools' not found. Available tools: apply_patch, execute_tool, read_file")
          .contains(List("apply_patch", "read_file")),
        RoutedTools.routedNames("Command is empty").isEmpty,
      )
    },
    test("the catalog has the IDE's descriptions and input schemas") {
      for catalog <- RoutedTools.catalog
      yield
        val read = catalog("read_file")
        assertTrue(
          !catalog.contains("execute_tool"),
          IntellijRouter.routed.filterNot(_ == "execute_tool").forall(catalog.contains),
          read.description.trim.startsWith("Reads a file"),
          read.inputSchema.get("required").contains(Json.Arr(Json.Str("file_path"))),
        )
    },
    test("an unknown routed tool takes its command line's rest") {
      for catalog <- RoutedTools.catalog
      yield
        val spec = RoutedTools.spec("new_tool", catalog)
        assertTrue(
          spec.inputSchema.get("required").contains(Json.Arr(Json.Str(RoutedTools.ArgumentsParameter))),
          RoutedTools.command("new_tool", Json.Obj("arguments" -> Json.Str("--a 'b c'")), known = false) == "new_tool --a 'b c'",
          RoutedTools.command("new_tool", Json.Obj(), known = false) == "new_tool",
        )
    },
    test("a known tool's arguments are single-quoted pairs, non-strings as JSON, nulls left out") {
      assertTrue(
        RoutedTools.command("read_file", Json.Obj("file_path" -> Json.Str("a b/it's.txt"), "limit" -> Json.Num(10),
          "offset" -> Json.Null), known = true) == """read_file --file_path 'a b/it'"'"'s.txt' --limit '10'""",
        RoutedTools.command("search_file", Json.Obj("q" -> Json.Str("*.scala"), "paths" -> Json.Arr(Json.Str("src/"))), known = true) ==
          """search_file --q '*.scala' --paths '["src/"]'""",
        RoutedTools.command("get_project_modules", Json.Obj(), known = true) == "get_project_modules",
      )
    },
    test("an apply_patch that applied fewer operations than it had is a failure; other results are as they came") {
      val partial = "1 out of 2 operations applied. 1 operations failed to be applied due to errors:\na.scala: Hunk context not found"
      assertTrue(
        RoutedTools.checked("apply_patch", Json.Obj("text" -> Json.Str(partial))) == Left(partial),
        RoutedTools.checked("apply_patch", Json.Obj("text" -> Json.Str("1 out of 1 operations applied."))).isRight,
        RoutedTools.checked("read_file", Json.Obj("text" -> Json.Str(partial))).isRight,
      )
    },
    test("a text result that starts with a number stays text: a patch that applied nothing is a failed call") {
      import com.jamesward.ziohttp.mcp.{CallToolResult, ToolContent}
      val text = "0 out of 1 operations applied. 1 operations failed to be applied due to errors: Hunk context not found"
      val result = McpToolset.resultJson("apply_patch", CallToolResult(Chunk(ToolContent.Text(text))))
      assertTrue(
        result.map(_.toJson).contains(Json.Obj("text" -> Json.Str(text)).toJson),
        result.flatMap(r => RoutedTools.checked("apply_patch", r).left.map(_ => "failed")).isLeft,
        McpToolset.resultJson("t", CallToolResult(Chunk(ToolContent.Text("""{"a":1}""")))).map(_.toJson).contains("""{"a":1}"""),
      )
    },
  )
