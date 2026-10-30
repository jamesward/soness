package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

/**
 * A stdio MCP server standing in for IntelliJ IDEA's (`idea stdioMcpServer`), launched by the replay specs as the
 * session's `jetbrains_air_ide` server. Like the IDE's, it speaks the legacy (`initialize`) protocol, so it refuses
 * `server/discover`. Its one tool, `get_project_info`, reports the project directory from `IJ_MCP_SERVER_PROJECT_PATH`
 * and the directory it was started in. It exits when its stdin closes.
 *
 * With `IJ_MCP_FIXTURE_ROUTER=true` it behaves like IntelliJ IDEA in router-only mode (Settings | Tools | MCP Server):
 * its only tool is `execute_tool`, a router taking a `command` such as `get_project_info --projectPath /p`, which
 * answers an unknown tool name with an error that lists the tools it routes to.
 */
object IdeMcpFixture extends ZIOAppDefault:

  private val tool = Json.Obj(
    "name" -> Json.Str("get_project_info"),
    "description" -> Json.Str("Describes the project open in the IDE: its name and directory."),
    "inputSchema" -> Json.Obj("type" -> Json.Str("object"), "properties" -> Json.Obj()),
    "outputSchema" -> Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "projectName" -> Json.Obj("type" -> Json.Str("string")),
        "projectPath" -> Json.Obj("type" -> Json.Str("string")),
        "workingDirectory" -> Json.Obj("type" -> Json.Str("string")),
      ),
    ),
  )

  /** IntelliJ IDEA's router tool, as its `tools/list` describes it. */
  private val router = Json.Obj(
    "name" -> Json.Str("execute_tool"),
    "description" -> Json.Str("Universal tool executor that can invoke specific IDE MCP tool dynamically."),
    "inputSchema" -> Json.Obj(
      "properties" -> Json.Obj("command" -> Json.Obj(
        "type" -> Json.Str("string"),
        "description" -> Json.Str("Command-line string with tool name and arguments"),
      )),
      "required" -> Json.Arr(Json.Str("command")),
      "type" -> Json.Str("object"),
    ),
  )

  private def text(s: String, isError: Boolean = false): Json =
    Json.Obj("content" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(s))), "isError" -> Json.Bool(isError))

  private def reply(id: Json, result: Json): Json.Obj = Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result)

  private def error(id: Json, code: Int, message: String): Json.Obj =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "error" -> Json.Obj("code" -> Json.Num(code), "message" -> Json.Str(message)))

  private def answer(id: Json, method: String, params: Json.Obj, project: String, cwd: String, routerOnly: Boolean): Json.Obj =
    def info = Json.Obj(
      "projectName" -> Json.Str(project.split('/').lastOption.getOrElse(project)),
      "projectPath" -> Json.Str(project),
      "workingDirectory" -> Json.Str(cwd),
    )
    method match
      case "initialize" =>
        reply(id, Json.Obj(
          "protocolVersion" -> Json.Str("2025-06-18"),
          "capabilities" -> Json.Obj("tools" -> Json.Obj()),
          "serverInfo" -> Json.Obj("name" -> Json.Str("ide-fixture"), "version" -> Json.Str("1.0.0")),
        ))
      case "ping"       => reply(id, Json.Obj())
      case "tools/list" => reply(id, Json.Obj("tools" -> Json.Arr(if routerOnly then router else tool)))
      case "tools/call" if !routerOnly && params.get("name").contains(Json.Str("get_project_info")) =>
        reply(id, Json.Obj("content" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(info.toJson))), "structuredContent" -> info))
      case "tools/call" if routerOnly && params.get("name").contains(Json.Str("execute_tool")) =>
        val command = params.get("arguments").flatMap(_.asObject).flatMap(_.get("command")).flatMap(_.asString).getOrElse("")
        command.trim.split("\\s+").headOption.getOrElse("") match
          case "get_project_info" => reply(id, text(info.toJson))
          case other              => reply(id, text(s"Tool '$other' not found. Available tools: get_project_info, read_file, search_text", isError = true))
      case other => error(id, -32601, s"Method not found: $other")

  def run =
    for
      project <- System.env("IJ_MCP_SERVER_PROJECT_PATH").map(_.getOrElse("unknown"))
      router  <- System.env("IJ_MCP_FIXTURE_ROUTER").map(_.contains("true"))
      cwd     <- System.property("user.dir").map(_.getOrElse("unknown"))
      _       <- ZStream.fromInputStream(java.lang.System.in)
                   .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
                   .filter(_.nonEmpty)
                   .map(_.fromJson[Json.Obj].toOption)
                   .collectSome
                   .foreach: message =>
                     (message.get("method").flatMap(_.asString), message.get("id")) match
                       case (Some(method), Some(id)) =>
                         Console.printLine(answer(id, method, message.get("params").flatMap(_.asObject).getOrElse(Json.Obj()), project, cwd, router).toJson)
                       case _ => ZIO.unit // notifications, and answers to requests it never makes
    yield ()
