package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

/**
 * IntelliJ IDEA's MCP server as `idea stdioMcpServer` runs it for an ACP agent (router-only): a stdio MCP server over
 * the project in `IJ_MCP_SERVER_PROJECT_PATH`, its one tool `execute_tool` routed by [[IntellijRouter]]. Like the real
 * server it speaks the legacy (`initialize`) protocol, answering `server/discover` with method-not-found, and answers a
 * routed tool's failure as an error result (`isError`) with the router's message. It exits when its stdin closes.
 */
object IntellijMcpFixture extends ZIOAppDefault:

  private def reply(id: Json, result: Json): Json.Obj = Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result)

  private def text(s: String, isError: Boolean): Json =
    val content = Json.Obj("content" -> Json.Arr(Json.Obj("text" -> Json.Str(s), "type" -> Json.Str("text"))))
    if isError then Json.Obj(content.fields :+ ("isError" -> Json.Bool(true))) else content

  private def answer(id: Json, method: String, params: Json.Obj, project: java.nio.file.Path): Json.Obj =
    method match
      case "initialize" =>
        reply(id, Json.Obj(
          "protocolVersion" -> Json.Str("2025-06-18"),
          "capabilities" -> Json.Obj("tools" -> Json.Obj("listChanged" -> Json.Bool(true))),
          "serverInfo" -> Json.Obj("name" -> Json.Str("IntelliJ IDEA MCP Server"), "version" -> Json.Str("2026.2.3")),
        ))
      case "ping"       => reply(id, Json.Obj())
      case "tools/list" => reply(id, Json.Obj("tools" -> Json.Arr(IntellijRouter.tool)))
      case "tools/call" =>
        params.get("name").flatMap(_.asString) match
          case Some("execute_tool") =>
            val command = params.get("arguments").flatMap(_.asObject).flatMap(_.get("command")).flatMap(_.asString).getOrElse("")
            IntellijRouter.run(command, project).fold(e => reply(id, text(e, isError = true)), r => reply(id, text(r, isError = false)))
          case other => reply(id, text(s"Tool ${other.getOrElse("")} not found", isError = true))
      case other =>
        Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id,
          "error" -> Json.Obj("code" -> Json.Num(-32601), "message" -> Json.Str(s"Method not found: $other")))

  def run =
    for
      project <- System.env("IJ_MCP_SERVER_PROJECT_PATH").someOrFail(IllegalStateException("IJ_MCP_SERVER_PROJECT_PATH is not set"))
      root     = java.nio.file.Path.of(project)
      out     <- Semaphore.make(1)
      _       <- ZStream.fromInputStream(java.lang.System.in)
                   .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
                   .filter(_.nonEmpty)
                   .map(_.fromJson[Json.Obj].toOption)
                   .collectSome
                   // requests are answered concurrently, as the IDE does (an edit doesn't wait for a test run), each
                   // answer written whole
                   .mapZIOParUnordered(32): message =>
                     (message.get("method").flatMap(_.asString), message.get("id")) match
                       case (Some(method), Some(id)) =>
                         ZIO.attemptBlocking(answer(id, method, message.get("params").flatMap(_.asObject).getOrElse(Json.Obj()), root))
                           .flatMap(a => out.withPermit(Console.printLine(a.toJson)))
                       case _ => ZIO.unit
                   .runDrain
    yield ()
