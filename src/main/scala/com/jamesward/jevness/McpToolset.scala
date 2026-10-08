package com.jamesward.jevness

import com.jamesward.ziohttp.mcp.{CallToolResult, ToolContent}
import com.jamesward.ziohttp.mcp.client.{McpClient, McpClientConfig}
import zio.*
import zio.http.{Client, Header, Headers}
import zio.json.*
import zio.json.ast.Json

/** An MCP server reached over Streamable HTTP. */
final case class McpEndpoint(name: String, url: String, headers: Map[String, String] = Map.empty)

/** The tools of the MCP servers a client gave a session. */
object McpToolset:

  /** Connects to every endpoint for the life of the scope and lists their tools. Tool names must be unique. */
  def connect(endpoints: List[McpEndpoint]): ZIO[Client & Scope, Throwable, Toolset] =
    for
      servers <- ZIO.foreach(endpoints): endpoint =>
                   McpClient
                     .connect(McpClientConfig(endpoint.url, headers = Headers.fromIterable(endpoint.headers.map((k, v) => Header.Custom(k, v)))))
                     .flatMap(client => client.listTools.map(client -> _))
                     .mapError(e => RuntimeException(s"MCP server '${endpoint.name}' (${endpoint.url}): ${e.getMessage}"))
      byName = servers.flatMap { case (client, definitions) => definitions.map(d => d.name.value -> (client, d)) }
      duplicates = byName.groupBy(_._1).collect { case (name, entries) if entries.size > 1 => name }
      _ <- ZIO.fail(IllegalArgumentException(s"tool names must be unique across MCP servers: ${duplicates.mkString(", ")}"))
             .when(duplicates.nonEmpty)
    yield
      val clients = byName.map((name, entry) => name -> entry._1).toMap
      val specs = Chunk.from(byName.map: (name, entry) =>
        val definition = entry._2
        ToolSpec(name, definition.description.getOrElse(name), definition.inputSchema, definition.outputSchema)
      )
      new Toolset:
        val tools: Chunk[ToolSpec] = specs
        def call(name: String, arguments: Json.Obj): Task[Json] =
          ZIO.fromOption(clients.get(name)).orElseFail(IllegalArgumentException(s"unknown tool: $name"))
            .flatMap(_.callTool(name, arguments).mapError(e => RuntimeException(s"tool '$name': ${e.getMessage}")))
            .flatMap(result => ZIO.fromEither(resultJson(name, result)))

  /** The structured content when the tool returned one, else the text content (parsed when it is JSON). */
  def resultJson(name: String, result: CallToolResult): Either[Throwable, Json] =
    val text = result.content.collect { case ToolContent.Text(t, _) => t }.mkString("\n")
    if result.isError.contains(true) then Left(RuntimeException(s"tool '$name' failed: $text"))
    else Right(result.structuredContent.getOrElse(text.fromJson[Json].getOrElse(Json.Obj("text" -> Json.Str(text)))))
