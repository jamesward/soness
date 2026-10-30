package com.jamesward.soness

import com.jamesward.ziohttp.mcp.{CallToolResult, Implementation, ToolContent, ToolDefinition}
import com.jamesward.ziohttp.mcp.client.{McpClient, McpClientConfig, McpStdioConfig}
import zio.*
import zio.http.{Client, Header, Headers}
import zio.json.*
import zio.json.ast.Json

import java.nio.file.Path

/** An MCP server a client gave a session. */
enum McpEndpoint(val name: String) derives CanEqual:
  /** Reached over Streamable HTTP. */
  case Http(override val name: String, url: String, headers: Map[String, String] = Map.empty) extends McpEndpoint(name)

  /** Launched as a subprocess and reached over its stdin and stdout, in `cwd` (the session's). */
  case Stdio(
    override val name: String,
    command: String,
    args: List[String] = Nil,
    env: Map[String, String] = Map.empty,
    cwd: Option[Path] = None,
  ) extends McpEndpoint(name)

  def describe: String = this match
    case Http(_, url, _)            => s"$name ($url)"
    case Stdio(_, command, _, _, _) => s"$name (stdio: $command)"

/** The tools of the MCP servers a client gave a session. */
object McpToolset:

  /**
   * The toolset, each server that could not be used with the reason, and the `instructions` each connected server
   * gave at `initialize` (how to use its tools), by server name.
   */
  final case class Connected(toolset: Toolset, failed: List[(McpEndpoint, String)], instructions: List[(String, String)] = Nil,
      origins: Map[String, Origin] = Map.empty)

  /** Which server a tool is from: its endpoint's name, and the name it gave itself (`serverInfo`). */
  final case class Origin(server: String, serverInfo: String)

  /** A tool of one server, and how to call it. */
  private final case class Served(spec: ToolSpec, call: Json.Obj => Task[Json])

  /**
   * Connects to every endpoint for the life of the scope and lists their tools. A server that can't be reached or
   * listed is left out (and reported in `failed`), so one bad server doesn't cost the session the others. A JetBrains
   * IDE's server in router mode offers the tools it routes instead of its router ([[RoutedTools]]). A tool whose name
   * another server's tool has too is offered as `<server>.<tool>`.
   */
  def connect(endpoints: List[McpEndpoint]): ZIO[Client & Scope, Throwable, Connected] =
    for
      results <- ZIO.foreachPar(endpoints)(endpoint => tools(endpoint).flatMap((client, definitions) =>
                   served(endpoint, client, definitions).map(s => (s, client.instructions.map(_.trim).filter(_.nonEmpty), client.serverInfo.name)))
                   .either.map(endpoint -> _))
      servers  = results.collect { case (endpoint, Right((listed, _, info))) => (endpoint, listed, info) }
      notes    = results.collect { case (endpoint, Right((_, Some(text), _))) => endpoint.name -> text }
      failed   = results.collect { case (endpoint, Left(error)) => endpoint -> message(error) }
      clashing = servers.flatMap(_._2.map(_.spec.name)).groupBy(identity).collect { case (n, all) if all.size > 1 => n }.toSet
      _       <- ZIO.logInfo(s"MCP tools offered under their server's name, as their names clash: ${clashing.mkString(", ")}").when(clashing.nonEmpty)
      named    = servers.flatMap: (endpoint, listed, info) =>
                   listed.map: t =>
                     val name = if clashing(t.spec.name) then s"${endpoint.name}.${t.spec.name}" else t.spec.name
                     (name, t.copy(spec = t.spec.copy(name = name)), Origin(endpoint.name, info))
      byName   = named.map((n, t, _) => n -> t)
    yield
      val calls = byName.map((name, tool) => name -> tool.call).toMap
      val toolset = new Toolset:
        val tools: Chunk[ToolSpec] = Chunk.from(byName.map(_._2.spec))
        def call(name: String, arguments: Json.Obj): Task[Json] =
          ZIO.fromOption(calls.get(name)).orElseFail(IllegalArgumentException(s"unknown tool: $name")).flatMap(_(arguments))
      Connected(toolset, failed, notes, named.map((n, _, o) => n -> o).toMap)

  /** A server's tools: as listed, or, for a JetBrains IDE's router, the tools it routes. */
  private def served(endpoint: McpEndpoint, client: McpClient, definitions: Chunk[ToolDefinition]): Task[Chunk[Served]] =
    val listed = definitions.map: definition =>
      val name = definition.name.value
      Served(
        ToolSpec(
          name,
          definition.description.getOrElse(name),
          definition.inputSchema,
          definition.outputSchema,
          definition.annotations.flatMap(_.toJsonAST.toOption).flatMap(_.asObject),
        ),
        arguments => callTool(client, name, arguments),
      )
    if !RoutedTools.isRouter(client.serverInfo, definitions) then ZIO.succeed(listed)
    else
      routed(endpoint, client).catchAll: error =>
        ZIO.logWarning(s"MCP server ${endpoint.describe} routes its tools through ${RoutedTools.Router}, " +
          s"but its routed tools couldn't be listed (${message(error)}); offering ${RoutedTools.Router} itself").as(listed)

  /** The tools a JetBrains IDE's router routes, each called through the router with its command line. */
  private def routed(endpoint: McpEndpoint, client: McpClient): Task[Chunk[Served]] =
    for
      probe   <- client.callTool(RoutedTools.Router, Json.Obj("command" -> Json.Str(RoutedTools.Probe)))
                   .mapError(e => RuntimeException(e.getMessage))
      answer   = probe.content.collect { case ToolContent.Text(t, _) => t }.mkString("\n")
      names   <- ZIO.fromOption(RoutedTools.routedNames(answer))
                   .orElseFail(RuntimeException(s"unexpected answer: ${answer.take(200)}"))
      catalog <- RoutedTools.catalog
      unknown  = names.filterNot(catalog.contains)
      _       <- ZIO.logInfo(s"MCP server ${endpoint.describe} (${client.serverInfo.name} ${client.serverInfo.version}) routes " +
                   s"${names.size} tools through ${RoutedTools.Router}; offering them as tools" +
                   (if unknown.isEmpty then "" else s" (without schemas: ${unknown.mkString(", ")})"))
    yield Chunk.from(names.map: name =>
      val known = catalog.contains(name)
      Served(
        RoutedTools.spec(name, catalog),
        arguments =>
          val command = RoutedTools.command(name, arguments, known)
          ZIO.logDebug(s"$name through ${RoutedTools.Router}: $command") *>
            callTool(client, RoutedTools.Router, Json.Obj("command" -> Json.Str(command))).mapError: e =>
              RuntimeException(e.getMessage.replace(s"tool '${RoutedTools.Router}'", s"tool '$name'"))
            .flatMap(result => ZIO.fromEither(RoutedTools.checked(name, result)).mapError(e => RuntimeException(s"tool '$name' failed: $e")))
      )
    )

  private def callTool(client: McpClient, name: String, arguments: Json.Obj): Task[Json] =
    client.callTool(name, arguments).mapError(e => RuntimeException(s"tool '$name': ${e.getMessage}"))
      .flatMap(result => ZIO.fromEither(resultJson(name, result)))

  private def tools(endpoint: McpEndpoint): ZIO[Client & Scope, Throwable, (McpClient, Chunk[ToolDefinition])] =
    val connected: ZIO[Client & Scope, Throwable, McpClient] = endpoint match
      case McpEndpoint.Http(_, url, headers) =>
        McpClient.connect(McpClientConfig(url, clientInfo, headers = Headers.fromIterable(headers.map((k, v) => Header.Custom(k, v)))))
      case McpEndpoint.Stdio(name, command, args, env, cwd) =>
        McpClient.connectStdio(McpStdioConfig(
          command,
          Chunk.from(args),
          env,
          cwd,
          clientInfo,
          stderr = line => ZIO.logDebug(s"MCP server $name stderr: $line"),
        ))
    connected.flatMap(client => client.listTools.map(client -> _))
      .mapError(e => RuntimeException(s"MCP server ${endpoint.describe}: ${message(e)}"))

  private def clientInfo = Implementation("soness", Soness.version)

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.getClass.getName)

  /** The structured content when the tool returned one, else the text content (parsed when it is JSON). */
  def resultJson(name: String, result: CallToolResult): Either[Throwable, Json] =
    val text = result.content.collect { case ToolContent.Text(t, _) => t }.mkString("\n")
    if result.isError.contains(true) then Left(RuntimeException(s"tool '$name' failed: $text"))
    else Right(result.structuredContent.getOrElse(parsed(text).getOrElse(Json.Obj("text" -> Json.Str(text)))))

  /**
   * `text` as JSON when it's an object or an array, whole. Not a bare scalar: zio-json reads a leading number and
   * ignores the rest, so "0 out of 1 operations applied…" would be the number 0, its message lost.
   */
  private def parsed(text: String): Option[Json] =
    val t = text.trim
    Option.when((t.startsWith("{") && t.endsWith("}")) || (t.startsWith("[") && t.endsWith("]")))(t).flatMap(_.fromJson[Json].toOption)
