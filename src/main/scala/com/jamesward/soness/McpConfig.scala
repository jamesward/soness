package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.{Files, Path}

/**
 * The project's own MCP servers, from its `.mcp.json`: the format coding agents share (Claude Code's project scope,
 * which Cursor, Kiro and others read too):
 *
 * {{{
 * { "mcpServers": {
 *     "sbta":  { "command": "./sbta", "args": ["--mcp"], "env": { "K": "$${K:-default}" } },
 *     "docs":  { "type": "http", "url": "https://…/mcp", "headers": { "Authorization": "Bearer $${TOKEN}" } } } }
 * }}}
 *
 * A server is stdio when it has a `command` (`type` `stdio`, or none), Streamable HTTP when its `type` is `http` (or
 * it has only a `url`); `sse` servers and `disabled` ones are skipped. `$${VAR}` and `$${VAR:-default}` in the command,
 * arguments, URL, headers and env are expanded from soness's environment. A relative command (`./sbta`) is the
 * project's, resolved against its directory. Fields soness doesn't use (`timeout`, `autoApprove`, …) are ignored.
 */
object McpConfig:

  val FileName = ".mcp.json"

  /** The servers `.mcp.json` in `dir` declares, and each it skipped with the reason; none when there's no file. */
  def load(dir: Path, env: String => Option[String]): UIO[(List[McpEndpoint], List[String])] =
    val file = dir.resolve(FileName)
    ZIO.attemptBlocking(Option.when(Files.isRegularFile(file))(Files.readString(file))).either.map:
      case Left(e)           => (Nil, List(s"$file: ${e.getMessage}"))
      case Right(None)       => (Nil, Nil)
      case Right(Some(text)) => parse(text, dir, env).fold(e => (Nil, List(s"$file: $e")), identity)

  /** `.mcp.json`'s text as endpoints (launched in `dir`), and the servers skipped with the reason. */
  def parse(text: String, dir: Path, env: String => Option[String]): Either[String, (List[McpEndpoint], List[String])] =
    for
      json    <- text.fromJson[Json]
      servers <- json.asObject.flatMap(_.get("mcpServers")).map(_.asObject.toRight("mcpServers isn't an object"))
                   .getOrElse(Right(Json.Obj()))
    yield
      val read = servers.fields.toList.map((name, value) => value.asObject.toRight(s"$name: not an object").flatMap(server(name, _, dir, env)))
      (read.collect { case Right(Some(e)) => e }, read.collect { case Left(reason) => reason })

  private def server(name: String, obj: Json.Obj, dir: Path, env: String => Option[String]): Either[String, Option[McpEndpoint]] =
    def str(key: String) = obj.get(key).flatMap(_.asString).map(expand(_, env))
    def strings(key: String) = obj.get(key).flatMap(_.asArray).toList.flatten.flatMap(_.asString).map(expand(_, env))
    def map(key: String) = obj.get(key).flatMap(_.asObject).toList.flatMap(_.fields).collect:
      case (k, Json.Str(v)) => k -> expand(v, env)
    .toMap
    val kind = str("type").map(_.toLowerCase).getOrElse(if obj.contains("command") then "stdio" else if obj.contains("url") then "http" else "")
    if obj.get("disabled").contains(Json.Bool(true)) then Right(None)
    else kind match
      case "stdio" =>
        str("command").filter(_.nonEmpty).toRight(s"$name: a stdio server needs a command").map: command =>
          // a project-relative command (`./sbta`, `bin/server`) is the project's, wherever soness was launched
          val resolved = if command.contains('/') && !Path.of(command).isAbsolute then dir.resolve(command).normalize.toString else command
          Some(McpEndpoint.Stdio(name, resolved, strings("args"), map("env"), Some(dir)))
      case "http" | "streamable-http" | "streamablehttp" =>
        str("url").filter(_.nonEmpty).toRight(s"$name: an http server needs a url").map(url => Some(McpEndpoint.Http(name, url, map("headers"))))
      case "sse"  => Left(s"$name: SSE servers aren't supported")
      case other  => Left(s"$name: unknown server type '$other'")

  private val Variable = """\$\{([A-Za-z_][A-Za-z0-9_]*)(?::-([^}]*))?\}""".r

  /** `$${VAR}` and `$${VAR:-default}` expanded; an unset variable without a default is empty. */
  def expand(value: String, env: String => Option[String]): String =
    Variable.replaceAllIn(value, m => java.util.regex.Matcher.quoteReplacement(env(m.group(1)).filter(_.nonEmpty).getOrElse(Option(m.group(2)).getOrElse(""))))

  /**
   * The session's servers: the client's (ACP's `mcpServers`) and then the project's; a project server named like one
   * of the client's is left out, since the client's is the one it configured for this session.
   */
  def combine(client: List[McpEndpoint], project: List[McpEndpoint]): List[McpEndpoint] =
    client ++ project.filterNot(p => client.exists(_.name == p.name))
