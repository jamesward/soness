package com.jamesward.soness

import com.jamesward.ziohttp.mcp.{Implementation, ToolDefinition}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

/**
 * An IntelliJ IDEA (JetBrains IDE) MCP server in router mode, as the IDE gives ACP agents: its one tool, `execute_tool`,
 * takes a command line (`read_file --file_path build.sbt`) for any of the IDE's tools, and neither it nor anything else
 * the server offers describes them. Such a server is recognized by its `serverInfo` ("… MCP Server") and its tool list
 * (only `execute_tool`, with a `command`), and asked once which tools it routes: a call to a tool it doesn't have
 * answers "Tool '…' not found. Available tools: …". Each routed tool is then offered as a tool of its own, described
 * from [[catalog]] (`intellij-mcp-tools.json`: the IDE's own descriptions and schemas, from its MCP server's direct
 * mode), so Jev chooses among the real tools, by name, description and parameter names (the IDE has too many tools
 * for a choice to carry their schemas too), and the LLM writes arguments against the real schema; [[command]] renders them as the
 * router's command line. A routed tool the catalog doesn't know is offered by name, its arguments written as
 * the command line's rest.
 */
object RoutedTools:

  /** The router's tool. */
  val Router = "execute_tool"

  /** A tool name no IDE has, to ask the router which tools it routes. */
  val Probe = "soness_list_routed_tools"

  /** One of the IDE's tools as its MCP server describes it in direct mode. */
  final case class Entry(name: String, description: String, inputSchema: Json.Obj, outputSchema: Option[Json.Obj] = None)
      derives JsonDecoder

  private final case class Catalog(source: String, tools: List[Entry]) derives JsonDecoder

  /** The IDE's tools by name, from `intellij-mcp-tools.json`. */
  val catalog: Task[Map[String, Entry]] =
    ZStream.fromResource("intellij-mcp-tools.json").via(ZPipeline.utf8Decode).runFold("")(_ + _)
      .flatMap(text => ZIO.fromEither(text.fromJson[Catalog]).mapError(e => IllegalStateException(s"intellij-mcp-tools.json: $e")))
      .map(_.tools.map(e => e.name -> e).toMap)

  /** Whether a server is a JetBrains IDE's MCP server in router mode. */
  def isRouter(serverInfo: Implementation, tools: Chunk[ToolDefinition]): Boolean =
    serverInfo.name.endsWith(" MCP Server") && tools.size == 1 && tools.head.name.value == Router &&
      tools.head.inputSchema.get("properties").flatMap(_.asObject).exists(_.contains("command"))

  /** The routed tool names in the router's answer to [[Probe]], without the router itself. */
  def routedNames(answer: String): Option[List[String]] =
    "Available tools: (.+)".r.findFirstMatchIn(answer).map(_.group(1).split(",").toList.map(_.trim).filter(_.nonEmpty))
      .map(_.filterNot(_ == Router)).filter(_.nonEmpty)

  /** The parameter an unknown routed tool's arguments are written to, as the command line after the tool's name. */
  val ArgumentsParameter = "arguments"

  /**
   * Whether a routed tool only reads, by its name (the IDE's tool list carries no annotations): its reads, searches,
   * listings and lookups; not its builds, problems checks (they compile), runs, edits or commands.
   */
  def reads(name: String): Boolean =
    List("read_", "search_", "list_", "get_", "find_").exists(name.startsWith) &&
      !Set("get_file_problems", "get_run_configurations")(name)

  /** The tool for a routed name: the catalog's description and schemas, or the name with a free-form command line. */
  def spec(name: String, catalog: Map[String, Entry]): ToolSpec =
    catalog.get(name) match
      case Some(entry) =>
        ToolSpec(name, s"${entry.description.trim}\n\n(An IDE tool, called through the IDE's $Router.)", entry.inputSchema,
          entry.outputSchema, annotations = Option.when(reads(name))(Json.Obj("readOnlyHint" -> Json.Bool(true))), schemasInChoice = false)
      case None =>
        ToolSpec(
          name,
          s"An IDE tool, called through the IDE's $Router; its parameters aren't known.",
          Json.Obj(
            "type" -> Json.Str("object"),
            "properties" -> Json.Obj(ArgumentsParameter -> Json.Obj(
              "type" -> Json.Str("string"),
              "description" -> Json.Str("The tool's arguments as `--paramName value` pairs; JSON for object and array values"),
            )),
            "required" -> Json.Arr(Json.Str(ArgumentsParameter)),
          ),
          None,
          schemasInChoice = false,
        )

  /**
   * The router's command line for a call: the tool's name and a `--name 'value'` pair per argument (nulls left out).
   * Strings are passed as they are, other values as JSON (the router parses `42`, `true` and `["a"]` by the parameter's
   * type). Each value is single-quoted, which the router reads literally, backslashes and newlines included; a quote
   * inside one is written `'"'"'`, as in a shell. An unknown tool's [[ArgumentsParameter]] is the line's rest.
   */
  def command(name: String, arguments: Json.Obj, known: Boolean): String =
    if !known then (name :: arguments.get(ArgumentsParameter).flatMap(_.asString).filter(_.trim.nonEmpty).toList).mkString(" ")
    else
      val pairs = arguments.fields.toList.flatMap:
        case (_, Json.Null)       => Nil
        case (key, Json.Str(s))   => List(s"--$key ${quote(s)}")
        case (key, other)         => List(s"--$key ${quote(other.toJson)}")
      (name :: pairs).mkString(" ")

  private val Applied = """(\d+) out of (\d+) operations applied""".r.unanchored

  /**
   * A routed tool's result as what it means. The IDE's `apply_patch` answers a patch it couldn't fully apply as a
   * success ("0 out of 1 operations applied. 1 operations failed to be applied due to errors: … Hunk context not
   * found"); that is a failure of the call, its message the IDE's (which says which operations did apply).
   */
  def checked(name: String, result: Json): Either[String, Json] =
    if name != "apply_patch" then Right(result)
    else
      val text = result.asObject.flatMap(_.get("text")).flatMap(_.asString).getOrElse(result.asString.getOrElse(""))
      text match
        case Applied(applied, total) if applied.toInt < total.toInt => Left(text)
        case _                                                      => Right(result)

  private def quote(text: String): String = "'" + text.replace("'", "'\"'\"'") + "'"
