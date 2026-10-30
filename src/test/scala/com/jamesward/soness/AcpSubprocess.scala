package com.jamesward.soness

import com.jamesward.zioacp.{AgentCommand, Transport}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}

/**
 * soness's `Main` launched as a subprocess, as an editor launches it, and a raw JSON-RPC peer that drives it with the
 * exact messages a client sends. The subprocess inherits this JVM's environment, plus `env`.
 */
object AcpSubprocess:

  def property(name: String): Task[String] =
    System.property(name).someOrFail(IllegalStateException(s"system property $name is not set"))

  /** The messages IntelliJ IDEA 2026.2.3 sent soness (`initialize`, `session/new`, `session/prompt`), one per line. */
  val intellijCapture: Task[Chunk[String]] =
    ZStream.fromResource("intellij-idea-2026.2.3.jsonl")
      .via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .filter(_.nonEmpty)
      .runCollect

  /**
   * The captured `session/new`, for `cwd`, with the orders MCP server at `mcpUrl` and its `jetbrains_air_ide` stdio
   * server (IntelliJ's own command and arguments) replaced by `IdeMcpFixture` on the test classpath.
   */
  def intellijNewSession(
    line: String,
    cwd: String,
    mcpUrl: String,
    routerOnly: Boolean = false,
    skillRoot: Option[java.nio.file.Path] = None,
    fixture: String = "com.jamesward.soness.IdeMcpFixture",
  ): Task[String] =
    for
      javaHome  <- property("java.home")
      classpath <- property("soness.testClasspath")
      ide        = Json.Obj("command" -> Json.Str(s"$javaHome/bin/java"), "args" -> Json.Arr(
                     Json.Str("-cp"), Json.Str(classpath), Json.Str(fixture),
                   ))
      router     = if routerOnly then """{"name":"IJ_MCP_FIXTURE_ROUTER","value":"true"},""" else ""
    yield line.replace("${CWD}", cwd).replace("${MCP_URL}", mcpUrl)
      .replace(""""command":"/opt/intellij-idea/bin/idea","args":["stdioMcpServer"]""", ide.toJson.drop(1).dropRight(1))
      .replace(""""env":[{"name":"IJ_MCP_SERVER_PROJECT_PATH"""", s""""env":[$router{"name":"IJ_MCP_SERVER_PROJECT_PATH"""")
      // IntelliJ IDEA's skill view, a workspace root it sends in `_meta.additionalRoots`
      .replace("/tmp/air/skill-views/other", skillRoot.fold("/tmp/air/skill-views/other")(_.toString))

  /** Starts soness, with the command-line `args`, for the life of the scope. */
  def launch(env: Map[String, String], args: List[String] = Nil): ZIO[Scope, Throwable, Peer] =
    for
      javaHome  <- property("java.home")
      classpath <- property("soness.classpath")
      command    = AgentCommand(
                     s"$javaHome/bin/java",
                     List("--enable-native-access=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "-cp", classpath,
                       "com.jamesward.soness.Main") ++ args,
                     // the replayed sessions run in this project, whose `.mcp.json` would launch sbta: left out
                     // and the developer's own skills (`~/.agents/skills`) too: a home with none
                     Map("SONESS_PROJECT_MCP" -> "false", "SONESS_USER_HOME" -> "/nonexistent", "SONESS_EVENT_LOG" -> "false",
                       // and no web search of the developer's (a key in `.env`)
                       "SONESS_ACP_AGENTS" -> "", "BRAVE_API_KEY" -> "") ++ env,
                   )
      stderr    <- Ref.make(Chunk.empty[String])
      transport <- Transport.process(command, line => stderr.update(_ :+ line))
      incoming  <- Queue.unbounded[Json.Obj]
      _         <- transport.incoming.mapZIO(line => ZIO.fromEither(line.fromJson[Json.Obj]).mapError(RuntimeException(_)))
                     .foreach(incoming.offer).forkScoped
      ids       <- Ref.make(100) // above the captured messages' ids
    yield Peer(transport, incoming, ids, stderr)

  /** Sends lines and requests to soness, and collects what arrives until each response. */
  final class Peer(transport: Transport, incoming: Queue[Json.Obj], ids: Ref[Int], stderr: Ref[Chunk[String]]):
    def send(line: String): Task[Unit] = transport.send(line)

    /** What soness has written to stderr so far. */
    def logged: UIO[Chunk[String]] = stderr.get

    /** The messages received before the response to `id`, and that response; fails with what arrived by `timeout`. */
    def awaitResponse(id: Int, timeout: Duration = 30.seconds): Task[(List[Json.Obj], Json.Obj)] =
      def loop(before: Ref[List[Json.Obj]]): Task[(List[Json.Obj], Json.Obj)] =
        incoming.take.flatMap: message =>
          val isResponse = message.get("method").isEmpty && message.get("id").contains(Json.Num(id))
          if isResponse then before.get.map(_.reverse -> message) else before.update(message :: _) *> loop(before)
      Ref.make(List.empty[Json.Obj]).flatMap: before =>
        loop(before).timeoutFail(())(timeout).catchAll: _ =>
          before.get.zip(stderr.get).flatMap((got, err) => ZIO.fail(RuntimeException(
            s"no response to $id within $timeout; received: ${got.reverse.map(_.toJson)}; its stderr ends: ${err.takeRight(15).mkString("\n")}")))

    /** The messages that arrive until `done` holds of all received so far (with this batch), or `timeout` passes. */
    def collect(done: List[Json.Obj] => Boolean, timeout: Duration): UIO[List[Json.Obj]] =
      Ref.make(List.empty[Json.Obj]).flatMap: got =>
        (incoming.take.flatMap(m => got.updateAndGet(_ :+ m)).repeatUntil(done)).timeout(timeout) *> got.get

    /** Sends a request and gives the notifications before its response, and its result; fails on a JSON-RPC error. */
    def call(method: String, params: Json.Obj, timeout: Duration = 30.seconds): Task[(List[Json.Obj], Json)] =
      for
        id       <- ids.updateAndGet(_ + 1)
        _        <- send(Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> params).toJson)
        received <- awaitResponse(id, timeout)
        (updates, response) = received
        result   <- ZIO.fromOption(response.get("result"))
                      .orElseFail(RuntimeException(s"$method failed: ${response.get("error").fold("no result")(_.toJson)}"))
      yield updates -> result

  def field(json: Json, path: String*): Option[Json] =
    path.foldLeft(Option(json))((j, key) => j.flatMap(_.asObject).flatMap(_.get(key)))

  def str(json: Json, path: String*): Option[String] = field(json, path*).flatMap(_.asString)

  /** A `session/new` result's config option `id`. */
  def configOption(result: Json, id: String): Option[Json] =
    field(result, "configOptions").flatMap(_.asArray).flatMap(_.find(o => str(o, "id").contains(id)))

  /** A select config option's value ids. */
  def values(option: Json): List[String] =
    // grouped (each group's options) or a flat list
    field(option, "options").flatMap(_.asArray).toList.flatten
      .flatMap(o => field(o, "options").flatMap(_.asArray).map(_.toList).getOrElse(List(o))).flatMap(o => str(o, "value"))

  /** A `session/prompt` request for `text`. */
  def promptRequest(id: Int, sessionId: String, text: String): String =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str("session/prompt"),
      "params" -> Json.Obj("sessionId" -> Json.Str(sessionId), "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(text)))),
    ).toJson

  /** The text of a turn's `agent_message_chunk` updates. */
  def answer(updates: List[Json.Obj]): String =
    updates.filter(u => str(u, "params", "update", "sessionUpdate").contains("agent_message_chunk"))
      .flatMap(u => str(u, "params", "update", "content", "text")).mkString
