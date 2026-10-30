package com.jamesward.soness

import com.jamesward.zioacp.ClientConnection
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.ast.Json

/**
 * The ACP client's own capabilities as tools, for a client that advertises them (`clientCapabilities.fs`,
 * `clientCapabilities.terminal`; Zed does, IntelliJ IDEA gives an MCP server instead):
 *
 *   - `read_text_file` (`fs/read_text_file`): a file as the editor has it, unsaved changes included;
 *   - `write_text_file` (`fs/write_text_file`): a whole file written through the editor, shown as a diff;
 *   - `edit_text_file`, with both: an exact replacement in a file, read and written back through the editor (its
 *     unsaved changes kept), as [[LocalTools]]' `edit_file` does on disk;
 *   - `run_terminal_command` (`terminal/create`): a command in the client's terminal, embedded in its tool call so
 *     the user sees its output live; its end is waited for with `terminal/wait_for_exit` (the client says when it
 *     exits: nothing polls), then its output read and the terminal released. Stopped (the turn cancelled, the call
 *     judged hung), the command is killed and its terminal released.
 *
 * A session tool of the same name wins.
 */
object ClientTools:

  /** Where the current turn's plan goes (ACP's `plan` session update): set by the agent for each turn. */
  val currentPlan: FiberRef[List[(String, com.jamesward.zio_decisions.orchestration.ParallelLoop.Progress)] => UIO[Unit]] =
    Unsafe.unsafely(FiberRef.unsafe.make(_ => ZIO.unit))

  /** The tool call the current fiber's tool call is reported as, so a tool can add to its updates (a terminal). */
  val currentCall: FiberRef[Option[ToolCallId]] = Unsafe.unsafely(FiberRef.unsafe.make(Option.empty[ToolCallId]))

  private def schema(required: List[String], properties: (String, String, String)*): Json.Obj =
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(properties.map((n, t, d) => n -> (Json.Obj("type" -> Json.Str(t), "description" -> Json.Str(d)): Json))*),
      "required" -> Json.Arr(required.map(Json.Str(_))*),
    )

  private val readOnly = Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))

  val Read = ToolSpec("read_text_file",
    "Reads a text file as the editor has it (unsaved changes included). Optionally from a line, a number of lines.",
    schema(List("path"), ("path", "string", "The file's absolute path, or relative to the project"),
      ("line", "integer", "The line to start at (1-based)"), ("limit", "integer", "How many lines to read")),
    None, readOnly)

  val Write = ToolSpec("write_text_file",
    "Writes a whole text file through the editor (created when it doesn't exist): the content replaces the file's.",
    schema(List("path", "content"), ("path", "string", "The file's absolute path, or relative to the project"),
      ("content", "string", "The file's whole new content")),
    None)

  val Edit = ToolSpec("edit_text_file",
    "Edits a file through the editor (unsaved changes kept) by replacing an exact piece of its text: `old_text` must " +
      "occur exactly once (give enough surrounding lines to make it unique), unless `replace_all`.",
    schema(List("path", "old_text", "new_text"), ("path", "string", "The file's absolute path, or relative to the project"),
      ("old_text", "string", "The exact text to replace, whitespace included"), ("new_text", "string", "The text to put in its place"),
      ("replace_all", "boolean", "Replace every occurrence")),
    None)

  val Run = ToolSpec("run_terminal_command",
    "Runs a command in the user's terminal (its output shown to the user as it runs) and gives its exit status and " +
      "output once it ends. For builds, tests and scripts.",
    schema(List("command"), ("command", "string", "The command, run by the shell"),
      ("timeout_seconds", "integer", "Stop it after this many seconds (default 600)")),
    None)

  /** The tools the client's capabilities allow, in the session `sessionId` over `cwd`. */
  def toolset(client: ClientConnection, capabilities: ClientCapabilities, sessionId: SessionId, cwd: String): Toolset =
    val offered = Chunk(
      Option.when(capabilities.fs.readTextFile)(Read),
      Option.when(capabilities.fs.writeTextFile)(Write),
      Option.when(capabilities.fs.readTextFile && capabilities.fs.writeTextFile)(Edit),
      Option.when(capabilities.terminal)(Run),
    ).flatten
    def absolute(path: String) =
      if java.nio.file.Paths.get(path).isAbsolute then path else java.nio.file.Paths.get(cwd).resolve(path).normalize.toString
    def str(a: Json.Obj, k: String) = a.get(k).flatMap(_.asString)
    def int(a: Json.Obj, k: String) = a.get(k).flatMap(_.asNumber).map(_.value.longValue)
    def failure(e: RpcError) = RuntimeException(e.message)
    new Toolset:
      val tools: Chunk[ToolSpec] = offered
      def call(name: String, arguments: Json.Obj): Task[Json] = name match
        case "read_text_file" =>
          ZIO.fromOption(str(arguments, "path")).orElseFail(IllegalArgumentException("path is required")).flatMap: path =>
            client.readTextFile(ReadTextFileRequest(sessionId, absolute(path), int(arguments, "line"), int(arguments, "limit")))
              .mapBoth(failure, r => Json.Obj("text" -> Json.Str(r.content)))
        case "write_text_file" =>
          (for
            path    <- ZIO.fromOption(str(arguments, "path")).orElseFail(IllegalArgumentException("path is required"))
            content <- ZIO.fromOption(str(arguments, "content")).orElseFail(IllegalArgumentException("content is required"))
            _       <- client.writeTextFile(WriteTextFileRequest(sessionId, absolute(path), content)).mapError(failure)
          yield Json.Obj("written" -> Json.Str(absolute(path))))
        case "edit_text_file" =>
          (for
            path <- ZIO.fromOption(str(arguments, "path")).orElseFail(IllegalArgumentException("path is required"))
            old  <- ZIO.fromOption(str(arguments, "old_text")).orElseFail(IllegalArgumentException("old_text is required"))
            nu   <- ZIO.fromOption(str(arguments, "new_text")).orElseFail(IllegalArgumentException("new_text is required"))
            text <- client.readTextFile(ReadTextFileRequest(sessionId, absolute(path), None, None)).mapBoth(failure, _.content)
            out  <- ZIO.fromEither(LocalTools.replaced(text, old, nu, arguments.get("replace_all").contains(Json.Bool(true))))
                      .mapError(IllegalArgumentException(_))
            _    <- client.writeTextFile(WriteTextFileRequest(sessionId, absolute(path), out)).mapError(failure)
          yield Json.Obj("edited" -> Json.Str(absolute(path))))
        case "run_terminal_command" =>
          ZIO.fromOption(str(arguments, "command")).orElseFail(IllegalArgumentException("command is required")).flatMap: command =>
            run(client, sessionId, cwd, command, int(arguments, "timeout_seconds").getOrElse(600L))
        case other => ZIO.fail(IllegalArgumentException(s"unknown tool: $other"))

  /**
   * A command in a client terminal: created, shown in its tool call, waited for (the client answers
   * `terminal/wait_for_exit` when it exits), its output read, released; killed and released when stopped or past
   * its timeout.
   */
  private def run(client: ClientConnection, sessionId: SessionId, cwd: String, command: String, timeoutSeconds: Long): Task[Json] =
    def failure(e: RpcError) = RuntimeException(e.message)
    // created interruptibly (a client that never answers doesn't hold the call), released however the call ends
    client.createTerminal(CreateTerminalRequest(sessionId, "bash", List("-c", command), cwd = Some(cwd),
        outputByteLimit = Some(1024L * 1024))).mapBoth(failure, _.terminalId).flatMap: id =>
      val release = client.releaseTerminal(ReleaseTerminalRequest(sessionId, id)).ignore
      (for
        call   <- currentCall.get
        _      <- ZIO.foreachDiscard(call): c =>
                    client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(c,
                      content = Some(List(ToolCallContent.Terminal(id)))))).ignore
        exited <- client.waitForTerminalExit(WaitForTerminalExitRequest(sessionId, id)).mapError(failure)
                    .timeout(Duration.fromSeconds(timeoutSeconds))
                    .onInterrupt(client.killTerminal(KillTerminalRequest(sessionId, id)).ignore)
        _      <- ZIO.when(exited.isEmpty)(client.killTerminal(KillTerminalRequest(sessionId, id)).ignore)
        output <- client.terminalOutput(TerminalOutputRequest(sessionId, id)).mapError(failure)
      yield exited match
        case None => Json.Obj("is_timed_out" -> Json.Bool(true), "output" -> Json.Str(output.output))
        case Some(e) =>
          Json.Obj(
            "exit_code" -> e.exitCode.fold(Json.Null: Json)(c => Json.Num(c)),
            "signal" -> e.signal.fold(Json.Null: Json)(Json.Str(_)),
            "output" -> Json.Str(output.output),
            "truncated" -> Json.Bool(output.truncated),
          )
      ).ensuring(release)
