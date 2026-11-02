package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.{AgentLoop, LlmResult, ParallelLoop}
import com.jamesward.zio_decisions.orchestration.AgentLoop.{ArgumentsRequest, ReplyRequest}
import com.jamesward.zioacp.{AcpClient, AgentCommand, AgentConnection}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/**
 * The generative model of Jev's loops, whoever provides it ([[Provider]]): it writes a chosen tool's arguments, the
 * reply, the parallel plan and status answers. Jev decides; the writer only writes.
 */
trait Writer extends AgentLoop.Llm, ParallelLoop.Planner:
  def status(question: String, tree: String): Task[String]

  /** Gets ready to write (an ACP agent's process and first session started), so the first writing doesn't wait for it. */
  def warm: UIO[Unit] = ZIO.unit

/** Where a session's writing comes from: ACP's session modes, each with its own models (the model picker). */
enum Provider(val id: String, val title: String, val description: String) derives CanEqual:
  case Bedrock extends Provider("bedrock", "Bedrock", "A Bedrock model writes the arguments and the replies")
  case Acp extends Provider("acp", "ACP", "An ACP agent (Kiro, Claude Code, …) writes the arguments and the replies")

object Provider:
  def of(id: String): Option[Provider] = values.find(_.id == id)

  /** The ACP provider's models are its agents, by name: `acp:<name>`. */
  val AcpPrefix = "acp:"
  def ofModel(model: String): Provider = if model.startsWith(AcpPrefix) then Acp else Bedrock

/**
 * An ACP agent as the writer: each writing is a prompt, the agent asked to answer with only what's asked (a JSON object
 * for arguments and plans, text for replies) and to use no tools; any permission it asks for is refused. The agent's
 * process is started on first use and kept for the life of `scope`.
 *
 * Every writing is in a clean session of its own: each prompt is whole (the request, the steps it needs, the tool), so
 * an earlier writing's history would only make the model read more (it slowed later writings in a reused session). A
 * spare session is always being made, so a writing doesn't wait for one (after Kiro's first, a new session takes
 * milliseconds), and a session is closed once its writing is answered. All of them work in one scratch directory with
 * no MCP servers.
 */
final class AcpWriter(name: String, command: AgentCommand, connection: UIO[Either[Throwable, AcpWriter.Connected]],
    prompts: WritingPrompts) extends Writer:
  import AcpWriter.*

  def arguments(request: ArgumentsRequest): Task[LlmResult[Json.Obj]] =
    val prompt = s"${Prompts.Arguments}\n\n${prompts.argumentsPrompt(request)}\n\n" +
      s"Answer with only the arguments for ${request.tool.name}: one JSON object that fits its input schema, nothing else " +
      "(no prose, no code fence). Don't use any tool: the agent will make the call."
    ask(prompt).flatMap((text, usage, ms) =>
      ZIO.fromOption(jsonObject(text)).orElseFail(RuntimeException(s"$name wrote no JSON object: ${text.take(300)}"))
        .map(LlmResult(_, usage._1, usage._2, ms)))

  def reply(request: ReplyRequest): Task[LlmResult[String]] =
    val (system, prompt) = prompts.replyPrompt(request)
    ask(s"$system\n\n$prompt\n\nAnswer with only the reply, in Markdown. Don't use any tool.")
      .map((text, usage, ms) => LlmResult(text.trim, usage._1, usage._2, ms))

  /** Several calls written at once, as one JSON object of `calls`: a batch to run at the same time, or calls in order. */
  override def workflow(request: AgentLoop.WorkflowRequest): Task[LlmResult[List[AgentLoop.PlannedCall]]] =
    val schema = BedrockLlmTransport.workflowSchema(request.tools.map(_.name), request.maxCalls)
    ask(s"${Prompts.Arguments}\n\n${prompts.workflowPrompt(request)}\n\nAnswer with only one JSON object fitting this schema: " +
      s"${schema.toJson}. No prose, no code fence. Don't use any tool: the agent will make the calls.").flatMap: (text, usage, ms) =>
      ZIO.fromOption(jsonObject(text)).orElseFail(RuntimeException(s"$name wrote no JSON object: ${text.take(300)}"))
        .map(o => LlmResult(BedrockLlmTransport.plannedCalls(o), usage._1, usage._2, ms))

  def plan(request: ParallelLoop.PlanRequest): Task[LlmResult[ParallelLoop.Plan]] =
    ask(s"${Prompts.Planner}\n\n${prompts.planPrompt(request)}\n\nAnswer with only the plan as one JSON object fitting " +
      s"this schema: ${ParallelLoop.Plan.schema.toJson}. No prose. Don't use any tool.").flatMap: (text, usage, ms) =>
      ZIO.fromEither(jsonObject(text).toRight(s"$name wrote no plan").flatMap(_.toJson.fromJson[ParallelLoop.Plan]))
        .mapBoth(RuntimeException(_), LlmResult(_, usage._1, usage._2, ms))

  /** The agent's process and its first session, started now rather than at the first writing (seconds, for Kiro). */
  override def warm: UIO[Unit] = connection.flatMap {
    case Right(c) => c.spare.get.flatMap(ZIO.foreachDiscard(_)(_.await))
    case Left(_)  => ZIO.unit
  }

  def status(question: String, tree: String): Task[String] =
    ask(prompts.statusPrompt(question, tree) + "\n\nDon't use any tool.").map(_._1.trim)

  /**
   * `prompt` in a fresh session of its own, where the agent may use its web search (a search tool call is allowed
   * once, every other permission refused): the agent's text. The session is used for this prompt only.
   */
  def search(prompt: String): Task[String] =
    connection.flatMap(ZIO.fromEither(_)).flatMap: c =>
      ZIO.acquireReleaseWith(c.take.zip(Promise.make[Nothing, String]).tap((s, found) => c.searching.update(_.updated(s.session, found))))(
          (s, _) => c.searching.update(_ - s.session)): (slot, found) =>
        for
          // Kiro: the session's agent becomes the one with only web_search (allowed without asking)
          _       <- c.agent.setSessionMode(SetSessionModeRequest(slot.session, SessionModeId(SearchAgent))).ignore
                       .when(slot.modes(SearchAgent))
          started <- Clock.nanoTime
          // the search's own results, as soon as its tool call completes: the agent's turn is cancelled rather than
          // waited for while it writes an answer from them (seconds), and the results are the answer, each a cited link
          // (the turn isn't interrupted: `session/cancel` ends it, in the background)
          turn    <- c.agent.prompt(PromptRequest(slot.session, List(ContentBlock.text(prompt))))
                       .mapError(e => RuntimeException(s"$name: ${e.message}")).forkIn(c.scope)
          raced   <- turn.join.raceEither(found.await.disconnect)
          _       <- c.agent.cancel(slot.session).ignore.when(raced.isRight)
          ended   <- Clock.nanoTime
          written <- raced.fold(_ => slot.text.get.map(_.toString), ZIO.succeed(_))
          _       <- ZIO.logDebug(s"ACP search $name in ${(ended - started) / 1000000}ms (session ${slot.session.value.take(8)}): prompt=$prompt answer=$written")
        yield written

  /** `prompt` as it is, in a clean session: the agent's text and the time it took (for the evals' replays). */
  private[soness] def raw(prompt: String): Task[(String, Long)] = ask(prompt).map((text, _, ms) => (text, ms))

  /** One prompt in a clean session (closed after): the agent's text, its tokens (in, out) when it reports them, and the time taken. */
  private def ask(prompt: String): Task[(String, (Int, Int), Long)] =
    connection.flatMap(ZIO.fromEither(_)).flatMap: c =>
      ZIO.acquireReleaseWith(c.take)(c.close): slot =>
        for
          started  <- Clock.nanoTime
          response <- c.agent.prompt(PromptRequest(slot.session, List(ContentBlock.text(prompt))))
                        .mapError(e => RuntimeException(s"$name: ${e.message}"))
          ended    <- Clock.nanoTime
          written  <- slot.text.get.map(_.toString)
          _        <- ZIO.logDebug(s"ACP writer $name in ${(ended - started) / 1000000}ms (session ${slot.session.value.take(8)}): prompt=$prompt answer=$written")
        yield (written, response.usage.fold((0, 0))(u => (u.inputTokens.toInt, u.outputTokens.toInt)), (ended - started) / 1000000)

object AcpWriter:
  /**
   * The model Kiro writes with, unless its command names one (`--model`): Sonnet answers soness's prompts (tens of
   * thousands of characters) in about half Opus's time (`KiroLatencySpec`).
   */
  val KiroModel = "claude-sonnet-5.5"

  /** A session, made ahead: where its text goes, and the modes the agent offers in it. */
  final case class Slot(session: SessionId, text: Ref[StringBuilder], modes: Set[String] = Set.empty)

  /**
   * The agents soness gives Kiro in its sessions' directory (Kiro loads the agent files of a workspace's `.kiro/agents` as session
   * modes): writing with no tools at all (no built-in, no MCP server: the writer only answers), and searching with only
   * `web_search` (no other built-in, no MCP server), allowed without asking. An agent without these modes is told in its prompt to use no
   * tool, and its permission requests are refused.
   */
  val WriteAgent = "soness-write"
  val SearchAgent = "soness-search"

  private def agentConfig(name: String, description: String, tools: List[String], model: Option[String]): String =
    Json.Obj(Chunk[(String, Json)](
      "name" -> Json.Str(name), "description" -> Json.Str(description), "prompt" -> Json.Null,
      "tools" -> Json.Arr(tools.map(Json.Str(_))*), "allowedTools" -> Json.Arr(tools.map(Json.Str(_))*),
      "mcpServers" -> Json.Obj(), "useLegacyMcpJson" -> Json.Bool(false), "resources" -> Json.Arr(),
    ) ++ Chunk.from(model.map(m => "model" -> Json.Str(m)))).toJson

  /**
   * A connected agent: where each of its sessions' text goes, the warm sessions by loop, and the spare being made; its
   * sessions work in `dir`, made when it connected and removed when its scope closes. `searching` are the sessions
   * running a [[AcpWriter.search]], the only ones whose web tool calls are allowed.
   */
  final case class Connected(agent: AgentConnection, sessions: Ref[Map[SessionId, Ref[StringBuilder]]], dir: java.nio.file.Path,
      spare: Ref[Option[Fiber[Throwable, Slot]]], scope: Scope,
      searching: Ref[Map[SessionId, Promise[Nothing, String]]]):
    /** A new session, warm: in the scratch directory, its text collected. */
    def open: Task[Slot] =
      for
        created <- agent.newSession(NewSessionRequest(dir.toString, Nil)).mapError(e => RuntimeException(e.message))
        session  = created.sessionId
        modes    = created.modes.toList.flatMap(_.availableModes).map(_.id.value).toSet
        _       <- ZIO.logDebug(s"ACP writer session ${session.value.take(8)}: modes ${modes.mkString(", ")}")
        // writing with no tools, when the agent offers soness's writing agent (Kiro)
        _       <- agent.setSessionMode(SetSessionModeRequest(session, SessionModeId(WriteAgent))).mapError(e => RuntimeException(e.message))
                     .when(modes(WriteAgent))
        text    <- Ref.make(new StringBuilder)
        _       <- sessions.update(_.updated(session, text))
      yield Slot(session, text, modes)

    /** A writing's session, done with: forgotten here, and closed in the agent (in the background; one that can't is left). */
    def close(slot: Slot): UIO[Unit] =
      sessions.update(_ - slot.session) *>
        agent.closeSession(CloseSessionRequest(slot.session)).ignore.forkIn(scope).unit

    /** The spare session (or, when there's none, a new one), with the next spare started. */
    def take: Task[Slot] =
      for
        ready <- spare.getAndSet(None)
        // awaited, not joined: a join takes on the fiber's FiberRefs (the log annotations and loop of whichever loop
        // started it), and every later log line and writing of this loop would be that one's
        slot  <- ready.fold(open)(_.await.flatMap(exit => exit).orElse(open))
        _     <- prepare
      yield slot

    /** Starts making the next spare session, in the background, for the life of the agent. */
    def prepare: UIO[Unit] = open.forkIn(scope).flatMap(f => spare.set(Some(f)))

  /**
   * The ACP client side: the agent's messages collected per session; its permission requests refused (no tools),
   * except a search tool call in a searching session, allowed once.
   */
  private final class Collecting(sessions: Ref[Map[SessionId, Ref[StringBuilder]]], searching: Ref[Map[SessionId, Promise[Nothing, String]]])
      extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] = n.update match
      case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) =>
        sessions.get.flatMap(_.get(n.sessionId).fold(ZIO.unit)(_.update(_.append(t))))
      // a searching session's search that completed with results: they're the search's answer
      case SessionUpdate.ToolCallUpdated(u) if u.kind.contains(ToolKind.Search) && u.status.contains(ToolCallStatus.Completed) =>
        searching.get.flatMap(_.get(n.sessionId).fold(ZIO.unit)(found =>
          ZIO.foreachDiscard(u.rawOutput.flatMap(searchResults))(found.succeed)))
      case _ => ZIO.unit
    def requestPermission(r: RequestPermissionRequest) =
      searching.get.map: ids =>
        val web = r.toolCall.kind.contains(ToolKind.Search)
        val allow = r.options.find(_.kind == PermissionOptionKind.AllowOnce)
        allow.filter(_ => web && ids.contains(r.sessionId)) match
          case Some(option) => RequestPermissionResponse(RequestPermissionOutcome.Selected(option.optionId))
          case None         => RequestPermissionResponse(RequestPermissionOutcome.Cancelled)

  /**
   * A search tool call's output (Kiro's `web_search`: `{"items": [{"Json": {"results": [{title, url, snippet, domain}]}}]}`)
   * as Markdown: one cited link a result, with its snippet; none without results.
   */
  def searchResults(output: Json): Option[String] =
    def str(o: Json.Obj, k: String) = o.get(k).flatMap(_.asString).map(_.trim).filter(_.nonEmpty)
    val results = output.asObject.flatMap(_.get("items")).flatMap(_.asArray).toList.flatten
      .flatMap(i => i.asObject.flatMap(_.get("Json")).flatMap(_.asObject).flatMap(_.get("results")).flatMap(_.asArray).toList.flatten)
      .flatMap(_.asObject).flatMap(r => str(r, "url").map(url => (str(r, "title").getOrElse(url), url, str(r, "snippet"))))
    Option.when(results.nonEmpty)(results.map((title, url, snippet) => s"- [$title]($url)${snippet.fold("")(" — " + _)}").mkString("\n"))

  /** A writer of the agent `command`, started on first use and kept for the life of `scope`. */
  def make(name: String, command: AgentCommand, scope: Scope, prompts: WritingPrompts): UIO[AcpWriter] =
    connect(command, scope).memoize.map(AcpWriter(name, command, _, prompts))

  /** A writer of the agent at the other end of `transport` (in tests, one served in memory), for the life of the scope. */
  def over(name: String, transport: com.jamesward.zioacp.Transport, prompts: WritingPrompts): ZIO[Scope, Throwable, AcpWriter] =
    connected(None, (client, _) => AcpClient.connect(transport, client)).flatMap(ZIO.fromEither(_)).map(c => AcpWriter(name, AgentCommand(name), ZIO.succeed(Right(c)), prompts))

  /**
   * The command a writer's agent is launched with, and the model its agents' configs name: Kiro (`kiro-cli acp`, no
   * `--agent` of its own) gets soness's tool-less writing agent, and [[KiroModel]] unless the command names a model.
   */
  def launched(command: AgentCommand): (AgentCommand, Option[String]) =
    val kiro = java.nio.file.Path.of(command.command).getFileName.toString.startsWith("kiro") && command.args.contains("acp") &&
      !command.args.contains("--agent")
    val model = Option.when(kiro && !command.args.contains("--model"))(KiroModel)
    (if kiro then command.copy(args = command.args ++ List("--agent", WriteAgent) ++ model.toList.flatMap(m => List("--model", m))) else command,
      model)

  private def connect(command: AgentCommand, scope: Scope): UIO[Either[Throwable, Connected]] =
    // started in the scratch directory: Kiro loads the agents of its working directory's `.kiro/agents`, not a session's
    // and Kiro with soness's tool-less agent as its default, so a new session starts none of the user's MCP servers
    val (started, model) = launched(command)
    connected(model, (client, dir) => AcpClient.launch(started.copy(cwd = started.cwd.orElse(Some(dir))), client,
      line => ZIO.logDebug(s"ACP writer ${command.command} stderr: $line")))
      .provideEnvironment(ZEnvironment[Scope](scope))

  private def connected(model: Option[String], open: (com.jamesward.zioacp.Client, java.nio.file.Path) => ZIO[Scope, Throwable, AgentConnection])
      : ZIO[Scope, Nothing, Either[Throwable, Connected]] =
    (for
      sessions <- Ref.make(Map.empty[SessionId, Ref[StringBuilder]])
      searching <- Ref.make(Map.empty[SessionId, Promise[Nothing, String]])
      dir      <- ZIO.acquireRelease(ZIO.attemptBlocking(java.nio.file.Files.createTempDirectory("soness-writer")))(d =>
                    ZIO.attemptBlocking(java.nio.file.Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(java.nio.file.Files.delete(_))).ignore)
      _        <- ZIO.attemptBlocking:
                    val agents = java.nio.file.Files.createDirectories(dir.resolve(".kiro").resolve("agents"))
                    java.nio.file.Files.writeString(agents.resolve(s"$WriteAgent.json"),
                      agentConfig(WriteAgent, "soness's writer: answers only, with no tools", Nil, model))
                    java.nio.file.Files.writeString(agents.resolve(s"$SearchAgent.json"),
                      agentConfig(SearchAgent, "soness's web search: web_search only",
                        ToolResolver.catalog.covering("kiro", Set(ToolResolver.Capability.WebSearch)), model))
      agent    <- open(Collecting(sessions, searching), dir)
      _        <- agent.initialize(InitializeRequest(ProtocolVersion.V1, ClientCapabilities())).mapError(e => RuntimeException(e.message))
      spare    <- Ref.make(Option.empty[Fiber[Throwable, Slot]])
      scope    <- ZIO.scope
      c         = Connected(agent, sessions, dir, spare, scope, searching)
      // the first session, made now: the first writing doesn't wait for it
      _        <- c.prepare
    yield c).either

  /** The first JSON object in `text` (a fenced block, prose around it), whole. */
  def jsonObject(text: String): Option[Json.Obj] =
    val starts = text.indices.filter(text.charAt(_) == '{')
    starts.iterator.flatMap(i => balanced(text, i).flatMap(end => text.substring(i, end + 1).fromJson[Json.Obj].toOption)).nextOption()

  private def balanced(text: String, start: Int): Option[Int] =
    @scala.annotation.tailrec
    def scan(i: Int, depth: Int, inString: Boolean, escaped: Boolean): Option[Int] =
      if i >= text.length then None
      else
        val c = text.charAt(i)
        if inString then scan(i + 1, depth, !(c == '"' && !escaped), !escaped && c == '\\')
        else if c == '"' then scan(i + 1, depth, true, false)
        else if c == '{' || c == '[' then scan(i + 1, depth + 1, false, false)
        else if c == '}' || c == ']' then (if depth == 1 then Some(i) else scan(i + 1, depth - 1, false, false))
        else scan(i + 1, depth, false, false)
    scan(start, 0, false, false)
