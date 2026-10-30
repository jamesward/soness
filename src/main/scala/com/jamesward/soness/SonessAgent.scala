package com.jamesward.soness

import com.jamesward.zioacp.*
import com.jamesward.zioacp.schema.*
import com.jamesward.zio_decisions.orchestration.{AgentLoop, Interjection, ParallelLoop, ProcessTree}
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json

/** What one prompt turn cost, returned to the client in the `soness` entry of the prompt response's `_meta`. */
final case class TurnMetrics(
  model: String,
  ending: String,
  latencyMs: Long,
  toolCalls: Int,
  llmCalls: Int,
  llmInputTokens: Long,
  llmOutputTokens: Long,
  jevCalls: Int,
  jevInputTokens: Long,
  jevOutputTokens: Long,
  jevLatencyMs: Long = 0,
  llmLatencyMs: Long = 0,
  toolLatencyMs: Long = 0,
  route: Option[String] = None,
) derives JsonCodec:
  def inputTokens: Long = llmInputTokens + jevInputTokens
  def outputTokens: Long = llmOutputTokens + jevOutputTokens

object TurnMetrics:
  val MetaKey = "soness"

  def apply(model: String, ending: String, latencyMs: Long, toolCalls: Int, usage: ModelUsage): TurnMetrics =
    TurnMetrics(model, ending, latencyMs, toolCalls, usage.llmCalls, usage.llmInputTokens, usage.llmOutputTokens,
      usage.jevCalls, usage.jevInputTokens, usage.jevOutputTokens, usage.jevLatencyMs, usage.llmLatencyMs, usage.toolLatencyMs)

  def fromMeta(meta: Option[Json.Obj]): Option[TurnMetrics] =
    meta.flatMap(_.get(MetaKey)).flatMap(_.as[TurnMetrics].toOption)

/**
 * The soness ACP agent. Each session connects to the MCP servers the client passes to `session/new` (stdio and
 * Streamable HTTP; SSE servers and servers that fail to connect are skipped with a warning) and answers prompts with
 * Jev orchestrating those tools and the session's Bedrock model generating, reporting each tool call as a session
 * update and the turn's cost in the response `_meta`. The model is a session config option.
 */
final class SonessAgent private (
  client: ClientConnection,
  http: ZEnvironment[Client],
  catalog: HarnessCatalog,
  parent: Scope,
  sessions: Ref[Map[SessionId, SonessAgent.Session]],
  projects: Ref[Map[String, (SharedTools.Coordinator, Workspace)]],
  capabilities: Ref[ClientCapabilities],
  extensions: List[Extension],
  tree: ProcessTree,
  peers: Option[java.nio.file.Path],
  clientNamesKinds: Ref[Boolean],
) extends Agent:
  import SonessAgent.*

  /**
   * The coordinator and the shared state ([[Workspace]]) of the project at `cwd` (by its real path), made when the first
   * session there opens: every session in the project shares them.
   */
  private def project(cwd: Option[java.nio.file.Path]): UIO[(SharedTools.Coordinator, Workspace)] =
    val key = cwd.flatMap(p => scala.util.Try(p.toRealPath().toString).toOption).getOrElse("")
    projects.get.map(_.get(key)).someOrElseZIO:
      SharedTools.Coordinator.make(locks = Option.when(key.nonEmpty)(SharedTools.Coordinator.lockDir(java.nio.file.Path.of(key))))
        .provideEnvironment(ZEnvironment[Scope](parent))
        .zip(Workspace.make(Option.when(key.nonEmpty)(java.nio.file.Path.of(key)))).flatMap: made =>
        projects.modify(m => m.get(key).fold((made, m.updated(key, made)))(existing => (existing, m)))

  def initialize(request: InitializeRequest): IO[RpcError, InitializeResponse] =
    // what the client can do (its files, its terminals), for the tools offered on them
    capabilities.set(request.clientCapabilities) *>
      clientNamesKinds.set(request.clientInfo.exists(i => (i.name + " " + i.title.getOrElse("")).matches("(?i).*(intellij|jetbrains|air-plugin).*"))) *>
      ZIO.succeed(InitializeResponse(
      ProtocolVersion.V1,
      AgentCapabilities(
        mcpCapabilities = McpCapabilities(http = true),
        // additional directories are workspace roots to look for skills in
        // every session is kept in the project's event log: listed, loaded (its history replayed) and resumed
        loadSession = true,
        sessionCapabilities = SessionCapabilities(close = Some(Marker()), additionalDirectories = Some(Marker()),
          list = Some(Marker()), resume = Some(Marker())),
      ),
      agentInfo = Some(Implementation("soness", Soness.version)),
      // a message sent while a turn runs can go to it at once (`_session/steering`): IntelliJ IDEA's "send now"
      meta = Some(Json.Obj(SonessAgent.SteeringMeta -> Json.Obj("supported" -> Json.Bool(true)))),
    ))

  def newSession(request: NewSessionRequest): IO[RpcError, NewSessionResponse] =
    Random.nextUUID.map(uuid => SessionId(uuid.toString)).flatMap(open(_, request, resumed = false))
      .map((id, options) => NewSessionResponse(id, configOptions = options))

  /** `session/load`: the session, set up again, with its history replayed from the event log before the answer. */
  override def loadSession(request: LoadSessionRequest): IO[RpcError, LoadSessionResponse] =
    for
      events <- eventLog(request.cwd).fold(ZIO.succeed(Nil))(_.read)
      _      <- ZIO.fail(RpcError.invalidParams(s"no session ${request.sessionId.value} in this project"))
                  .unless(events.exists(_.session == request.sessionId.value))
      opened <- open(request.sessionId, NewSessionRequest(request.cwd, request.mcpServers, request.additionalDirectories, request.meta),
                  resumed = true)
      _      <- ZIO.foreachDiscard(EventLog.replay(events, request.sessionId.value))(u => client.sessionUpdate(request.sessionId, u).ignoreLogged)
    yield LoadSessionResponse(configOptions = opened._2)

  /**
   * `session/resume`: the session, set up again, with no replay (the client has its history); its tasks go on from its
   * last, and its earlier work is recalled from the event log. A session the log doesn't have is refused, as for
   * `session/load` (with no event log, `SONESS_EVENT_LOG=false`, there's nothing to check it against, so it's taken).
   */
  override def resumeSession(request: ResumeSessionRequest): IO[RpcError, ResumeSessionResponse] =
    for
      events <- ZIO.foreach(eventLog(request.cwd))(_.read)
      _      <- ZIO.fail(RpcError.invalidParams(s"no session ${request.sessionId.value} in this project"))
                  .when(events.exists(es => !es.exists(_.session == request.sessionId.value)))
      opened <- open(request.sessionId, NewSessionRequest(request.cwd, request.mcpServers, request.additionalDirectories, request.meta),
                  resumed = true)
    yield ResumeSessionResponse(configOptions = opened._2)

  /** `session/list`: the sessions of the project at `cwd` (every one its event log has), newest first. */
  override def listSessions(request: ListSessionsRequest): IO[RpcError, ListSessionsResponse] =
    ZIO.foreach(request.cwd.flatMap(eventLog))(_.read).map: events =>
      ListSessionsResponse(EventLog.sessions(events.getOrElse(Nil)).map(s =>
        SessionInfo(SessionId(s.session), s.cwd, title = s.title, updatedAt = Some(s.updatedAt))))

  /** The event log of the project at `cwd`; none when `SONESS_EVENT_LOG` is `false` or `cwd` isn't a path. */
  private def eventLog(cwd: String): Option[EventLog.Log] =
    Option.when(!sys.env.get("SONESS_EVENT_LOG").exists(_.trim.equalsIgnoreCase("false")))(())
      .flatMap(_ => scala.util.Try(java.nio.file.Paths.get(cwd)).toOption.filter(_.isAbsolute)).map(EventLog.at)

  private def open(id: SessionId, request: NewSessionRequest, resumed: Boolean)
      : IO[RpcError, (SessionId, Option[List[SessionConfigOption]])] =
    val cwd = scala.util.Try(java.nio.file.Paths.get(request.cwd)).toOption
    val fromClient = request.mcpServers.collect:
      case McpServer.Http(name, url, headers, _) => McpEndpoint.Http(name, url, headers.map(h => h.name -> h.value).toMap)
      case McpServer.Stdio(name, command, args, env, _) =>
        McpEndpoint.Stdio(name, command, args, env.map(e => e.name -> e.value).toMap, cwd)
    val skipped = request.mcpServers.collect { case McpServer.Sse(name, _, _, _) => s"$name (sse)" }
    for
      _         <- ZIO.logWarning(s"skipping SSE MCP servers, which soness doesn't support: ${skipped.mkString(", ")}")
                     .when(skipped.nonEmpty)
      // the project's own servers (its `.mcp.json`), after the client's; `SONESS_PROJECT_MCP=false` leaves them out
      useProject <- System.env("SONESS_PROJECT_MCP").map(_.forall(_.trim.toLowerCase != "false")).orElseSucceed(true)
      envVars    <- System.envs.orElseSucceed(Map.empty[String, String])
      declared   <- ZIO.foreach(cwd.filter(_ => useProject))(McpConfig.load(_, envVars.get)).map(_.getOrElse((Nil, Nil)))
      _          <- ZIO.foreachDiscard(declared._2)(reason => ZIO.logWarning(s"skipping a project MCP server: $reason"))
      endpoints  = McpConfig.combine(fromClient, declared._1)
      _          <- ZIO.logInfo(s"project MCP servers (${McpConfig.FileName}): ${declared._1.map(_.describe).mkString(", ")}").when(declared._1.nonEmpty)
      // a child of the agent's scope, so that a session left open is still closed (its stdio servers stopped) at exit
      scope     <- parent.fork
      connected <- McpToolset.connect(endpoints)
                     .provideEnvironment(http.add[Scope](scope))
                     .tapError(_ => scope.close(Exit.unit))
                     .mapError(e => RpcError.invalidParams(message(e)))
      // each reason names its server
      _         <- ZIO.foreachDiscard(connected.failed)((_, reason) => ZIO.logWarning(s"skipping $reason"))
      webFetch  <- System.env("SONESS_WEB_FETCH").map(_.forall(_.trim.toLowerCase != "false")).orElseSucceed(true)
      caps      <- capabilities.get
      // the project's source dependencies, and the client's other workspace roots (ACP's `additionalDirectories`): the
      // IDE's edits and listings, and soness's own file tools, reach them through soness
      dependencies = (cwd.toList.flatMap(SourceDependencies.of) ++ request.additionalDirectories.flatMap(d =>
                       scala.util.Try(java.nio.file.Paths.get(d)).toOption.filter(p => p.isAbsolute && java.nio.file.Files.isDirectory(p))))
                       .map(_.toAbsolutePath.normalize).distinct.filterNot(p => cwd.exists(c => p.equals(c.toAbsolutePath.normalize)))
      ide        = connected.copy(toolset = cwd.fold(connected.toolset)(root => LocalTools.reaching(root, connected.toolset, dependencies)))
      local     <- System.env("SONESS_LOCAL_TOOLS").map(_.forall(_.trim.toLowerCase != "false")).orElseSucceed(true)
      // every MCP server's tools, and soness's own only for what they don't cover (TOOLS_SPEC.md)
      resolution = ToolResolver.resolved(ide, declared._1.map(_.name).toSet -- fromClient.map(_.name),
                     List((ClientTools.toolset(client, caps, id, request.cwd), ToolResolver.ToolSource.AcpClient, "acp")) ++
                       cwd.filter(_ => local).map(root => (LocalTools.toolset(root, dependencies), ToolResolver.ToolSource.Local, "local")) ++
                       Option.when(webFetch)((WebFetch.toolset(http), ToolResolver.ToolSource.Builtin, "builtin")))
      toolset    = resolution.toolset
      _         <- ZIO.logInfo(s"session ${id.value}: ${resolution.report}")
      _         <- ZIO.logDebug(s"session ${id.value}: hidden tools: ${resolution.hidden.map((t, why) => s"${t.name} (${t.source}: $why)").mkString(", ")}")
      selection <- Ref.make(catalog.default)
      noted     <- Ref.make(false)
      openTurns <- Ref.make(0)
      pending   <- Ref.make(Vector.empty[String])
      skills    <- Skills.discover(roots(request))
      // one toolset for all the session's turns, which may run at the same time
      // one coordinator per project directory, for the agent's life: every session in it (a client's several chats,
      // several clients) shares its edit lock and its build and test runs
      projected <- project(cwd)
      (coordinator, workspace) = projected
      // what other sessions already did (a page fetched) is answered from the shared log, and every call is in it
      shared     = workspace.bind(coordinator.bind(toolset))
      turns     <- Ref.make(Map.empty[Int, RunningTurn])
      log        = eventLog(request.cwd)
      events    <- ZIO.foreach(log)(_.read).map(_.getOrElse(Nil))
      // a resumed session's tasks go on from its last
      counter   <- Ref.make(EventLog.lastTask(events, id.value))
      started   <- EventLog.now
      _         <- ZIO.foreachDiscard(log)(_.append(EventLog.Event.SessionStarted(id.value, started, request.cwd)))
                     .unless(events.exists(_.session == id.value))
      callIds   <- Ref.make(0)
      // the session, a node of the agent's one tree (every session's: `/status` shows them all), its tasks below it;
      // it ends when the session closes
      closed    <- Promise.make[Nothing, Unit]
      _         <- scope.addFinalizer(closed.succeed(()))
      named     <- Promise.make[Nothing, ProcessTree.Id]
      title      = events.collectFirst { case t: EventLog.Event.TaskStarted if t.session == id.value => short(t.request) }
      _         <- tree.child(None, ProcessTree.Kind.Session, id.value.take(8), Some(title.fold(request.cwd)(t => s"${request.cwd}: $t")))(n =>
                     named.succeed(n) *> closed.await).forkIn(parent)
      node      <- named.await
      ended     <- Hub.unbounded[Transcript.Turn]
      // every call of the session's tasks allowed, put to the user or refused first ([[Guard]]); Jev, through the
      // session's current model's harness, judges what the rules don't settle
      policy    <- Guard.Policy.fromEnv
      guard     <- Guard.make(policy, calls => harnessOf(selection).flatMap(_.safety(calls)), client.requestPermission, id, cwd, dependencies)
                     .provideEnvironment(ZEnvironment[Scope](scope))
      _         <- sessions.update(_.updated(id, Session(shared, skills, selection, noted, openTurns, pending, scope, cwd, turns, counter, callIds, tree,
                     ended, connected.instructions :+ ("soness" -> resolution.report), log, node, workspace, guard)))
      // the session's commands (its own, and those this build's extensions have for it), once the client knows the
      // session; and its watchdog, for the session's life
      opened    <- sessions.get.map(_(id))
      _         <- (ZIO.sleep(200.millis) *> client.sessionUpdate(id, SessionUpdate.AvailableCommandsUpdate(
                     Commands.available ++ extensions.flatMap(_.commands(handle(id, opened))))))
                     .forkIn(scope)
      limits    <- Watchdog.limits
      // Jev, through the session's current model's harness, judges its stalled calls
      judge      = (stalled: List[com.jamesward.zio_decisions.orchestration.Overseer.Stalled]) =>
                     harnessOf(selection).flatMap(_.oversee(stalled))
      _         <- Watchdog.watch(tree, limits, note => client.sessionUpdate(id, SessionUpdate.AgentMessageChunk(ContentBlock.text(note))).ignoreLogged,
                     judge, Some(node))
                     .forkIn(scope)
      _         <- ZIO.logInfo(s"session ${id.value}: instructions from MCP servers ${connected.instructions.map(_._1).mkString(", ")}")
                     .when(connected.instructions.nonEmpty)
      _         <- ZIO.logInfo(s"session ${id.value} in ${request.cwd}: ${endpoints.size - connected.failed.size} MCP servers, ${connected.toolset.tools.size} tools${if webFetch then " (and web_fetch)" else ""}, ${skills.size} skills; " +
                     s"model ${catalog.defaultModel}")
      _         <- ZIO.logDebug(s"session ${id.value} tools: ${toolset.tools.map(t => s"${t.name}: ${t.description}").mkString("; ")}")
      _         <- ZIO.logDebug(s"session ${id.value} skills: ${skills.map(s => s"${s.title}: ${s.description}").mkString("; ")}")
      _         <- ZIO.logInfo(s"session ${id.value} resumed").when(resumed)
      // the session's writer, started now (an ACP agent takes seconds to start): its first turn doesn't wait for it
      _         <- ZIO.foreachDiscard(catalog.harness(catalog.defaultModel))(_.warm.forkIn(scope))
      // what this build adds to every session (the development build's observer)
      session   <- sessions.get.map(_(id))
      _         <- ZIO.foreachDiscard(extensions)(_.started(handle(id, session)))
    yield (id, Some(configOptions(catalog.default)))

  /**
   * The session's choices: its writer (`model`, the editor's model picker), its decision model and its web search; a
   * change takes effect from the session's next turn.
   */
  override def setSessionConfigOption(request: SetSessionConfigOptionRequest): IO[RpcError, SetSessionConfigOptionResponse] =
    for
      session <- find(request.sessionId)
      value   <- request.value match
                   case SessionConfigValue.ValueId(id) => ZIO.succeed(id.value)
                   case other                          => ZIO.fail(RpcError.invalidParams(s"expected a value id, not $other"))
      current <- session.selection.get
      next    <- request.configId.value match
                   case ModelOption =>
                     catalog.unavailableModels.find(_.id == value).fold(
                       ZIO.succeed(current.copy(model = value)).when(catalog.models.contains(value)).someOrFail(RpcError.invalidParams(s"unknown model: $value")))(
                       c => ZIO.fail(unavailable(c)))
                   case DecisionOption if catalog.composed =>
                     chosen(catalog.decisions, value, "decision model").map(c => current.copy(decision = Some(c.id)))
                   case SearchOption if catalog.composed =>
                     if value == NoSearch then ZIO.succeed(current.copy(search = None))
                     else chosen(catalog.searches, value, "web search").map(c => current.copy(search = Some(c.id)))
                   case other => ZIO.fail(RpcError.invalidParams(s"unknown config option: $other"))
      _       <- ZIO.fromOption(catalog.harness(next)).orElseFail(RpcError.invalidParams(s"no harness for $next"))
                   .flatMap(h => h.warm.forkIn(session.scope))
      _       <- session.selection.set(next) *> ZIO.logInfo(s"session ${request.sessionId.value}: ${describe(next)}")
      // a search that changed is noted again in the next turn when there's none
      _       <- session.searchNoted.set(false).when(next.search != current.search)
    yield SetSessionConfigOptionResponse(configOptions(next))

  /** The choice of `id`, when it's one and available. */
  private def chosen(choices: List[HarnessCatalog.Choice], id: String, what: String): IO[RpcError, HarnessCatalog.Choice] =
    ZIO.fromOption(choices.find(_.id == id)).orElseFail(RpcError.invalidParams(s"unknown $what: $id"))
      .flatMap(c => if c.available then ZIO.succeed(c) else ZIO.fail(unavailable(c)))

  private def unavailable(c: HarnessCatalog.Choice): RpcError =
    RpcError.invalidParams(s"${c.name.stripSuffix(" (unavailable)")} is unavailable: ${c.unavailable.getOrElse("")}")

  private def describe(selection: HarnessCatalog.Selection): String =
    s"decision model ${selection.decision.getOrElse("-")}, model ${selection.model}, web search ${selection.search.getOrElse(NoSearch)}"

  /** The session's current harness: its selection's. */
  private def harnessOf(selection: Ref[HarnessCatalog.Selection]): Task[Harness] =
    selection.get.flatMap(s => ZIO.fromOption(catalog.harness(s)).orElseFail(IllegalStateException(s"no harness for ${describe(s)}")))

  override def closeSession(request: CloseSessionRequest): IO[RpcError, CloseSessionResponse] =
    for
      session <- find(request.sessionId)
      _       <- sessions.update(_ - request.sessionId)
      _       <- session.scope.close(Exit.unit)
    yield CloseSessionResponse()

  /** A prompt turn; its log lines, the harness's included, carry the session id. */
  def prompt(request: PromptRequest, turn: PromptTurn): IO[RpcError, PromptResponse] =
    ZIO.logAnnotate("session", request.sessionId.value):
      sessions.get.map(_.get(request.sessionId)).flatMap:
        case None => answer(request, turn)
        case Some(session) =>
          ZIO.acquireReleaseWith(session.openTurns.update(_ + 1) *> session.pending.getAndSet(Vector.empty).flatMap(said =>
              // what background tasks said while no turn was open, first
              ZIO.when(said.nonEmpty)(client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(
                said.mkString("While no turn was open, background work said:\n\n", "\n\n", "\n\n---\n\n")))).ignoreLogged)))(_ =>
            session.openTurns.update(_ - 1))(_ => answer(request, turn))

  /**
   * `text` said in the session, at once; when no prompt turn is open (a client such as IntelliJ IDEA shows nothing
   * outside one), it's kept too, and said again when the next turn starts.
   */
  private def deliver(sessionId: SessionId, session: Session, text: String): UIO[Unit] =
    client.sessionUpdate(sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(text + "\n\n"))).ignoreLogged *>
      session.openTurns.get.flatMap: open =>
        (session.pending.update(_ :+ text) *> ZIO.logDebug(s"kept for the next turn: ${short(text)}")).when(open == 0).unit

  /**
   * A prompt: run as a task of its own when the session is idle. While tasks run (a client that sends a prompt before
   * the turn before it ended), Jev decides what it is ([[Interjection]]): a new task, started at once alongside them;
   * steering of a running task, which that task's loop reads at its next decision, the prompt itself answered at once;
   * or a follow-up of a running task, started when it's done, from its reply.
   */
  private def answer(request: PromptRequest, turn: PromptTurn): IO[RpcError, PromptResponse] =
    val text = request.prompt.collect { case ContentBlock.Text(t, _, _) => t }.mkString("\n")
    for
      session  <- find(request.sessionId)
      model    <- session.selection.get.map(_.model)
      harness  <- harnessOf(session.selection).mapError(e => RpcError.internalError(e.getMessage))
      running  <- session.turns.get.map(_.values.toList.sortBy(_.id))
      _        <- ZIO.logDebug(s"prompt while running: ${running.map(_.id).mkString(",")}").when(running.nonEmpty)
      // a command (the session's own, or one of this build's extensions'), answered at once
      commanded <- commandAnswer(request.sessionId, session, text)
      response <- commanded match
                    case Some(answer) =>
                      answer.flatMap(t => client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(t))))
                        .as(routedResponse(TurnMetrics(model, "command", 0, 0, ModelUsage()), "command"))
                    case None =>
                      if running.isEmpty then task(request, turn, session, harness, text, None)
                      else routed(request, turn, session, harness, text, running)
    yield response

  /**
   * `_session/steering` (`{sessionId, prompt}`): a message the client sends into the running turn rather than queueing
   * it (IntelliJ IDEA's "send now"). Jev routes it as it routes a prompt that arrives while tasks run: steering of a
   * task, a stop, a question about the work, a stop of one process; a new request or a follow-up goes to the newest
   * running task as part of its work, since the user wants it now. What it says goes to the running turn's messages,
   * and the answer is `{"outcome": "injected"}`. With nothing running there's no turn to send it into: the method isn't
   * found, so the client sends it as a prompt.
   */
  override def extMethod(method: String, params: Json): IO[RpcError, Json] =
    if method != SonessAgent.SteeringMethod then ZIO.fail(RpcError.methodNotFound(method))
    else
      for
        sessionId <- ZIO.fromOption(params.asObject.flatMap(_.get("sessionId")).flatMap(_.asString)).map(SessionId(_))
                       .orElseFail(RpcError.invalidParams("sessionId is required"))
        blocks    <- ZIO.fromEither(params.asObject.flatMap(_.get("prompt")).getOrElse(Json.Arr()).as[List[ContentBlock]])
                       .mapError(e => RpcError.invalidParams(s"prompt: $e"))
        text       = blocks.collect { case ContentBlock.Text(t, _, _) => t }.mkString("\n")
        session   <- find(sessionId)
        running   <- session.turns.get.map(_.values.toList.sortBy(_.id))
        _         <- ZIO.logDebug(s"steering into the turn: ${running.size} running, text ${text.take(80)}")
        _         <- ZIO.fail(RpcError.methodNotFound(method)).when(running.isEmpty || text.trim.isEmpty)
        // a command (/status, /stop, an extension's) is answered from the process tree at once, not routed
        commanded <- commandAnswer(sessionId, session, text)
        out       <- commanded.fold(ZIO.logAnnotate("session", sessionId.value)(steerNow(sessionId, session, text, running)))(answer =>
                       answer.flatMap(t => client.sessionUpdate(sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(t + "\n\n"))))
                         .as(Json.Obj("outcome" -> Json.Str("injected"))))
      yield out

  /** The answer to `text` when it's a session command (the session's own, or one of this build's extensions'). */
  private def commandAnswer(sessionId: SessionId, session: Session, text: String): UIO[Option[UIO[String]]] =
    ZIO.succeed:
      Commands.named(text).flatMap((name, input) =>
        extensions.iterator.flatMap(e => Option.when(e.commands(handle(sessionId, session)).exists(_.name == name))(e))
          .flatMap(_.command(handle(sessionId, session), name, input)).nextOption())
        .orElse(Commands.parse(text).map(commandText(session, _)))

  private def steerNow(sessionId: SessionId, session: Session, text: String, running: List[RunningTurn]): IO[RpcError, Json] =
    val say = (note: String) => client.sessionUpdate(sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(note + "\n\n")))
    val injected = Json.Obj("outcome" -> Json.Str("injected"))
    def into(t: RunningTurn, message: String, note: String): IO[RpcError, Json] =
      t.steer(message).flatMap: delivered =>
        if delivered then say(note).as(injected)
        // it ended in the meantime: the client sends it as a prompt
        else ZIO.fail(RpcError.methodNotFound(SonessAgent.SteeringMethod))
    for
      model    <- session.selection.get.map(_.model)
      harness  <- harnessOf(session.selection).mapError(e => RpcError.internalError(e.getMessage))
      views    <- ZIO.foreach(running)(_.view)
      procs    <- processes(session)
      // the client waits for this answer (IntelliJ IDEA gives up after a few seconds: "unconfirmed by agent"), so a
      // route that's slow to come steers the newest task, the likeliest
      decision <- harness.route(text, views, None, procs).timeout(SteeringDeadline).someOrFail(
                    IllegalStateException(s"no route within ${SteeringDeadline.render}")).either
      newest    = running.last
      out      <- decision match
        case Left(error) =>
          ZIO.logWarning(s"couldn't route a message sent into the turn (${message(error)}); steering the newest task") *>
            into(newest, text, s"Got it: I'll apply this to the task I'm working on (\"${short(newest.request)}\").")
        case Right(d) =>
          ZIO.logInfo(s"steering now: ${d.route}") *>
            (d.route match
              case Interjection.Route.Steer(n) =>
                val t = running.find(_.id == n).getOrElse(newest)
                into(t, text, s"Got it: I'll apply this to the task I'm working on (\"${short(t.request)}\").")
              case Interjection.Route.Stop(n) =>
                val t = running.find(_.id == n).getOrElse(newest)
                t.stop.flatMap(stopped => say(if stopped then s"Stopping: \"${short(t.request)}\"." else s"\"${short(t.request)}\" had already finished."))
                  .as(injected)
              case Interjection.Route.Status =>
                // answered in the session when the summary is written (by the writer, which can take a while); the
                // message is confirmed now
                statusText(session, all = false).flatMap(tree =>
                  harness.status(text, tree).catchAll(e => ZIO.succeed(s"$tree\n\n(couldn't summarize: ${message(e)})")))
                  .flatMap(say).ignoreLogged.forkIn(session.scope).as(injected)
              case Interjection.Route.StopProcess(n) =>
                session.tree.stop(ProcessTree.Id(n), "stopped by the user").flatMap(stopped =>
                  say(if stopped then s"Stopped [$n]; its task goes on." else s"[$n] isn't running.")).as(injected)
              case _ =>
                // a further request, sent now rather than queued: part of the work under way
                into(newest, s"The user added this request, to do as part of this work: $text",
                  s"Got it: I'll do this too, as part of what I'm working on (\"${short(newest.request)}\").")
            )
    yield out

  /** A session command's answer, from the session's process tree. */
  private def commandText(session: Session, command: Commands.Command): UIO[String] =
    command match
      case Commands.Command.Status       => statusText(session, all = false)
      case Commands.Command.StatusAll    => statusText(session, all = true)
      case Commands.Command.Stop(target) =>
        session.tree.snapshot.map(_.find(_.id.value == target)).flatMap:
          // a session goes on (it's a chat): its work stops
          case Some(n) if n.state == ProcessTree.State.Running && n.kind == ProcessTree.Kind.Session =>
            session.tree.snapshot.map(_.filter(t => t.parent.contains(n.id) && t.state == ProcessTree.State.Running)).flatMap: tasks =>
              ZIO.foreachDiscard(tasks)(t => session.tree.stop(t.id, "stopped by the user"))
                .as(if tasks.isEmpty then s"[${n.id}] ${n.name} has nothing running." else s"Stopping ${n.name}'s ${tasks.size} task(s).")
          case Some(n) if n.state == ProcessTree.State.Running =>
            session.tree.stop(n.id, "stopped by the user").as(s"Stopping [${n.id}] ${n.kind.toString.toLowerCase} ${n.name}.")
          case Some(n) => ZIO.succeed(s"[${n.id}] ${n.name} isn't running (${n.state.toString.toLowerCase}).")
          case None    => ZIO.succeed(s"There's no process $target. ${Commands.usage}")
      case Commands.Command.Unknown(what) => ZIO.succeed(s"Unknown command $what. ${Commands.usage}")


  /**
   * The process tree as text: every session's (this one marked), what runs (and, with `all`, what ended too), the
   * stalled marked; then the user's other soness processes' ([[Peers]]).
   */
  private def statusText(session: Session, all: Boolean): UIO[String] =
    def shown(nodes: Vector[ProcessTree.Node]) =
      if all then nodes else nodes.filter(n => n.state == ProcessTree.State.Running && n.kind != ProcessTree.Kind.Session ||
        n.kind == ProcessTree.Kind.Session && n.state == ProcessTree.State.Running)
    for
      now    <- Clock.instant
      nodes  <- tree.snapshot
      others <- ZIO.foreach(peers)(Peers.others).map(_.getOrElse(Nil))
    yield
      val here = shown(nodes)
      val there = others.map(p => p -> shown(p.nodes)).filter(_._2.nonEmpty)
      val busy = here.exists(_.kind != ProcessTree.Kind.Session) || there.exists(_._2.exists(_.kind != ProcessTree.Kind.Session))
      if !busy then "Nothing is running." + (if all then "" else " (/status all shows what ran.)")
      else
        s"This session is [${session.node}].\n```\n" + ProcessTree.render(here, now) + "\n```\n" +
          there.map((p, ns) => s"Another soness process (pid ${p.pid}; its ids are its own, `/stop` doesn't reach them):\n```\n" +
            ProcessTree.render(ns, now) + "\n```\n").mkString + Commands.usage

  private def routed(request: PromptRequest, turn: PromptTurn, session: Session, harness: Harness, text: String,
      running: List[RunningTurn]): IO[RpcError, PromptResponse] =
    for
      views    <- ZIO.foreach(running)(_.view)
      procs    <- processes(session)
      decision <- harness.route(text, views, None, procs).timed.either
      response <- decision match
        case Left(error) =>
          ZIO.logWarning(s"couldn't route a message that came while tasks ran (${message(error)}); starting it as a new task") *>
            task(request, turn, session, harness, text, None)
        case Right((elapsed, d)) =>
          val target = d.route match
            case Interjection.Route.Steer(n) => running.find(_.id == n)
            case Interjection.Route.After(n) => running.find(_.id == n)
            case Interjection.Route.Stop(n)  => running.find(_.id == n)
            case _                           => None
          val probabilities = d.probabilities.toList.sortBy(-_._2).map((k, p) => f"$k $p%.2f").mkString(", ")
          ZIO.logInfo(s"interjection: ${d.route} ($probabilities)") *>
            ((d.route, target) match
              case (Interjection.Route.Steer(_), Some(t)) =>
                for
                  delivered <- t.steer(text)
                  model     <- session.selection.get.map(_.model)
                  response  <-
                    if delivered then
                      val note = t.label.fold(s"Got it: I'll apply this to the task I'm working on (\"${short(t.request)}\").")(l =>
                        s"Got it: I'll apply this to the $l running in the background (\"${short(t.request)}\"), and report " +
                          "here what it did when it's done.\n\n")
                      client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(note))) *>
                        // a background task has no prompt turn of its own: this one stays open until it ends, so what
                        // it says, its summary last, reaches the chat (a client shows messages only during a turn)
                        t.reply.await.when(t.label.isDefined).timed.map: (waited, _) =>
                          routedResponse(TurnMetrics(model, "steered", elapsed.toMillis + waited.toMillis, 0, jevUsage(d)), s"steer:${t.id}")
                    // the task ended in the meantime: the steering is a follow-up of what it did
                    else t.reply.await.flatMap(reply => task(request, turn, session, harness, text, Some(followUp(t, reply)), s"after:${t.id}"))
                yield response
              case (Interjection.Route.Status, _) =>
                for
                  tree  <- statusText(session, all = false)
                  model <- session.selection.get.map(_.model)
                  reply <- harness.status(text, tree).catchAll(e => ZIO.succeed(s"$tree\n\n(couldn't summarize: ${message(e)})"))
                  _     <- client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(reply)))
                yield routedResponse(TurnMetrics(model, "status", elapsed.toMillis, 0, jevUsage(d)), "status")
              case (Interjection.Route.StopProcess(n), _) =>
                for
                  node    <- session.tree.snapshot.map(_.find(_.id.value == n))
                  stopped <- session.tree.stop(ProcessTree.Id(n), "stopped by the user")
                  model   <- session.selection.get.map(_.model)
                  note     = node.fold(s"There's no process $n.")(p =>
                               if stopped then s"Stopped [${p.id}] ${p.kind.toString.toLowerCase} ${p.name}; its task goes on."
                               else s"[${p.id}] ${p.name} had already ended.")
                  _       <- client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(note)))
                yield routedResponse(TurnMetrics(model, "stopped", elapsed.toMillis, 0, jevUsage(d)), s"stop-process:$n")
              case (Interjection.Route.Stop(_), Some(t)) =>
                for
                  stopped  <- t.stop
                  model    <- session.selection.get.map(_.model)
                  note      = if stopped then s"Stopped: \"${short(t.request)}\"." else s"\"${short(t.request)}\" had already finished."
                  _        <- client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(note)))
                yield routedResponse(TurnMetrics(model, "stopped", elapsed.toMillis, 0, jevUsage(d)), s"stop:${t.id}")
              case (Interjection.Route.After(_), Some(t)) =>
                ZIO.logInfo(s"interjection: waiting for task ${t.id} to end") *>
                  t.reply.await.raceEither(turn.awaitCancel).flatMap:
                    case Left(reply) => task(request, turn, session, harness, text, Some(followUp(t, reply)), s"after:${t.id}")
                    case Right(_)    => ZIO.succeed(PromptResponse(StopReason.Cancelled))
              case _ => task(request, turn, session, harness, text, None, "new"))
    yield response

  /** A task of the session: the prompt worked on by the harness, registered as running while it runs. */
  private def task(request: PromptRequest, turn: PromptTurn, session: Session, harness: Harness, text: String,
      earlier: Option[String], route: String = "", forbidden: Set[String] = Set.empty): IO[RpcError, PromptResponse] =
    // soness's transcript tools, and `watch_log`, which starts a watch of a log in this session (not in a task a watch
    // started: it acts on what it's shown, and doesn't start watches of its own)
    val transcripts = TranscriptTools.toolset(session.cwd,
      Option.when(route != "watch")((file, instructions) => watch(request.sessionId, session, file, instructions)))
    // web search, when the session's writer gives one (Kiro's, today), and the tools this build's extensions offer the task
    val searching = harness.webSearch.fold(transcripts)(search => WebFetch.combine(transcripts, WebSearch.toolset(search)))
    def extended(running: RunningTurn): Toolset =
      extensions.flatMap(_.tools(taskHandle(request.sessionId, session, running, route))).foldLeft(searching)(WebFetch.combine)
    ZIO.scoped:
      for
        model   <- session.selection.get.map(_.model)
        // how the harness works is the catalog's (`SONESS_MODE`, for tests and evals): users don't choose it
        mode     = catalog.defaultMode
        id      <- session.counter.updateAndGet(_ + 1)
        started <- Clock.instant
        running <- RunningTurn.make(request.sessionId.value, id, text, started)
        _       <- ZIO.acquireRelease(session.turns.update(_.updated(id, running)))(_ =>
                     session.turns.update(_ - id))
        _       <- ZIO.logDebug(s"prompt (task $id, model $model, mode ${mode.id}): $text")
        calls   <- Ref.make(0)
        toolset  = reporting(request.sessionId, session.guard.bind(guarded(WebFetch.combine(session.toolset, extended(running)), forbidden), text,
                     running.progress.get.map(_.toList.dropRight(1))), calls, session.callIds, running, session.log, id, session.cwd)
        _       <- ZIO.foreachDiscard(session.log)(log => EventLog.now.flatMap(at =>
                     log.append(EventLog.Event.TaskStarted(request.sessionId.value, id, at, text))))
        // what of the project's earlier work bears on this request: Jev recalls it from the event log (no clearing:
        // a new session's request is answered from what matters, whichever session did it)
        recalled <- recall(session, request.sessionId, harness, text, id)
        // `either` first: `raceEither` waits for the first *success*, so a failed run would wait for a cancel forever
        project <- ZIO.foreach(session.cwd)(ProjectContext.describe).map(_.flatten)
        // the skills the project's instructions say to follow are read before the work starts (read each turn: current)
        notes   <- ZIO.foreach(session.cwd)(ProjectContext.instructions).map(_.getOrElse(""))
        // a server's own instructions (how to use its tools) are base knowledge too, and may name a skill
        skills   = Skills.required(session.skills, (notes :: session.serverNotes.map(_._2)).mkString("\n\n"))
        context  = ProjectContext.withServers((earlier.toList ++ recalled.toList ++ project.toList).reduceOption(_ + "\n\n" + _),
                     // the coverage as it is for this task: web search is the writer's (Kiro's, Bedrock's), so it follows the model
                     session.serverNotes.map((n, t) => if n == "soness" && harness.webSearch.isDefined then
                       n -> t.replace("WebSearch: not provided", "WebSearch: web_search") else n -> t))
        // the task is a node of the session's process tree: its loops and tool calls are below it
        // the loop's name is the session's too: sessions in one project share its coordinator
        // the turn's plan, as ACP's `plan` session updates (every entry each time, as the protocol requires)
        planTo   = (entries: List[(String, ParallelLoop.Progress)]) =>
                     client.sessionUpdate(request.sessionId, SessionUpdate.Plan(entries.map((content, st) =>
                       PlanEntry(content, PlanEntryPriority.Medium, st match
                         case ParallelLoop.Progress.Pending    => PlanEntryStatus.Pending
                         case ParallelLoop.Progress.InProgress => PlanEntryStatus.InProgress
                         case ParallelLoop.Progress.Completed  => PlanEntryStatus.Completed)))).ignoreLogged
        // what the loop does between its tool calls (writing arguments, the reply, going on), as the client's thoughts
        thought  = (note: String) =>
                     client.sessionUpdate(request.sessionId, SessionUpdate.AgentThoughtChunk(ContentBlock.text(note + "\n"))).ignoreLogged
        // a session without web search is told so once (until its search changes): its requests can't search the web
        selected <- session.selection.get
        _       <- (thought(if !catalog.searches.exists(_.available) then s"Note: no web search provider is available, so this session can't search the web (it can still fetch pages). To have one, $NoSearchHelp."
                     else "Note: web search is off for this session (Web search: None), so it can't search the web (it can still fetch pages).") *>
                     session.searchNoted.set(true))
                     .when(catalog.composed && selected.search.isEmpty).unlessZIO(session.searchNoted.get)
        taskLoop = s"${request.sessionId.value.take(8)}/task-$id"
        // the task in the project's shared state: other sessions' loops see what it's for
        _       <- session.workspace.begin(taskLoop, text)
        outcome <- AgentLoop.notes.locally(Some(thought))(ClientTools.currentPlan.locally(planTo)(ParallelLoop.currentLoop.locally(Some(taskLoop)):
                     session.tree.child(Some(session.node), ProcessTree.Kind.Task, s"task-$id", Some(short(text)))(_ =>
                       harness.runInProject(text, toolset, skills, context, mode, running.steering.get, session.workspace.context(taskLoop))).timed.either
                       .raceEither(turn.awaitCancel.race(running.stopped.await))))
        _       <- running.reply.succeed(outcome match
                     case Left(Right((_, result))) => Some(result.answer)
                     case _                        => None)
        endedAt <- EventLog.now
        // a turn that failed partway (the decision model unreachable, say) still says what it did: to the user, and to
        // the event log, so a retry recalls it instead of starting over
        done    <- running.progress.get
        failed   = outcome.left.toOption.flatMap(_.left.toOption).map: error =>
                     s"This request stopped before it finished: ${message(error)}" +
                       (if done.isEmpty then ". No tool calls had been made."
                        else s"\n\nBefore it stopped, these ${done.size} tool calls had run:\n" + done.map(c => s"- $c").mkString("\n") +
                          "\n\nAsking again goes on from here: what they found is kept.")
        // the turn, as it went, for whoever watches the session's turns (the session's shared state, not its log)
        toolCalls <- calls.get
        now     <- Clock.instant
        turned  <- running.record.updateAndGet(_.copy(endedAt = Some(now), answer = outcome.left.toOption.flatMap(_.toOption).map(_._2.answer),
                     failure = outcome.left.toOption.flatMap(_.left.toOption).map(message),
                     metrics = outcome match
                       case Left(Right((elapsed, result))) => TurnMetrics(model, result.ending, elapsed.toMillis, toolCalls, result.usage)
                                                                .toJsonAST.toOption.flatMap(_.asObject)
                       case Right(_)                       => Some(Json.Obj("ending" -> Json.Str("cancelled")))
                       case Left(Left(_))                  => None,
                     jevCalls = outcome.left.toOption.flatMap(_.toOption).fold(0)(_._2.usage.jevCalls),
                     llmCalls = outcome.left.toOption.flatMap(_.toOption).fold(0)(_._2.usage.llmCalls),
                     project = session.cwd.map(_.toString)))
        _       <- session.ended.publish(turned)
        _       <- ZIO.foreachDiscard(session.log)(_.append(EventLog.Event.TaskEnded(request.sessionId.value, id, endedAt,
                     outcome match
                       case Left(Right((_, result))) => result.ending
                       case Left(Left(_))            => "failed"
                       case Right(_)                 => "cancelled",
                     outcome.left.toOption.flatMap(_.toOption).map(_._2.answer).orElse(failed))))
        response <- outcome match
          case Right(_) => ZIO.succeed(PromptResponse(StopReason.Cancelled))
          case Left(Left(error)) =>
            ZIO.logWarningCause(s"prompt turn of ${request.sessionId.value} failed", Cause.fail(error)) *>
              ZIO.foreachDiscard(failed)(text => client.sessionUpdate(request.sessionId,
                SessionUpdate.AgentMessageChunk(ContentBlock.text(text))).ignoreLogged) *>
              ZIO.fail(RpcError.internalError(message(error)))
          case Left(Right((elapsed, result))) =>
            for
              toolCalls <- calls.get
              metrics    = TurnMetrics(model, result.ending, elapsed.toMillis, toolCalls, result.usage)
                             .copy(route = Option.when(route.nonEmpty)(route))
              _         <- ZIO.logDebug(s"answer: ${result.answer}")
              _         <- ZIO.logDebug(s"turn metrics: ${metrics.toJson}")
              _         <- client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(result.answer)))
            yield PromptResponse(
              StopReason.EndTurn,
              // ACP's turn usage (Jev's and the LLM's tokens together); `_meta.soness` keeps them apart
              Some(TurnUsage(metrics.inputTokens + metrics.outputTokens, metrics.inputTokens, metrics.outputTokens)),
              Some(Json.Obj(TurnMetrics.MetaKey -> metrics.toJsonAST.toOption.get)),
            )
      yield response

  private def routedResponse(metrics: TurnMetrics, route: String): PromptResponse =
    val m = metrics.copy(route = Some(route))
    PromptResponse(StopReason.EndTurn, Some(TurnUsage(m.inputTokens + m.outputTokens, m.inputTokens, m.outputTokens)),
      Some(Json.Obj(TurnMetrics.MetaKey -> m.toJsonAST.toOption.get)))

  private def jevUsage(d: Interjection.Decision): ModelUsage =
    ModelUsage(jevCalls = 1, jevInputTokens = d.usage.inputTokens.toLong, jevOutputTokens = d.usage.outputTokens.toLong,
      jevLatencyMs = d.latencyMs)

  /**
   * The earlier tasks of the project (any session's, from its event log) that Jev judges bear on `text`, as a context
   * section; none when there are none, or Jev can't be asked.
   */
  private def recall(session: Session, sessionId: SessionId, harness: Harness, text: String, task: Int): UIO[Option[String]] =
    (for
      events  <- ZIO.foreach(session.log)(_.read).map(_.getOrElse(Nil))
      now     <- Clock.instant
      // this session's earlier tasks only (a resumed or loaded session's included): other sessions' work is theirs,
      // seen through the project's activity log (what they're doing, their edits and runs), not recalled as this one's
      earlier  = EventLog.tasks(events).filter(t => t.session == sessionId.value && t.task != task)
      chosen  <- if earlier.isEmpty then ZIO.succeed(Nil)
                 else harness.recall(text, earlier.map(t => com.jamesward.zio_decisions.orchestration.Recall.Earlier(
                   t.request, t.reply, t.ending, EventLog.minutesAgo(t.endedAt, now), true, t.files)))
      kept     = chosen.flatMap(earlier.lift)
      _       <- ZIO.logInfo(s"recalled ${kept.size} of ${earlier.size} earlier tasks").when(earlier.nonEmpty)
    yield Option.when(kept.nonEmpty):
      kept.map: t =>
        s"- ${EventLog.minutesAgo(t.endedAt, now)} minutes ago, the agent was asked:\n${t.request.take(2000)}\n" +
          s"  It ${t.ending}${t.reply.fold("")(r => s", replying:\n${r.take(3000)}")}" +
          (if t.files.isEmpty then "" else s"\n  Files it worked on: ${t.files.distinct.take(20).mkString(", ")}")
      .mkString("Earlier work in this session (from its event log, recalled as bearing on this request):\n", "\n", ""))
      .catchAllCause(c => ZIO.logWarningCause("recalling earlier work failed", c).as(None))

  /**
   * A managed watch of a soness debug log, a process of the session's tree (`/status` shows it, `/stop` ends it): each
   * turn that ends in the log (another session's) is judged against `instructions` and, when Jev judges it shows
   * something to act on, worked on as a task of this session, one at a time, with what the turn shows as its context.
   */
  private def watch(sessionId: SessionId, session: Session, file: java.nio.file.Path, instructions: String): Task[String] =
    for
      _       <- ZIO.fail(IllegalArgumentException(s"$file doesn't exist")).unlessZIO(ZIO.attemptBlocking(java.nio.file.Files.isRegularFile(file)))
      already <- session.tree.subtree(session.node).map(_.find(n => n.state == ProcessTree.State.Running && n.kind == ProcessTree.Kind.Task &&
                   n.detail.exists(_.startsWith(s"$file: "))))
      out     <- already.fold(start(sessionId, session, file, instructions))(n =>
                   ZIO.succeed(s"$file is already watched, as process [${n.id}]. This request is done: reply that the watch runs."))
    yield out

  private def start(sessionId: SessionId, session: Session, file: java.nio.file.Path, instructions: String): Task[String] =
    for
      // the log's size now: the watch reads on from here, whenever its fiber gets to start
      from    <- ZIO.attemptBlocking(java.nio.file.Files.size(file))
      started <- Promise.make[Nothing, ProcessTree.Id]
      _       <- ZIO.logAnnotate("session", sessionId.value):
                   session.tree.child(Some(session.node), ProcessTree.Kind.Task, s"watch ${file.getFileName}", Some(s"$file: ${short(instructions)}")): id =>
                     started.succeed(id) *> Watch.endedTurns(file, from = Some(from)).filter(_.session != sessionId.value)
                       .mapZIO(turn => react(sessionId, session, file, instructions, turn)
                         .catchAllCause(c => ZIO.logWarningCause(s"watch of $file: acting on turn ${turn.id} failed", c)))
                       .runDrain
                 .catchAllCause(c => ZIO.logInfoCause(s"watch of $file ended", c))
                 .forkIn(session.scope)
      id      <- started.await
      _       <- ZIO.logInfo(s"watching $file as process [$id]: $instructions")
    yield s"Started: $file is watched from now on, in the background, as process [$id]. Each turn that ends there will be " +
      s"judged against the instructions and, when it shows something to act on, worked on in this session, one at a time " +
      s"(this session's replies say so as it happens). /stop $id stops the watch. This request is done: reply that the " +
      "watch runs; nothing else is needed now."

  /** The session, as this build's extensions see it. */
  private def handle(sessionId: SessionId, session: Session): Extension.Session =
    new Extension.Session:
      val id: String = sessionId.value
      val cwd: Option[java.nio.file.Path] = session.cwd
      val tree: ProcessTree = session.tree
      val node: ProcessTree.Id = session.node
      val toolset: Toolset = session.toolset
      def harness: Task[Harness] =
        harnessOf(session.selection)
      def ended: ZIO[Scope, Nothing, Dequeue[Transcript.Turn]] = session.ended.subscribe
      def say(text: String): UIO[Unit] =
        client.sessionUpdate(sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(text + "\n\n"))).ignoreLogged
      def fork(zio: UIO[Any]): UIO[Unit] = zio.forkIn(session.scope).unit
      def toolsIn(dir: java.nio.file.Path): ZIO[Scope, Throwable, Extension.Tools] =
        for
          envVars     <- System.envs.orElseSucceed(Map.empty[String, String])
          declared    <- McpConfig.load(dir, envVars.get)
          _           <- ZIO.foreachDiscard(declared._2)(reason => ZIO.logWarning(s"$dir: skipping an MCP server: $reason"))
          scope       <- ZIO.scope
          connected   <- McpToolset.connect(declared._1).provideEnvironment(http.add[Scope](scope))
          _           <- ZIO.foreachDiscard(connected.failed)((_, reason) => ZIO.logWarning(s"$dir: skipping $reason"))
          dependencies = SourceDependencies.of(dir)
          resolution   = ToolResolver.resolved(connected.copy(toolset = LocalTools.reaching(dir, connected.toolset, dependencies)),
                           declared._1.map(_.name).toSet, List(
                             (LocalTools.toolset(dir, dependencies), ToolResolver.ToolSource.Local, "local"),
                             (WebFetch.toolset(http), ToolResolver.ToolSource.Builtin, "builtin")))
          withAll      = WebFetch.combine(resolution.toolset, TranscriptTools.toolset(Some(dir), None))
          projected   <- project(Some(dir))
        yield Extension.Tools(projected._2.bind(projected._1.bind(withAll)), connected.instructions)
      def contextOf(dir: java.nio.file.Path, instructions: List[(String, String)]): UIO[(List[AgentLoop.Document], Option[String])] =
        val home = sys.env.get("SONESS_USER_HOME").orElse(sys.props.get("user.home")).filter(_.nonEmpty).map(java.nio.file.Path.of(_))
        for
          skills    <- Skills.discover(dir :: home.toList)
          described <- ProjectContext.describe(dir)
          notes     <- ProjectContext.instructions(dir)
        yield (Skills.required(skills, (notes :: instructions.map(_._2)).mkString("\n\n")), ProjectContext.withServers(described, instructions))
      def background(request: String, label: String)(work: Extension.Background => Task[String]): Task[(Int, ProcessTree.Id)] =
        startBackground(sessionId, session, None, request, label, work)

  /** A task of the session, as this build's extensions see it. */
  private def taskHandle(sessionId: SessionId, of: Session, launcher: RunningTurn, how: String): Extension.Task =
    new Extension.Task:
      def session: Extension.Session = handle(sessionId, of)
      val route: String = how
      def background(request: String, label: String)(work: Extension.Background => Task[String]): Task[(Int, ProcessTree.Id)] =
        startBackground(sessionId, of, Some(launcher), request, label, work)

  /**
   * `work` as a task of the session that runs in the background, taking `launcher`'s place: registered as running (so
   * a message while it runs is routed to it as to any task: steering, a question, a stop), its launcher's steering so
   * far handed over and the launcher no longer counted as running (it only writes its reply), and a node of the
   * session's process tree, with what it runs below it. What it returns is said in the session when it ends.
   */
  private def startBackground(sessionId: SessionId, session: Session, launcher: Option[RunningTurn], request: String,
      label: String, work: Extension.Background => Task[String]): Task[(Int, ProcessTree.Id)] =
    val say = (text: String) => deliver(sessionId, session, s"**[$label]** $text")
    val say0 = say
    for
      id      <- session.counter.updateAndGet(_ + 1)
      started <- Clock.instant
      running <- RunningTurn.make(sessionId.value, id, request, started, Some(label))
      handed  <- ZIO.foreach(launcher)(_.steering.get).map(_.getOrElse(Nil))
      _       <- running.steering.set(handed)
      _       <- session.turns.update(t => launcher.fold(t)(l => t - l.id).updated(id, running))
      _       <- ZIO.foreachDiscard(session.log)(log => EventLog.now.flatMap(at =>
                   log.append(EventLog.Event.TaskStarted(sessionId.value, id, at, request))))
      calls   <- Ref.make(0)
      node    <- Promise.make[Nothing, ProcessTree.Id]
      ended    = (ending: String, reply: Option[String]) =>
                   running.reply.succeed(reply) *> session.turns.update(_ - id) *>
                     ZIO.foreachDiscard(session.log)(log => EventLog.now.flatMap(at =>
                       log.append(EventLog.Event.TaskEnded(sessionId.value, id, at, ending, reply))))
      // its own loop name and log annotation, not the launching task's (which the forked fiber would inherit)
      taskLoop = s"${sessionId.value.take(8)}/task-$id"
      _       <- ZIO.logAnnotate("session", sessionId.value):
                   ZIO.scoped(session.workspace.begin(taskLoop, request) *>
                   ParallelLoop.currentLoop.locally(Some(taskLoop))(ZIO.logAnnotate("loop", taskLoop)(
                   session.tree.child(Some(session.node), ProcessTree.Kind.Task, s"task-$id", Some(short(request))): nid =>
                     node.succeed(nid) *> work(new Extension.Background:
                       val id: Int = running.id
                       val node: ProcessTree.Id = nid
                       def say(text: String): UIO[Unit] = say0(text)
                       def steering: UIO[List[String]] = running.steering.get
                       def reported(toolset: Toolset): Toolset = reporting(sessionId, toolset, calls, session.callIds, running, session.log, running.id, session.cwd)
                       def loop[A](name: String)(work: Task[A]): Task[A] =
                         val full = s"$taskLoop/$name"
                         ParallelLoop.currentLoop.locally(Some(full))(ZIO.logAnnotate("loop", full)(work)))
                   .either.raceEither(running.stopped.await)
                   .flatMap:
                     case Left(Right(summary)) => say(summary) *> ended("answered", Some(summary))
                     case Left(Left(error))    => say(s"\"${short(request)}\" stopped: ${message(error)}") *> ended("failed", None)
                     case Right(_)             => say(s"\"${short(request)}\" stopped.") *> ended("cancelled", None))))
                   .forkIn(session.scope)
      nid     <- node.await
      _       <- ZIO.logInfo(s"background task $id, process [$nid]: $request")
    yield (id, nid)

  /**
   * `toolset` with its edits of files named in `forbidden` refused: a watch's task improves the agent, and the watched
   * turn's files (or this project's copies of them, its eval fixtures) are that turn's work, not this one's.
   */
  private def guarded(toolset: Toolset, forbidden: Set[String]): Toolset =
    if forbidden.isEmpty then toolset
    else new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        val annotations = toolset.tools.find(_.name == name).flatMap(_.annotations)
        val touched = ToolCallView.locations(arguments).map(l => java.nio.file.Paths.get(l.path).getFileName.toString).filter(forbidden)
        if ToolCallView.kind(name, annotations) != ToolKind.Edit || touched.isEmpty then toolset.call(name, arguments)
        else ZIO.fail(IllegalArgumentException(s"not edited: ${touched.mkString(", ")} is the watched turn's work (its project's " +
          "file, or a copy of it). This task improves the agent that did that turn (this project's harness code), not that work."))

  /** How likely, by Jev, a watched turn must be to show something to act on before it's worked on. */
  private val ActOn = 0.6

  /** A watched turn that ended: judged against the instructions, and worked on as a task when it shows something. */
  private def react(sessionId: SessionId, session: Session, file: java.nio.file.Path, instructions: String,
      turn: Transcript.Turn): Task[Unit] =
    val seen = Transcript.render(turn, 12000)
    val say = (text: String) => client.sessionUpdate(sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(text + "\n\n"))).ignoreLogged
    for
      harness <- harnessOf(session.selection)
      p       <- harness.judge(s"The instructions: $instructions\n\nA turn just ended in the watched log $file:\n$seen",
                   "Does this turn show something about the agent's own working to act on, as the instructions ask?",
                   "it shows a problem or an improvement the instructions are about: a failure, a wrong, missing or misleading " +
                     "answer, failed or repeated calls, a slow step, work going round in circles",
                   "it went as it should: answered, without failures, repeats or wasted time worth acting on")
                   .catchAllCause(c => ZIO.logWarningCause("judging a watched turn failed; acting on it", c).as(1.0))
      _       <- ZIO.logInfo(f"watch: turn ${turn.id} ended (${turn.ending}); act on it: p $p%.2f")
      _       <- if p < ActOn then say(s"Watched turn ${turn.id} ended (${turn.ending}): nothing to act on.")
                 else
                   val prompt =
                     s"""$instructions
                        |
                        |A turn of the soness transcript being watched ($file) just ended: turn ${turn.id} ("${Transcript.cut(turn.request, 200)}"),
                        |${turn.ending}. That turn was an agent (soness) working on its own user's request in its own project
                        |(${turn.project.getOrElse("another directory")}), not this one: don't redo that request's work, and don't edit
                        |anything of that project or this project's copies of it (eval fixtures). What matters is how the agent did it: what in its transcript shows the
                        |harness (this project) failing, wasting calls or time, going in circles or misleading the user. What it shows
                        |is in the context; transcript_turn, transcript_call and transcript_search (log $file, turn ${turn.id}) show
                        |it in full. Act on it as the instructions say: find the cause in this project's code, make the change, add or
                        |update a test that shows it, and run the relevant tests. When there's nothing in this project's code to
                        |change (the cause is the model, or the turn went well), say so and change nothing.""".stripMargin
                   say(s"Watched turn ${turn.id} ended (${turn.ending}); acting on it: ${Transcript.diagnose(turn).headOption.getOrElse("")}") *>
                     Promise.make[Nothing, Unit].flatMap: never =>
                       task(PromptRequest(sessionId, List(ContentBlock.text(prompt))), PromptTurn(never), session, harness, prompt,
                         Some(s"What the watched turn shows (from $file):\n$seen"), "watch",
                         forbidden = turn.calls.flatMap(c => scala.util.Try(c.arguments.fromJson[Json.Obj].toOption).toOption.flatten.toList)
                           .flatMap(ToolCallView.locations).map(l => java.nio.file.Paths.get(l.path).getFileName.toString).toSet).unit
                         .catchAll(e => ZIO.logWarning(s"watch: the task for turn ${turn.id} failed: ${e.message}"))
    yield ()

  /** A tool result as the event log keeps it. */
  private def MaxLoggedResult(json: Json): String = json match
    case Json.Str(text) => text
    case other          => other.toJson

  /** The context of a follow-up: the task it follows, and its reply. */
  private def followUp(t: RunningTurn, reply: Option[String]): String =
    s"Just before this request, in this session, the agent was asked:\n${t.request}\n" +
      reply.fold("That task didn't finish (it failed or was cancelled).")(r => s"It replied:\n$r")

  private def short(text: String): String = if text.length <= 80 then text else text.take(77) + "…"

  private def find(id: SessionId): IO[RpcError, Session] =
    sessions.get.map(_.get(id)).someOrFail(RpcError.invalidParams(s"unknown session: ${id.value}"))

  private def modelOf(id: String): IO[RpcError, String] =
    if catalog.models.contains(id) then ZIO.succeed(id) else ZIO.fail(RpcError.invalidParams(s"unknown model: $id"))

  /**
   * The session's choices as config options, each a drill-down by provider: its writer (`model`: Bedrock's models,
   * the ACP agents; category `_llm_model`, not the editors' top-level `model` picker), and, in a catalog of the real
   * providers, its decision model (Jev, OpenAI's) and its web search (Kiro's, Bedrock's, or none). How the harness
   * works on a request isn't the user's to choose (`SONESS_MODE` sets it for tests and evals).
   */
  private def configOptions(selection: HarnessCatalog.Selection): List[SessionConfigOption] =
    // ACP has no disabled option: an unavailable one is named so, says why, and is marked in `_meta` (choosing it fails)
    def option(c: HarnessCatalog.Choice) = SessionConfigSelectOption(SessionConfigValueId(c.id), c.name,
      Some(c.unavailable.fold(c.description)(why => s"Unavailable: $why. ${c.description}")),
      meta = c.unavailable.map(why => Json.Obj("soness" -> Json.Obj("available" -> Json.Bool(false), "reason" -> Json.Str(why)))))
    def grouped(choices: List[HarnessCatalog.Choice]) =
      SessionConfigSelectOptions.Grouped(choices.groupBy(_.group).toList.sortBy((g, _) => choices.indexWhere(_.group == g))
        .map((g, cs) => SessionConfigSelectGroup(SessionConfigGroupId(g.toLowerCase), g, cs.map(option))))
    val writers = catalog.models.map: id =>
      if id.startsWith(Provider.AcpPrefix) then
        val agent = id.stripPrefix(Provider.AcpPrefix)
        HarnessCatalog.Choice(id, modelName(id), s"$agent writes the arguments and the replies (an ACP agent)", agent.capitalize)
      else HarnessCatalog.Choice(id, modelName(id), s"Bedrock model $id writes the arguments and the replies", "Bedrock")
    val model = SessionConfigOption.Select(SessionConfigId(ModelOption), "LLM provider", SessionConfigValueId(selection.model),
      grouped(writers ++ catalog.unavailableModels), description = Some("The model that writes the tool calls' arguments and the replies"),
      // a category of its own, like the decision model's and the search's, so a client shows it as a drill-down
      // beside them rather than as its top-level model picker (where IntelliJ IDEA lists its groups as the models)
      category = Some(SessionConfigOptionCategory.Other("_llm_model")))
    if !catalog.composed then List(model)
    else
      val decision = SessionConfigOption.Select(SessionConfigId(DecisionOption), "Decision model",
        SessionConfigValueId(selection.decision.getOrElse(catalog.decisions.head.id)), grouped(catalog.decisions),
        description = Some("The decision model that orchestrates: each step, what to call next, and when to stop"),
        // the editors' model picker (a client wants one option of the `model` category): the decision models
        category = Some(SessionConfigOptionCategory.Model))
      val none = HarnessCatalog.Choice(NoSearch, "None",
        if !catalog.searches.exists(_.available) then s"No web search provider is available: ${NoSearchHelp}" else "No web search", "None")
      val search = SessionConfigOption.Select(SessionConfigId(SearchOption), "Web search",
        SessionConfigValueId(selection.search.getOrElse(NoSearch)), grouped(catalog.searches :+ none),
        description = Some("Which provider's web search the session's web_search tool uses"),
        category = Some(SessionConfigOptionCategory.Other("_web_search")))
      List(decision, model, search)

  /** The session's tools, reporting each call to the client as a tool call update. */
  /** A path a tool was given, as a file: resolved in the project (a `file://` URL as its path). */
  private def resolveIn(cwd: Option[java.nio.file.Path], path: String): Option[java.nio.file.Path] =
    cwd.flatMap(root => scala.util.Try(root.resolve(path.stripPrefix("file://")).normalize).toOption)

  /** A file's text, or none when it isn't one (yet) or is too large to show as a diff. */
  private def fileText(file: java.nio.file.Path): UIO[Option[String]] =
    ZIO.attemptBlocking(Option.when(java.nio.file.Files.isRegularFile(file) && java.nio.file.Files.size(file) < 2_000_000)(
      java.nio.file.Files.readString(file))).orElseSucceed(None)

  private def reporting(sessionId: SessionId, toolset: Toolset, calls: Ref[Int], ids: Ref[Int], running: RunningTurn,
      log: Option[EventLog.Log] = None, task: Int = 0, cwd: Option[java.nio.file.Path] = None): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        for
          _      <- calls.update(_ + 1)
          // ids are the session's: its tasks may run at the same time
          n      <- ids.updateAndGet(_ + 1)
          id      = ToolCallId(s"call-$n")
          _      <- running.progress.update(_ :+ s"$name ${short(arguments.toJson)}")
          begun  <- Clock.instant
          _      <- ZIO.logDebug(s"tool call ${id.value} $name: ${arguments.toJson}")
          // the loop it's for (a task's, a parallel part's) in `_meta.soness.loop`
          loop   <- ParallelLoop.currentLoop.get
          annotations = toolset.tools.find(_.name == name).flatMap(_.annotations)
          // a client that writes the kind's verb itself (IntelliJ IDEA's "Edited", "Fetched") gets the title without one
          verbs  <- clientNamesKinds.get
          // a background task's calls are marked with its label, so they're told from the user's own task's
          title   = running.label.fold("")(l => s"[$l] ") +
                      (if verbs then ToolCallView.bareTitle(name, arguments, annotations) else ToolCallView.title(name, arguments, annotations))
          _      <- client.sessionUpdate(sessionId, SessionUpdate.ToolCallStarted(ToolCall(
                      id, title, Some(name), ToolCallView.kind(name, annotations),
                      ToolCallStatus.InProgress, content = ToolCallView.diffs(name, arguments),
                      locations = ToolCallView.absolute(ToolCallView.locations(arguments), cwd),
                      rawInput = Some(ToolCallView.shownInput(name, arguments, annotations)),
                      meta = loop.map(l => Json.Obj(TurnMetrics.MetaKey -> Json.Obj("loop" -> Json.Str(l)))),
                    ))).ignoreLogged
          // a call stopped (the turn cancelled, the call stopped as hung) ends as failed, so the client shows no call
          // still running: ACP v1's statuses have no "cancelled"
          // an edit's files as they are now, to show what it changed when it ends
          edits   = ToolCallView.kind(name, annotations) == ToolKind.Edit
          files   = if edits then ToolCallView.locations(arguments).map(_.path).flatMap(p => resolveIn(cwd, p).map(p -> _)) else Nil
          before <- ZIO.foreach(files)((p, f) => fileText(f).map(p -> _))
          result <- ClientTools.currentCall.locally(Some(id))(toolset.call(name, arguments)).timed.onInterrupt(
                      client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(
                        id, status = Some(ToolCallStatus.Failed), rawOutput = Some(Json.Str("cancelled")),
                      ))).ignoreLogged,
                    ).tapBoth(
                      error => ZIO.logDebug(s"tool call ${id.value} $name failed: ${message(error)}") *> client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(
                        id, status = Some(ToolCallStatus.Failed), rawOutput = Some(Json.Str(message(error))),
                      ))).ignoreLogged,
                      (elapsed, json) =>
                        ZIO.logDebug(s"tool call ${id.value} $name returned in ${elapsed.toMillis}ms: ${json.toJson}") *>
                          // an edit's result is what it changed, file by file, with the tool's own words after
                          ZIO.foreach(before)((p, old) => resolveIn(cwd, p).fold(ZIO.none)(fileText).map(now => (p, old, now))).flatMap: after =>
                            val (diffs, summary) = ToolCallView.changes(after.map((p, b, a) =>
                              (resolveIn(cwd, p).fold(p)(_.toString), b, a)))
                            val shown = if !edits then json else Json.Obj("changed" -> Json.Str(summary),
                              "result" -> Json.Str(SharedTools.textOf(json)))
                            ZIO.logDebug(s"tool call ${id.value} $name changed: $summary").when(edits) *>
                              client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(
                                id, status = Some(ToolCallStatus.Completed), rawOutput = Some(shown),
                                content = Option.when(diffs.nonEmpty)(diffs),
                              ))).ignoreLogged,
                    ).either
          // the call, in the turn's record
          finish <- Clock.instant
          _      <- running.record.update(t => Transcript.withCall(t, Transcript.Call(id.value, name, loop, arguments.toJson, begun,
                      result.toOption.map(_._1.toMillis), Some(result.fold(e => Left(message(e)), r => Right(MaxLoggedResult(r._2))))), finish))
          // the call, in the project's event log
          at     <- EventLog.now
          _      <- ZIO.foreachDiscard(log)(_.append(EventLog.Event.ToolCalled(sessionId.value, task, at, id.value, name,
                      ToolCallView.title(name, arguments, annotations), EventLog.kindName(ToolCallView.kind(name, annotations)),
                      EventLog.cut(arguments.toJson), result.isRight,
                      EventLog.cut(result.fold(message, r => MaxLoggedResult(r._2))),
                      ToolCallView.locations(arguments).map(_.path))))
          done   <- ZIO.fromEither(result)
        yield done._2

object SonessAgent:
  /**
   * A model's display name, which differs from its id: IntelliJ IDEA rejects a model option whose name is its value.
   * `us.anthropic.claude-sonnet-4-5-20250929-v1:0` is `claude-sonnet-4-5-20250929 (anthropic)`, `openai.gpt-oss-120b`
   * is `gpt-oss-120b (openai)`; an id without a provider is `<id> (Bedrock)`.
   */
  def modelName(id: String): String =
    if id.startsWith(Provider.AcpPrefix) then s"${id.stripPrefix(Provider.AcpPrefix)} (ACP)" else bedrockName(id)

  private def bedrockName(id: String): String =
    val unversioned = id.replaceFirst("-v\\d+(:\\d+)?$", "")
    unversioned.split('.').toList.filterNot(Set("us", "eu", "apac", "global")) match
      case provider :: model :: Nil => s"$model ($provider)"
      case _                        => s"$unversioned (Bedrock)"

  /** The config option ids of the session's writer (a Bedrock model, an ACP agent), decision model and web search. */
  val ModelOption = "model"
  val DecisionOption = "decision_model"
  val SearchOption = "web_search"

  /** The web search option's value for none. */
  val NoSearch = "none"

  /** What gives a web search, for a session that has none. */
  val NoSearchHelp =
    "set BRAVE_API_KEY, install Kiro (kiro-cli, signed in or KIRO_API_KEY), or use a Bedrock OpenAI GPT-5.4+ model on Mantle (SONESS_BEDROCK_API=mantle)"

  /** IntelliJ IDEA's steering extension: the method a message sent into a running turn comes by, and its capability. */
  val SteeringMethod = "_session/steering"

  /** How long a message sent into the running turn waits for its route before it steers the newest task. */
  val SteeringDeadline: Duration = 3.seconds
  val SteeringMeta = "steering"

  private final case class Session(
    toolset: Toolset,
    skills: List[AgentLoop.Document],
    /** What the session's turns run with: its decision model, writer and web search. */
    selection: Ref[HarnessCatalog.Selection],
    /** Whether the session was told it has no web search (once, until its search changes). */
    searchNoted: Ref[Boolean],
    /** How many of the session's prompt turns are open: a client shows a message only during one. */
    openTurns: Ref[Int],
    /** What background tasks said while no prompt turn was open, said when the next one starts. */
    pending: Ref[Vector[String]],
    scope: Scope.Closeable,
    cwd: Option[java.nio.file.Path],
    turns: Ref[Map[Int, RunningTurn]],
    counter: Ref[Int],
    callIds: Ref[Int],
    tree: ProcessTree,
    ended: Hub[Transcript.Turn],
    serverNotes: List[(String, String)],
    log: Option[EventLog.Log],
    /** The session's node in the agent's process tree: its tasks are below it. */
    node: ProcessTree.Id,
    /** The project's shared state, which the session's tasks read and add to. */
    workspace: Workspace,
    /** What allows, asks about or refuses the session's tasks' calls. */
    guard: Guard.Session,
  )

  /** The session's running processes below its tasks, each with the task it's in, for Jev to route a message about one. */
  private def processes(session: Session): UIO[List[Interjection.Process]] =
    for
      now <- Clock.instant
      all <- session.tree.subtree(session.node)
    yield
      val byId = all.map(n => n.id -> n).toMap
      def task(n: ProcessTree.Node): Option[Int] =
        n.parent.flatMap(byId.get) match
          case Some(p) if p.kind == ProcessTree.Kind.Task => p.name.stripPrefix("task-").toIntOption
          case Some(p)                                     => task(p)
          case None                                        => None
      all.filter(n => n.state == ProcessTree.State.Running && n.kind != ProcessTree.Kind.Task && n.kind != ProcessTree.Kind.Session).toList.map: n =>
        Interjection.Process(n.id.value, n.kind.toString.toLowerCase, n.name, task(n), n.runningMs(now) / 1000, n.idleMs(now) / 1000, n.detail)

  /**
   * A task the session is working on: its request, when it started, its calls so far (in short), the steering it got,
   * and its reply when it ends (none when it failed or was cancelled).
   */
  private final case class RunningTurn(
    id: Int,
    request: String,
    started: java.time.Instant,
    progress: Ref[Vector[String]],
    steering: Ref[List[String]],
    reply: Promise[Nothing, Option[String]],
    stopped: Promise[Nothing, Unit],
    record: Ref[Transcript.Turn],
    /** A background task's label (`self-improvement`): its messages and tool calls are marked with it. */
    label: Option[String] = None,
  ):
    /** Stops the task (its prompt answers `cancelled`); false when it had already ended. */
    def stop: UIO[Boolean] = reply.isDone.flatMap(done => if done then ZIO.succeed(false) else stopped.succeed(()))

    def view: UIO[Interjection.Running] =
      for
        now <- Clock.instant
        p   <- progress.get
        st  <- steering.get
      yield Interjection.Running(id, request, java.time.Duration.between(started, now).toMillis, p.toList, st)

    /** Adds `message` to the task's steering; false when the task has ended. */
    def steer(message: String): UIO[Boolean] =
      reply.isDone.flatMap(done => if done then ZIO.succeed(false) else steering.update(_ :+ message).as(true))

  private object RunningTurn:
    def make(session: String, id: Int, request: String, started: java.time.Instant, label: Option[String] = None): UIO[RunningTurn] =
      for
        progress <- Ref.make(Vector.empty[String])
        steering <- Ref.make(List.empty[String])
        reply    <- Promise.make[Nothing, Option[String]]
        stopped  <- Promise.make[Nothing, Unit]
        record   <- Ref.make(Transcript.Turn(session, id, request, started))
      yield RunningTurn(id, request, started, progress, steering, reply, stopped, record, label)

  /**
   * The session's workspace roots, where its skills are looked for: `cwd`, the `additionalDirectories` ACP defines, and
   * any `_meta.additionalRoots` (an array of absolute paths some clients send instead, IntelliJ IDEA among them).
   */
  private def roots(request: NewSessionRequest): List[java.nio.file.Path] =
    val extra = request.meta.flatMap(_.get("additionalRoots")).flatMap(_.asArray).toList.flatten.flatMap(_.asString)
    // the user's own skills (`~/.agents/skills`) last: a project's skill of the same name wins
    // `SONESS_USER_HOME` replaces it (tests, for a home without the developer's own skills)
    val home = sys.env.get("SONESS_USER_HOME").orElse(sys.props.get("user.home")).filter(_.nonEmpty).toList
    (request.cwd :: request.additionalDirectories ++ extra ++ home).flatMap(p => scala.util.Try(java.nio.file.Paths.get(p)).toOption)
      .filter(_.isAbsolute).distinct

  /** An agent whose sessions (their MCP connections and stdio servers) are closed by `session/close` or with `parent`. */
  /**
   * `peers`, when given, is where the user's soness processes publish their process trees ([[Peers]]): this one's is
   * published there, and `/status` shows the others'.
   */
  def make(client: ClientConnection, http: ZEnvironment[Client], catalog: HarnessCatalog, parent: Scope,
      extensions: List[Extension] = Nil, peers: Option[java.nio.file.Path] = None): UIO[Agent] =
    for
      sessions     <- Ref.make(Map.empty[SessionId, Session])
      projects     <- Ref.make(Map.empty[String, (SharedTools.Coordinator, Workspace)])
      capabilities <- Ref.make(ClientCapabilities())
      // one tree for every session of the agent: each session a node, its tasks below it
      tree         <- ProcessTree.make
      namesKinds   <- Ref.make(false)
      _            <- ZIO.foreachDiscard(peers)(dir => Peers.publish(tree, dir).provideEnvironment(ZEnvironment[Scope](parent)))
    yield new SonessAgent(client, http, catalog, parent, sessions, projects, capabilities, extensions, tree, peers, namesKinds)

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.getClass.getName)
