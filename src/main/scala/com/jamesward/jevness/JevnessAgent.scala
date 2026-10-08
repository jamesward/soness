package com.jamesward.jevness

import com.jamesward.zioacp.*
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json

/** What one prompt turn cost, returned to the client in the `jevness` entry of the prompt response's `_meta`. */
final case class TurnMetrics(
  mode: String,
  latencyMs: Long,
  toolCalls: Int,
  llmCalls: Int,
  llmInputTokens: Long,
  llmOutputTokens: Long,
  jevCalls: Int,
  jevInputTokens: Long,
  jevOutputTokens: Long,
) derives JsonCodec:
  def inputTokens: Long = llmInputTokens + jevInputTokens
  def outputTokens: Long = llmOutputTokens + jevOutputTokens

object TurnMetrics:
  val MetaKey = "jevness"

  def apply(mode: HarnessMode, latencyMs: Long, toolCalls: Int, usage: ModelUsage): TurnMetrics =
    TurnMetrics(mode.id, latencyMs, toolCalls, usage.llmCalls, usage.llmInputTokens, usage.llmOutputTokens,
      usage.jevCalls, usage.jevInputTokens, usage.jevOutputTokens)

  def fromMeta(meta: Option[Json.Obj]): Option[TurnMetrics] =
    meta.flatMap(_.get(MetaKey)).flatMap(_.as[TurnMetrics].toOption)

/**
 * The jevness ACP agent. Each session connects to the HTTP MCP servers the client passes to `session/new` and answers
 * prompts with the harness of the session's mode (`jev` or the `llm` baseline), reporting each tool call as a session
 * update and the turn's cost in the response `_meta`.
 */
final class JevnessAgent private (
  client: ClientConnection,
  http: ZEnvironment[Client],
  harnesses: Map[HarnessMode, Harness],
  defaultMode: HarnessMode,
  sessions: Ref[Map[SessionId, JevnessAgent.Session]],
) extends Agent:
  import JevnessAgent.*

  def initialize(request: InitializeRequest): IO[RpcError, InitializeResponse] =
    ZIO.succeed(InitializeResponse(
      ProtocolVersion.V1,
      AgentCapabilities(
        mcpCapabilities = McpCapabilities(http = true),
        sessionCapabilities = SessionCapabilities(close = Some(Marker())),
      ),
      agentInfo = Some(Implementation("jevness", Jevness.version)),
    ))

  def newSession(request: NewSessionRequest): IO[RpcError, NewSessionResponse] =
    for
      endpoints <- ZIO.foreach(request.mcpServers)(endpoint)
      scope     <- Scope.make
      toolset   <- McpToolset.connect(endpoints)
                     .provideEnvironment(http.add[Scope](scope))
                     .tapError(_ => scope.close(Exit.unit))
                     .mapError(e => RpcError.invalidParams(message(e)))
      id        <- Random.nextUUID.map(uuid => SessionId(uuid.toString))
      mode      <- Ref.make(defaultMode)
      _         <- sessions.update(_.updated(id, Session(toolset, mode, scope)))
    yield NewSessionResponse(id, modes = Some(modeState(defaultMode)))

  override def setSessionMode(request: SetSessionModeRequest): IO[RpcError, SetSessionModeResponse] =
    for
      session <- find(request.sessionId)
      mode    <- ZIO.fromOption(HarnessMode.fromId(request.modeId.value).filter(harnesses.contains))
                   .orElseFail(RpcError.invalidParams(s"unknown mode: ${request.modeId.value}"))
      _       <- session.mode.set(mode)
    yield SetSessionModeResponse()

  override def closeSession(request: CloseSessionRequest): IO[RpcError, CloseSessionResponse] =
    for
      session <- find(request.sessionId)
      _       <- sessions.update(_ - request.sessionId)
      _       <- session.scope.close(Exit.unit)
    yield CloseSessionResponse()

  def prompt(request: PromptRequest, turn: PromptTurn): IO[RpcError, PromptResponse] =
    val text = request.prompt.collect { case ContentBlock.Text(t, _, _) => t }.mkString("\n")
    for
      session <- find(request.sessionId)
      mode    <- session.mode.get
      harness <- ZIO.fromOption(harnesses.get(mode)).orElseFail(RpcError.internalError(s"mode ${mode.id} is unavailable"))
      calls   <- Ref.make(0)
      toolset  = reporting(request.sessionId, session.toolset, calls)
      outcome <- harness.run(text, toolset).timed.raceEither(turn.awaitCancel)
                   .mapError(e => RpcError.internalError(message(e)))
      response <- outcome match
        case Right(_) => ZIO.succeed(PromptResponse(StopReason.Cancelled))
        case Left((elapsed, result)) =>
          for
            toolCalls <- calls.get
            metrics    = TurnMetrics(mode, elapsed.toMillis, toolCalls, result.usage)
            _         <- client.sessionUpdate(request.sessionId, SessionUpdate.AgentMessageChunk(ContentBlock.text(result.answer)))
          yield PromptResponse(StopReason.EndTurn, meta = Some(Json.Obj(TurnMetrics.MetaKey -> metrics.toJsonAST.toOption.get)))
    yield response

  private def find(id: SessionId): IO[RpcError, Session] =
    sessions.get.map(_.get(id)).someOrFail(RpcError.invalidParams(s"unknown session: ${id.value}"))

  private def modeState(current: HarnessMode): SessionModeState =
    SessionModeState(
      SessionModeId(current.id),
      HarnessMode.values.toList.filter(harnesses.contains)
        .map(m => SessionMode(SessionModeId(m.id), m.title, Some(m.description))),
    )

  /** The session's tools, reporting each call to the client as a tool call update. */
  private def reporting(sessionId: SessionId, toolset: Toolset, calls: Ref[Int]): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        for
          n      <- calls.updateAndGet(_ + 1)
          id      = ToolCallId(s"call-$n")
          _      <- client.sessionUpdate(sessionId, SessionUpdate.ToolCallStarted(ToolCall(
                      id, name, Some(name), ToolKind.Fetch, ToolCallStatus.InProgress, rawInput = Some(arguments),
                    ))).ignoreLogged
          result <- toolset.call(name, arguments).tapBoth(
                      error => client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(
                        id, status = Some(ToolCallStatus.Failed), rawOutput = Some(Json.Str(message(error))),
                      ))).ignoreLogged,
                      json => client.sessionUpdate(sessionId, SessionUpdate.ToolCallUpdated(ToolCallUpdate(
                        id, status = Some(ToolCallStatus.Completed), rawOutput = Some(json),
                      ))).ignoreLogged,
                    )
        yield result

object JevnessAgent:
  private final case class Session(toolset: Toolset, mode: Ref[HarnessMode], scope: Scope.Closeable)

  def make(
    client: ClientConnection,
    http: ZEnvironment[Client],
    harnesses: Map[HarnessMode, Harness],
    defaultMode: HarnessMode,
  ): UIO[Agent] =
    Ref.make(Map.empty[SessionId, Session]).map(new JevnessAgent(client, http, harnesses, defaultMode, _))

  private def endpoint(server: McpServer): IO[RpcError, McpEndpoint] = server match
    case McpServer.Http(name, url, headers, _) => ZIO.succeed(McpEndpoint(name, url, headers.map(h => h.name -> h.value).toMap))
    case other => ZIO.fail(RpcError.invalidParams(s"only HTTP MCP servers are supported: $other"))

  private def message(error: Throwable): String = Option(error.getMessage).getOrElse(error.getClass.getName)
