package com.jamesward.jevness

import com.jamesward.zio_evals.*
import com.jamesward.zioacp.{Agent, AcpAgent, AcpClient, ClientConnection, Client as AcpClientHandler, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

/**
 * Runs eval prompts through an ACP agent over an in-memory transport, so every eval exercises the same protocol path
 * an editor uses. The eval "model id" is the ACP session mode (`jev` or `llm`); the Bedrock model is the agent's.
 */
final class AcpAgentLoop(agent: ClientConnection => UIO[Agent]) extends AgentLoop:

  def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
    ZIO.scoped:
      for
        updates    <- Ref.make(Chunk.empty[SessionUpdate])
        pipe       <- Transport.pipe
        _          <- AcpAgent.serve(pipe._1)(agent).forkScoped
        connection <- AcpClient.connect(pipe._2, AcpAgentLoop.Recorder(updates))
        _          <- connection.initialize(InitializeRequest(ProtocolVersion.V1, clientInfo = Some(Implementation("jevness-evals", Jevness.version)))).rpc
        session    <- connection.newSession(NewSessionRequest(java.lang.System.getProperty("user.dir"), mcpServers.map(AcpAgentLoop.server))).rpc
        _          <- connection.setSessionMode(SetSessionModeRequest(session.sessionId, SessionModeId(modelId))).rpc
        timed      <- connection.prompt(PromptRequest(session.sessionId, List(ContentBlock.text(prompt)))).rpc.timed
        (elapsed, response) = timed
        metrics    <- ZIO.fromOption(TurnMetrics.fromMeta(response.meta))
                        .orElseFail(RuntimeException(s"prompt response has no ${TurnMetrics.MetaKey} metrics: ${response.meta}"))
        events     <- updates.get.map(AcpAgentLoop.transcript)
      yield AgentRunResult(
        answer = events.collect { case TranscriptEvent.AgentMessage(text) => text }.mkString,
        iterations = metrics.llmCalls + metrics.jevCalls,
        toolCalls = metrics.toolCalls,
        inputTokens = metrics.inputTokens,
        outputTokens = metrics.outputTokens,
        latencyMs = elapsed.toMillis,
        events = events :+ TranscriptEvent.Note(metrics.toJson),
      )

  def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
    ZIO.fail(UnsupportedOperationException("jevness answers in text; use JevJudge to grade it"))

  extension [A](effect: IO[RpcError, A]) private def rpc: Task[A] = effect.mapError(e => RuntimeException(e.message))

object AcpAgentLoop:
  private final case class Recorder(updates: Ref[Chunk[SessionUpdate]]) extends AcpClientHandler:
    def sessionUpdate(notification: SessionNotification): UIO[Unit] = updates.update(_ :+ notification.update)
    def requestPermission(request: RequestPermissionRequest): IO[RpcError, RequestPermissionResponse] =
      ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))

  private def server(config: McpServerConfig): McpServer =
    McpServer.Http(config.name, config.url, config.headers.toList.map((name, value) => HttpHeader(name, value)))

  private def transcript(updates: Chunk[SessionUpdate]): List[TranscriptEvent] =
    val names = updates.collect { case SessionUpdate.ToolCallStarted(call) => call.toolCallId -> call.title }.toMap
    updates.toList.collect {
      case SessionUpdate.AgentMessageChunk(ContentBlock.Text(text, _, _), _, _) => TranscriptEvent.AgentMessage(text)
      case SessionUpdate.ToolCallStarted(call) => TranscriptEvent.ToolCall(call.title, call.rawInput.fold("{}")(_.toJson))
      case SessionUpdate.ToolCallUpdated(update) if update.status.exists(s => s == ToolCallStatus.Completed || s == ToolCallStatus.Failed) =>
        TranscriptEvent.ToolResult(
          names.getOrElse(update.toolCallId, update.toolCallId.value),
          update.rawOutput.fold("")(_.toJson).take(2000),
          update.status.contains(ToolCallStatus.Failed),
        )
    }
