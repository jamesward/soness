package com.jamesward.soness

import com.jamesward.zio_evals.*
import com.jamesward.zio_evals.acp.AcpAgentLoop
import com.jamesward.zioacp.{Agent, AcpAgent, AcpClient, ClientConnection, Transport}
import zio.*
import zio.json.*
import zio.json.ast.Json

/**
 * soness's agent as zio-evals' ACP `AgentLoop`, connected in memory, so every eval exercises the same protocol path an
 * editor uses. The eval "model id" is the session's Bedrock model, chosen with its `model` config option; ACP's turn
 * usage carries the tokens, and soness's own metrics (`_meta.soness`) count the iterations (Jev and LLM calls).
 */
object SonessAgentLoop:

  def apply(agent: ClientConnection => ZIO[Scope, Nothing, Agent]): AgentLoop =
    val acp = AcpAgentLoop.connecting: (_, client, _) =>
      for
        pipe   <- Transport.pipe
        parent <- ZIO.scope
        _      <- AcpAgent.serve(pipe._1)(c => agent(c).provideEnvironment(ZEnvironment(parent))).forkScoped
        conn   <- AcpClient.connect(pipe._2, client)
      yield conn
    new AgentLoop:
      def run(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy): Task[AgentRunResult] =
        acp.run(prompt, modelId, mcpServers, policy).flatMap: result =>
          ZIO.fromOption(metrics(result))
            .orElseFail(RuntimeException(s"prompt response has no ${TurnMetrics.MetaKey} metrics: ${result.meta}"))
            // the metrics also go in the transcript, where the evals' reports read them
            .map(m => result.copy(iterations = m.llmCalls + m.jevCalls, events = result.events :+ TranscriptEvent.Note(m.toJson)))

      def runStructured(prompt: String, modelId: String, mcpServers: List[McpServerConfig], policy: AgentPolicy, schema: Json): Task[String] =
        ZIO.fail(UnsupportedOperationException("soness answers in text; use JevJudge to grade it"))

  /** soness's metrics of a run, from its prompt response's `_meta`. */
  def metrics(result: AgentRunResult): Option[TurnMetrics] = TurnMetrics.fromMeta(result.meta)
