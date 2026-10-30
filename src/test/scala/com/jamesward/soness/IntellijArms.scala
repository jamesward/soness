package com.jamesward.soness

import com.jamesward.soness.IntellijSession.{Call, Ide, Turn}
import com.jamesward.zio_evals.{AgentRunResult, TranscriptEvent}
import com.jamesward.zio_evals.acp.AcpAgentLoop
import com.jamesward.zioacp.AgentCommand
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.Path

/**
 * The agents an IDE task is measured on, harness against harness: each an ACP agent driven by zio-evals'
 * [[AcpAgentLoop]] the way IntelliJ IDEA 2026.2.3 drives agents: its captured `initialize`, a session in the project
 * with the IDE's MCP server (the real IDE's bridge, or the mock) and IntelliJ's skill views in `_meta.additionalRoots`,
 * and the prompt with IntelliJ's `_meta`. Every arm gets the same session; what each does with it (its own file tools,
 * the IDE's) is the agent's.
 *
 *  - `soness`: this project's staged-equivalent `Main` (the test runtime classpath), its model `BEDROCK_MODEL_ID`;
 *  - `kiro`: `kiro-cli acp`, its model `SONESS_EVAL_KIRO_MODEL` (default: Kiro's own default);
 *  - `claude`: Claude Code through its ACP adapter (`npx @agentclientprotocol/claude-agent-acp`) on Bedrock
 *    (`CLAUDE_CODE_USE_BEDROCK`, with `AWS_BEARER_TOKEN_BEDROCK`), its model `SONESS_EVAL_CLAUDE_MODEL` (a Bedrock
 *    model id, default Claude Sonnet 4.5).
 *
 * Kiro and Claude run with the user's own configuration (Kiro's global agents and steering, Claude's settings), as they
 * would in the IDE. Permission requests are allowed.
 */
object IntellijArms:

  val Names: List[String] = List("soness", "kiro", "claude")

  /** The Claude ACP adapter, pinned. */
  val ClaudeAdapter = "@agentclientprotocol/claude-agent-acp@0.89.1"

  final case class Arm(name: String, loop: AcpAgentLoop, model: String)

  /** IntelliJ IDEA's captured `initialize`. */
  val initialize: Task[InitializeRequest] =
    AcpSubprocess.intellijCapture.flatMap: lines =>
      ZIO.fromEither(lines.head.fromJson[Json.Obj].flatMap(_.get("params").toRight("no params")).flatMap(_.as[InitializeRequest]))
        .mapError(e => IllegalStateException(s"IntelliJ's captured initialize: $e"))

  /** An arm by name; `env` is added to soness's environment (not the other agents', which take their own config). */
  def arm(name: String, env: Map[String, String] = Map.empty, timeout: Duration = 10.minutes): Task[Arm] =
    for
      init <- initialize
      arm  <- name match
                case "soness" =>
                  for
                    javaHome  <- AcpSubprocess.property("java.home")
                    classpath <- AcpSubprocess.property("soness.classpath")
                    model     <- System.env("BEDROCK_MODEL_ID").someOrFail(IllegalStateException("BEDROCK_MODEL_ID is not set"))
                    // `SONESS_EVAL_LOG`: a debug log of every run (each turn's lines carry `session=<id>`)
                    log       <- System.env("SONESS_EVAL_LOG")
                  yield Arm(name, AcpAgentLoop(AgentCommand(s"$javaHome/bin/java",
                    List("--enable-native-access=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow", "-cp", classpath,
                      "com.jamesward.soness.Main") ++ log.toList.map(_ => "--debug"),
                    // one model, as configured in IntelliJ
                    Map("SONESS_BEDROCK_MODELS" -> "") ++ log.map("SONESS_LOG_FILE" -> _) ++ env), init, timeout), model)
                case "kiro" =>
                  System.env("SONESS_EVAL_KIRO_MODEL").map(m => Arm(name, AcpAgentLoop(AgentCommand("kiro-cli", List("acp")), init, timeout), m.getOrElse("")))
                case "claude" =>
                  for
                    model <- System.env("SONESS_EVAL_CLAUDE_MODEL").map(_.getOrElse("us.anthropic.claude-sonnet-4-5-20250929-v1:0"))
                    region <- System.env("AWS_REGION").map(_.getOrElse("us-east-1"))
                  yield Arm(name, AcpAgentLoop(AgentCommand("npx", List("-y", ClaudeAdapter),
                    Map("CLAUDE_CODE_USE_BEDROCK" -> "1", "AWS_REGION" -> region, "ANTHROPIC_MODEL" -> model)), init, timeout), "")
                case other => ZIO.fail(IllegalArgumentException(s"unknown arm $other; arms: ${Names.mkString(", ")}"))
    yield arm

  /** The session IntelliJ IDEA opens for a chat over `project`. */
  def session(ide: Ide, project: Path): Task[AcpAgentLoop.Session] =
    for
      skillRoot <- System.env("SONESS_IJ_SKILL_ROOT").someOrElseZIO:
                     AcpSubprocess.property("user.dir").map(d => Path.of(d, "src/test/resources/intellij-skill-views/other").toString)
      server    <- ide.idea match
                     case Some(idea) => ZIO.succeed(McpServer.Stdio("jetbrains_air_ide", idea, List("stdioMcpServer"), List(
                         EnvVariable("IJ_MCP_SERVER_PROJECT_PATH", project.toString), EnvVariable("IJ_MCP_SERVER_PORT", ide.port.get))))
                     case None =>
                       for
                         javaHome  <- AcpSubprocess.property("java.home")
                         classpath <- AcpSubprocess.property("soness.testClasspath")
                       yield McpServer.Stdio("jetbrains_air_ide", s"$javaHome/bin/java",
                         List("-cp", classpath, "com.jamesward.soness.IntellijMcpFixture"),
                         List(EnvVariable("IJ_MCP_SERVER_PROJECT_PATH", project.toString)))
      roots      = Json.Obj("additionalRoots" -> Json.Arr(Json.Str(skillRoot)))
    yield AcpAgentLoop.Session(project, List(server), meta = Some(roots), promptMeta = Some(roots))

  /** One run of `prompt` by `arm`, as the eval's [[Turn]]: soness's own metrics, or the agent's usage and cost. */
  def turn(arm: Arm, prompt: String, ide: Ide, project: Path): Task[Turn] =
    session(ide, project).flatMap: session =>
      // an agent that failed to start (a throttled startup check, say) is started again, up to twice: that's not the turn's result
      arm.loop.runIn(prompt, arm.model, session)
        .retry(Schedule.recurs(2) && Schedule.spaced(10.seconds) && Schedule.recurWhile[Throwable](e =>
          Option(e.getMessage).exists(m => m.startsWith("ACP initialize failed") || m.startsWith("ACP session/new failed"))))
        .either.map:
        case Left(error)   => Turn(Nil, "", Some(Option(error.getMessage).getOrElse(error.toString).take(400)), None)
        case Right(result) => Turn(calls(result), result.answer, None, Some(metrics(arm, result)))

  private def calls(result: AgentRunResult): List[Call] =
    // with timings, from them (in the order calls started); else from the transcript
    if result.toolCallTimings.nonEmpty then
      val outputs = result.events.collect { case r: TranscriptEvent.ToolResult => r.text }
      result.toolCallTimings.zipWithIndex.map: (t, i) =>
        val loop = t.meta.flatMap(_.get(TurnMetrics.MetaKey)).flatMap(_.asObject).flatMap(_.get("loop")).flatMap(_.asString)
        Call(t.name, t.input, t.status, outputs.lift(i).getOrElse(""), t.startMs, t.durationMs, loop)
    else fromEvents(result)

  private def fromEvents(result: AgentRunResult): List[Call] =
    val results = result.events.collect { case r: TranscriptEvent.ToolResult => r }
    // each call's result is the next result after it
    result.events.foldLeft((List.empty[Call], results)):
      case ((out, pending), TranscriptEvent.ToolCall(name, input)) =>
        pending.headOption match
          case Some(r) => (out :+ Call(name, input, if r.isError then "failed" else "completed", r.text), pending.tail)
          case None    => (out :+ Call(name, input, "?", ""), pending)
      case (state, _) => state
    ._1

  private def metrics(arm: Arm, result: AgentRunResult): Json =
    TurnMetrics.fromMeta(result.meta).flatMap(_.toJsonAST.toOption).getOrElse:
      Json.Obj(
        "model" -> Json.Str(result.events.collectFirst { case TranscriptEvent.Note(n) => n.replaceFirst(".*model ([^,]+),.*", "$1") }.getOrElse("?")),
        "ending" -> Json.Str(if result.answer.trim.nonEmpty then "answered" else "unable"),
        "latencyMs" -> Json.Num(result.latencyMs), "toolCalls" -> Json.Num(result.toolCalls),
        "jevCalls" -> Json.Num(0), "jevInputTokens" -> Json.Num(0), "jevOutputTokens" -> Json.Num(0),
        "llmCalls" -> Json.Null, "llmInputTokens" -> Json.Num(result.inputTokens), "llmOutputTokens" -> Json.Num(result.outputTokens),
        "costUsd" -> Json.Num(result.costUsd),
      )
