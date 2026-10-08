package com.jamesward.jevness

import com.jamesward.zio_bedrock.{Bedrock, Converse, Mantle}
import com.jamesward.zio_typesafe_ai.TypeSafeAI
import com.jamesward.zioacp.*
import zio.*
import zio.http.Client

/** Assembles the harnesses from the environment variables documented in the README. */
object Jevness:
  val version: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("0.0.0-dev")

  /** The harnesses whose services are configured, with the reason each other one is unavailable. */
  final case class Harnesses(available: Map[HarnessMode, Harness], unavailable: Map[HarnessMode, String])

  def harnesses: ZIO[Client & Scope, Nothing, Harnesses] =
    for
      bedrock <- bedrockLayer.build.either
      jev     <- TypeSafeAI.Client.live.build.either
    yield
      val llm = bedrock.map(env => LlmHarness(env))
      val jevHarness = for
        b <- bedrock
        j <- jev.left.map(_ => "TYPESAFE_API_KEY is not set")
      yield JevHarness(j, BedrockLlmTransport(b))
      val all = Map[HarnessMode, Either[String, Harness]](HarnessMode.Jev -> jevHarness, HarnessMode.Llm -> llm)
      Harnesses(
        all.collect { case (mode, Right(harness)) => mode -> harness },
        all.collect { case (mode, Left(reason)) => mode -> reason },
      )

  /** Converse (default) or Mantle, chosen with `JEVNESS_BEDROCK_API`; both read `AWS_BEARER_TOKEN_BEDROCK`, `BEDROCK_MODEL_ID` and `AWS_REGION`. */
  private def bedrockLayer: ZLayer[Client, String, Bedrock] =
    ZLayer.fromZIO(System.env("JEVNESS_BEDROCK_API").orDie).flatMap: api =>
      api.get.getOrElse("converse") match
        case "converse" => Converse.configured.mapError(e => s"Bedrock Converse is not configured: ${e.getMessage}")
        case "mantle"   => Mantle.configured.mapError(e => s"Bedrock Mantle is not configured: ${e.getMessage}")
        case other      => ZLayer.fail(s"JEVNESS_BEDROCK_API must be converse or mantle, not $other")

  /** The default session mode, `JEVNESS_MODE` (`jev` unless set), when it is available. */
  def defaultMode(harnesses: Harnesses): IO[String, HarnessMode] =
    for
      id   <- System.env("JEVNESS_MODE").orDie.map(_.getOrElse(HarnessMode.Jev.id))
      mode <- ZIO.fromOption(HarnessMode.fromId(id)).orElseFail(s"JEVNESS_MODE must be jev or llm, not $id")
      _    <- ZIO.fail(s"mode $id is unavailable: ${harnesses.unavailable.getOrElse(mode, "")}")
                .unless(harnesses.available.contains(mode))
    yield mode

/** The jevness ACP agent on stdin and stdout. */
object Main extends ZIOAppDefault:
  // stdout carries the protocol, so logs go to stderr
  override val bootstrap = AcpAgent.stderrLogging

  def run =
    val program =
      for
        http      <- ZIO.environment[Client]
        harnesses <- Jevness.harnesses
        _         <- ZIO.foreachDiscard(harnesses.unavailable)((mode, reason) => ZIO.logWarning(s"mode ${mode.id} is unavailable: $reason"))
        mode      <- Jevness.defaultMode(harnesses)
        _         <- AcpAgent.serveStdio()(client => JevnessAgent.make(client, http, harnesses.available, mode))
      yield ()
    program.provideSome[Scope](Client.default)
