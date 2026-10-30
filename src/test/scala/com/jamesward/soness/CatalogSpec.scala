package com.jamesward.soness

import com.jamesward.soness.FakeBedrock.Reply
import zio.*
import zio.http.{Client, Status}
import zio.test.*

/** `Soness.catalog` against a fake Bedrock that serves only some models: only models that answer are offered. */
object CatalogSpec extends ZIOSpecDefault:

  /** Serves `good-*` models and answers 404 for any other. */
  private val bedrock =
    FakeBedrock.serveUrl: body =>
      ZIO.succeed:
        if FakeBedrock.model(body).exists(_.startsWith("good-")) then Reply.Text("OK")
        else Reply.Failure(Status.NotFound, s"The model ${FakeBedrock.model(body).getOrElse("?")} does not exist")

  /** The Bedrock variables, pointing Mantle at the fake, and a Jev key (Jev isn't called while building the catalog). */
  private def configure(models: Map[String, String], jev: Boolean = true) =
    for
      url <- bedrock
      _   <- ZIO.foreachDiscard(Map(
               "SONESS_BEDROCK_API" -> "mantle",
               "SONESS_BEDROCK_ENDPOINT" -> url.encode,
               "AWS_BEARER_TOKEN_BEDROCK" -> "dev",
             ) ++ models ++ (if jev then Map("TYPESAFE_API_KEY" -> "dev") else Map.empty))((name, value) => TestSystem.putEnv(name, value))
    yield ()

  def spec = suite("Soness.catalog")(
    test("offers the configured models that answer, in order, and starts sessions with BEDROCK_MODEL_ID") {
      for
        _       <- configure(Map("BEDROCK_MODEL_ID" -> "good-b", "SONESS_BEDROCK_MODELS" -> "good-a, missing-x,good-b,good-c"))
        catalog <- Soness.catalog
      yield assertTrue(
        catalog.models == List("good-b", "good-a", "good-c"),
        catalog.defaultModel == "good-b",
        catalog.harness("good-c").isDefined,
        catalog.harness("missing-x").isEmpty,
      )
    },
    test("an unavailable BEDROCK_MODEL_ID gives way to the first available model") {
      for
        _       <- configure(Map("BEDROCK_MODEL_ID" -> "missing-x", "SONESS_BEDROCK_MODELS" -> "good-a"))
        catalog <- Soness.catalog
      yield assertTrue(catalog.models == List("good-a"), catalog.defaultModel == "good-a")
    },
    test("fails when no configured model answers") {
      for
        _     <- configure(Map("SONESS_BEDROCK_MODELS" -> "missing-x,missing-y"))
        error <- Soness.catalog.flip
      yield assertTrue(error == "no Bedrock model is available (checked missing-x, missing-y)")
    },
    test("fails without the decision model's (Jev's) key") {
      for
        _     <- configure(Map("BEDROCK_MODEL_ID" -> "good-a"), jev = false)
        error <- Soness.catalog.flip
      yield assertTrue(error == "the decision model (Jev) is not configured: set TYPESAFE_API_KEY (or OPENAI_API_KEY, for OpenAI's)")
    },
    test("fails without any model configured") {
      for
        _     <- configure(Map.empty)
        error <- Soness.catalog.flip
      yield assertTrue(error == "set BEDROCK_MODEL_ID or SONESS_BEDROCK_MODELS")
    },
    test("an OpenAI key and an ACP writer, no Bedrock: OpenAI decides, the ACP writer is the only model and the default") {
      for
        _       <- TestSystem.putEnv("OPENAI_API_KEY", "dev")
        _       <- TestSystem.putEnv("SONESS_ACP_AGENTS", "kiro=kiro-cli acp")
        catalog <- Soness.catalog
      yield assertTrue(catalog.models == List("acp:kiro"), catalog.defaultModel == "acp:kiro", catalog.harness("acp:kiro").isDefined)
    },
    test("no Bedrock and no ACP writer is still Bedrock's error") {
      for
        _     <- TestSystem.putEnv("OPENAI_API_KEY", "dev")
        _     <- TestSystem.putEnv("SONESS_ACP_AGENTS", "")
        error <- Soness.catalog.flip
      yield assertTrue(error == "Bedrock is not configured: set AWS_BEARER_TOKEN_BEDROCK")
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
