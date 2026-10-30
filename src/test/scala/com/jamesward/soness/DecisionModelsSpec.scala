package com.jamesward.soness

import zio.*
import zio.http.Client
import zio.test.*

/** `SONESS_DECISION_MODEL` picks the decision model's provider (and model); its credentials come through a source. */
object DecisionModelsSpec extends ZIOSpecDefault:

  private def from(values: (String, String)*): DecisionModels.Credentials =
    name => ZIO.succeed(values.toMap.get(name))

  def spec = suite("decision models")(
    test("unset, a provider, a provider and its model; an unknown provider names the available ones") {
      assertTrue(
        DecisionModels.parse(None) == Right(DecisionModels.Default),
        DecisionModels.parse(Some("  ")) == Right(DecisionModels.Default),
        DecisionModels.parse(Some("jev")) == Right(DecisionModels.Selection("jev", None)),
        DecisionModels.parse(Some("jev:jev-2")) == Right(DecisionModels.Selection("jev", Some("jev-2"))),
        DecisionModels.parse(Some("other:m")).left.exists(e => e.contains("no decision model provider other") && e.contains("jev")),
      )
    },
    test("Jev (zio-decisions' typesafe provider) from the credential source: its key is required; the selection's model wins over TYPESAFE_DEFAULT_MODEL") {
      for
        missing <- ZIO.scoped(DecisionModels.connect(from())).flip
        named   <- ZIO.scoped(DecisionModels.connect(from("TYPESAFE_API_KEY" -> "k", "TYPESAFE_DEFAULT_MODEL" -> "jev-x",
                     DecisionModels.Variable -> "jev:jev-y")).map(_._2.get.name))
        default <- ZIO.scoped(DecisionModels.connect(from("TYPESAFE_API_KEY" -> "k", "TYPESAFE_DEFAULT_MODEL" -> "jev-x")).map(_._2.get.name))
        unknown <- ZIO.scoped(DecisionModels.connect(from("TYPESAFE_API_KEY" -> "k", DecisionModels.Variable -> "nope"))).flip
      yield assertTrue(missing.contains("TYPESAFE_API_KEY"), named == "typesafe:jev-y", default == "typesafe:jev-x",
        unknown.contains("no decision model provider nope"))
    },
    test("unset, the provider is the first whose key is set: Jev's, then OpenAI's; neither is Jev's missing key") {
      for
        openai  <- DecisionModels.select(from("OPENAI_API_KEY" -> "k"))
        both    <- DecisionModels.select(from("OPENAI_API_KEY" -> "k", "TYPESAFE_API_KEY" -> "k"))
        chosen  <- DecisionModels.select(from("OPENAI_API_KEY" -> "k", "TYPESAFE_API_KEY" -> "k", DecisionModels.Variable -> "openai"))
        none    <- DecisionModels.select(from())
        name    <- ZIO.scoped(DecisionModels.connect(from("OPENAI_API_KEY" -> "k")).map(_._2.get.name))
      yield assertTrue(openai.provider == "openai", both.provider == "jev", chosen.provider == "openai",
        none == DecisionModels.Default, name == "openai:gpt-6-luna")
    },
  ).provide(Client.default)
