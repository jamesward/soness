package com.jamesward.soness

import com.jamesward.zio_decisions.{DecisionModel, Decisions}
import com.jamesward.zio_decisions.openai
import com.jamesward.zio_decisions.typesafe.TypeSafe
import zio.*
import zio.http.{Client, URL}

/**
 * The decision models ("System 1" models) soness can orchestrate with: each a provider of zio-decisions'
 * `DecisionModel`, chosen with `SONESS_DECISION_MODEL` (`<provider>` or `<provider>:<model>`; default `jev`): Jev
 * (zio-decisions' `TypeSafe`) or OpenAI's Decisions API (its `OpenAI`). A provider reads its credentials through [[Credentials]], the environment for now, so another
 * source (a sign-in, a credential store) can take its place without the providers changing.
 */
object DecisionModels:

  val Variable = "SONESS_DECISION_MODEL"

  /** Where a provider's settings and credentials come from, by name: the environment, for now. */
  type Credentials = String => UIO[Option[String]]

  val environment: Credentials = name => System.env(name).orElseSucceed(None).map(_.map(_.trim).filter(_.nonEmpty))

  /** A chosen decision model: its provider, and the provider's model when one is named. */
  final case class Selection(provider: String, model: Option[String]) derives CanEqual:
    override def toString: String = provider + model.fold("")(":" + _)

  /** A provider: its id, what it is, and how it connects to a model (its own, or the named one). */
  final case class Provider(id: String, title: String, key: String, description: String,
      connect: (Option[String], Credentials) => ZIO[Client & Scope, String, ZEnvironment[DecisionModel]])

  val Default = Selection("jev", None)

  /** `SONESS_DECISION_MODEL`'s value as a selection: `jev`, `jev:jev-latest`; unset or empty is [[Default]]. */
  def parse(value: Option[String]): Either[String, Selection] =
    value.map(_.trim).filter(_.nonEmpty).fold(Right(Default)): v =>
      val (provider, model) = v.split(":", 2) match
        case Array(p, m) => (p.trim, Some(m.trim).filter(_.nonEmpty))
        case Array(p)    => (p.trim, None)
        case _           => (v, None)
      providers.find(_.id == provider).toRight(
        s"$Variable: no decision model provider $provider (available: ${providers.map(_.id).mkString(", ")})").map(_ => Selection(provider, model))

  /** Each provider's key: with `SONESS_DECISION_MODEL` unset, the first provider whose key is set is the one. */
  private def keys = providers.map(p => p.id -> p.key)

  /**
   * The selection: `SONESS_DECISION_MODEL`'s, or, unset, the first provider whose key the credentials have (Jev's
   * `TYPESAFE_API_KEY`, then OpenAI's `OPENAI_API_KEY`), else [[Default]] (whose missing key is then the error).
   */
  def select(credentials: Credentials): IO[String, Selection] =
    credentials(Variable).flatMap:
      case Some(v) if v.trim.nonEmpty => ZIO.fromEither(parse(Some(v)))
      case _ =>
        ZIO.foreach(keys)((provider, key) => credentials(key).map(_.map(_ => provider)))
          .map(_.flatten.headOption.fold(Default)(Selection(_, None)))

  /** The selected decision model, connected; fails with what's missing (a key, a provider). */
  def connect(credentials: Credentials = environment): ZIO[Client & Scope, String, (Selection, ZEnvironment[DecisionModel])] =
    for
      selection <- select(credentials)
      provider   = providers.find(_.id == selection.provider).get
      model     <- provider.connect(selection.model, credentials)
      _         <- ZIO.logInfo(s"decision model: ${model.get.name}")
    yield (selection, model)

  /** A connected decision model: its name (`typesafe:jev-latest`), its provider and the model. */
  final case class Connected(name: String, provider: Provider, model: ZEnvironment[DecisionModel])

  /**
   * Every decision model the credentials can reach, the selected one ([[connect]]: it fails as that does) first and
   * then each other provider whose key is set, with its default model; one that fails to connect is left out, logged.
   */
  def connectAll(credentials: Credentials = environment): ZIO[Client & Scope, String, List[Connected]] =
    for
      (selection, first) <- connect(credentials)
      others             <- ZIO.foreach(providers.filter(_.id != selection.provider)): p =>
                              credentials(p.key).flatMap:
                                case None => ZIO.none
                                case Some(_) => p.connect(None, credentials).map(m => Connected(m.get.name, p, m)).asSome
                                  .catchAll(e => ZIO.logWarning(s"decision model ${p.id} is unavailable: $e").as(None))
    yield Connected(first.get.name, providers.find(_.id == selection.provider).get, first) :: others.flatten

  /**
   * Jev: `TYPESAFE_API_KEY`; the model is the selection's (`jev:<model>`), else `TYPESAFE_DEFAULT_MODEL`, else
   * `jev-latest`; `SONESS_TYPESAFE_ENDPOINT` replaces the TypeSafe AI API's origin (a proxy, or the tests' fake).
   */
  val Jev: Provider = Provider("jev", "Jev", "TYPESAFE_API_KEY", "Jev, TypeSafe AI's decision model", (named, credentials) =>
    for
      key      <- credentials("TYPESAFE_API_KEY").someOrFail(
                    "the decision model (Jev) is not configured: set TYPESAFE_API_KEY (or OPENAI_API_KEY, for OpenAI's)")
      model    <- named.fold(credentials("TYPESAFE_DEFAULT_MODEL"))(ZIO.some(_))
                    .map(_.fold(TypeSafe.JevLatest)(Decisions.ModelId(_)))
      endpoint <- credentials("SONESS_TYPESAFE_ENDPOINT").flatMap: url =>
                    ZIO.foreach(url)(u => ZIO.fromEither(URL.decode(u)).orElseFail(s"SONESS_TYPESAFE_ENDPOINT is not a URL: $u"))
      layer     = TypeSafe.layer(TypeSafe.ApiKey(key), model, endpoint.getOrElse(TypeSafe.DefaultEndpoint))
      decisions <- (layer >>> DecisionModel.observed(Logging.jev)).build
    yield decisions)

  /**
   * OpenAI's Decisions API: `OPENAI_API_KEY`; the model is the selection's (`openai:<model>`), else `gpt-6-luna`;
   * `SONESS_OPENAI_ENDPOINT` replaces the OpenAI API's origin (a proxy, or a fake).
   */
  val OpenAI: Provider = Provider("openai", "OpenAI", "OPENAI_API_KEY", "OpenAI's Decisions API", (named, credentials) =>
    for
      key       <- credentials("OPENAI_API_KEY").someOrFail("the decision model (OpenAI) is not configured: set OPENAI_API_KEY")
      endpoint  <- credentials("SONESS_OPENAI_ENDPOINT").flatMap: url =>
                     ZIO.foreach(url)(u => ZIO.fromEither(URL.decode(u)).orElseFail(s"SONESS_OPENAI_ENDPOINT is not a URL: $u"))
      model      = named.fold(openai.OpenAI.DefaultModel)(Decisions.ModelId(_))
      layer      = openai.OpenAI.layer(openai.OpenAI.ApiKey(key), model, endpoint.getOrElse(openai.OpenAI.DefaultEndpoint))
      decisions <- (layer >>> DecisionModel.observed(Logging.jev)).build
    yield decisions)

  val providers: List[Provider] = List(Jev, OpenAI)
