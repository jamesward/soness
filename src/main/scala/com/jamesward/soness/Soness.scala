package com.jamesward.soness

import com.jamesward.zio_bedrock.{Bedrock, Converse, ConverseConfig, Mantle, MantleApi, MantleConfig}
import com.jamesward.zio_bedrock.BedrockMiddleware.@@
import com.jamesward.zio_bedrock.Bedrock.{ApiKey, InferenceConfig, Message, ModelId, Region, RequestConfig}
import com.jamesward.zio_decisions.Decisions
import com.jamesward.zio_decisions.orchestration.AgentLoop
import com.jamesward.zioacp.*
import zio.*
import zio.http.{Client, URL, ZClient}

/** Assembles the harness catalog from the environment variables documented in the README. */
object Soness:
  val version: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("0.0.0-dev")

  /** How long a model's availability check may take before the model counts as unavailable. */
  val probeTimeout: Duration = 30.seconds

  /** The ACP agent named Kiro (`SONESS_ACP_AGENTS`, or found on the PATH): its writer searches the web too. */
  val KiroAgent = "kiro"

  /** The Bedrock API, key, region and origin every model is reached with. */
  private[soness] final case class BedrockAccess(mantle: Boolean, apiKey: ApiKey, region: Region, endpoint: Option[URL])

  /**
   * The HTTP client for Bedrock, Jev and the MCP servers. zio-http's default keeps a fixed pool of 10 connections per
   * host and opens all 10 with the first request, so every Bedrock origin left 9 unused connections that its 50 s idle
   * timeout then closed (each logged as a `ReadTimeoutException` at debug level). This pool opens connections as they
   * are needed.
   */
  val httpClient: ZLayer[Any, Throwable, Client] =
    ZLayer.succeed(ZClient.Config.default.dynamicConnectionPool(0, 10, 30.seconds)) >>> Client.fromConfig

  /**
   * The agent loop's strategy, `SONESS_LOOP_STRATEGY`: a comma-separated list of `catalogInState`,
   * `relevanceFilter`, `boundCalls`, `parallelCalls`, `noRepeats`, `correct`, `giveUp`, `batch`, and one of `keepContext` / `rateContext` (Jev judges,
   * each decision, which earlier results stay in its and the LLM's context; `all` for every switch, `none` for the plain loop).
   * Unset is [[DefaultStrategy]], the one the IntelliJ task evals measured best. For measuring orchestration choices.
   */
  private val loopStrategy: IO[String, AgentLoop.Strategy] =
    env("SONESS_LOOP_STRATEGY").flatMap:
      case None => ZIO.succeed(DefaultStrategy)
      case Some(value) =>
        val names = value.split(',').map(_.trim).filter(_.nonEmpty).toList
        val all = names.contains("all")
        val known = Set("all", "none", "catalogInState", "relevanceFilter", "boundCalls", "parallelCalls", "noRepeats",
          "keepContext", "rateContext", "correct", "giveUp", "batch")
        ZIO.fail(s"SONESS_LOOP_STRATEGY: unknown ${names.filterNot(known).mkString(", ")}; known: ${known.mkString(", ")}")
          .when(names.exists(n => !known(n)))
          .as(AgentLoop.Strategy(
            catalogInState = all || names.contains("catalogInState"),
            relevanceFilter = all || names.contains("relevanceFilter"),
            boundCalls = all || names.contains("boundCalls"),
            parallelCalls = all || names.contains("parallelCalls"),
            noRepeats = all || names.contains("noRepeats"),
            correct = all || names.contains("correct"),
            giveUp = all || names.contains("giveUp"),
            batch = all || names.contains("batch"),
            context =
              if names.contains("rateContext") then AgentLoop.ContextChoice.Rate
              else if names.contains("keepContext") then AgentLoop.ContextChoice.Keep
              else AgentLoop.ContextChoice.All,
          ))

  /**
   * Ready-made calls from earlier results (no generative-model call to write their arguments) and no repeats of a
   * call that already succeeded: on the IntelliJ task evals, 9/9 with 39% fewer LLM calls, 30% fewer LLM tokens and
   * 26% less Jev input than the plain loop. The relevance filter halves Jev's input but cost bug fixes; parallel calls
   * added Jev tokens without saving steps.
   */
  /** Reads in a row (files, searches, pages) before the decision model is asked whether it's time to act. */
  val DefaultExploreLimit = 6

  /** Calls between the decision model's reviews of whether the loop's work is moving forward. */
  val DefaultReviewEvery = 12

  /** The tools that only explore: reading, searching, fetching, by their kind. */
  val explores: AgentLoop.Tool => Boolean = t =>
    Set(com.jamesward.zioacp.schema.ToolKind.Read, com.jamesward.zioacp.schema.ToolKind.Search, com.jamesward.zioacp.schema.ToolKind.Fetch)
      .contains(ToolCallView.kind(t.name, t.annotations))

  /**
   * Batches too (`batch`): the independent calls a loop would make one after another (reads, fetches, searches, edits
   * of different files; each a decision and a writing, seconds each with an ACP agent writing) written at once and run
   * at the same time.
   */
  val DefaultStrategy: AgentLoop.Strategy = AgentLoop.Strategy(boundCalls = true, noRepeats = true, correct = true, batch = true)

  /**
   * The decision model ([[DecisionModels]]: Jev with `TYPESAFE_API_KEY`, or OpenAI's with `OPENAI_API_KEY`), the Bedrock
   * models that answered a check, with the default `BEDROCK_MODEL_ID` selects, and the ACP writers (Kiro). Bedrock is
   * optional when an ACP writer is available and no Bedrock setting is given; sessions then start with the first ACP
   * writer. Each unavailable model is logged with the reason. Fails without a decision model or any writer.
   */
  def catalog: ZIO[Client & Scope, String, HarnessCatalog] = catalogEnding(Set.empty)

  /** The catalog, with `endsTurn` the tools (a build's extensions') whose successful call is the request's whole work. */
  def catalogEnding(endsTurn: Set[String]): ZIO[Client & Scope, String, HarnessCatalog] =
    for
      // the decision model (`SONESS_DECISION_MODEL`; Jev today), through the provider's credentials
      // every decision model a key is set for (the selected one first): the session chooses among them
      deciders  <- DecisionModels.connectAll()
      // the ACP agents that can write (Kiro, Claude Code, …): with one, Bedrock is optional
      agents    <- acpAgents
      bedrock   <- bedrockConfigured
      access    <- if bedrock || agents.isEmpty then bedrockAccess.asSome else ZIO.none
      ids       <- if access.isDefined then modelIds else ZIO.succeed(Nil)
      _         <- ZIO.logInfo(s"Bedrock isn't configured (no AWS_BEARER_TOKEN_BEDROCK, BEDROCK_MODEL_ID or " +
                     s"SONESS_BEDROCK_MODELS); writing with ${agents.map(_._1).mkString(", ")}").when(access.isEmpty)
      checked   <- ZIO.foreachPar(ids)(id => connect(access.get, id).flatMap(check).either.map(id -> _))
      available  = checked.collect { case (id, Right(bedrock)) => id -> bedrock }
      _         <- ZIO.foreachDiscard(checked.collect { case (id, Left(reason)) => id -> reason }): (id, reason) =>
                     ZIO.logWarning(s"Bedrock model $id is unavailable: $reason")
      _         <- ZIO.fail(s"no Bedrock model is available (checked ${ids.mkString(", ")})").when(available.isEmpty && agents.isEmpty)
      model     <- defaultModel(available.map(_._1)).map(_.getOrElse(s"${Provider.AcpPrefix}${agents.head._1}"))
      strategy  <- loopStrategy
      _         <- ZIO.logInfo(s"loop strategy: ${strategy.describe}")
      // `SONESS_EXPLORE_LIMIT`: reads in a row before the decision model is asked whether it's time to act (0: never)
      explore   <- env("SONESS_EXPLORE_LIMIT").flatMap(v => ZIO.foreach(v)(s => ZIO.fromOption(s.toIntOption.filter(_ >= 0))
                     .orElseFail(s"SONESS_EXPLORE_LIMIT must be a number, not $s")))
      review    <- env("SONESS_REVIEW_EVERY").flatMap(v => ZIO.foreach(v)(s => ZIO.fromOption(s.toIntOption.filter(_ >= 0))
                     .orElseFail(s"SONESS_REVIEW_EVERY must be a number, not $s")))
      // `SONESS_MAX_PARALLEL`: how many calls a batch may have and how many things run at once (a batch's calls, a plan's parts)
      parallel  <- env("SONESS_MAX_PARALLEL").flatMap(v => ZIO.foreach(v)(s => ZIO.fromOption(s.trim.toIntOption.filter(_ > 0))
                     .orElseFail(s"SONESS_MAX_PARALLEL must be a positive number, not $s")))
      maxSteps  <- env("SONESS_MAX_STEPS").flatMap(v => ZIO.foreach(v)(s => ZIO.fromOption(s.toIntOption.filter(_ > 0))
                     .orElseFail(s"SONESS_MAX_STEPS must be a positive number, not $s")))
      // Jev checks each reply: one that reads as the model's own planning is written again
      config     = AgentLoop.Config(instructions = Prompts.Jev, strategy = strategy, maxSteps = maxSteps.getOrElse(AgentLoop.Config().maxSteps),
                     checkReply = !sys.env.get("SONESS_CHECK_REPLY").exists(_.trim.equalsIgnoreCase("false")),
                     // Jev picks what a call's writer needs (earlier steps, documents, the project context); the writer
                     // may ask for the rest
                     briefArguments = !sys.env.get("SONESS_BRIEF_ARGUMENTS").exists(_.trim.equalsIgnoreCase("false")),
                     // starting a watch is the request's whole work: the watch goes on by itself
                     endsTurn = c => c.name == TranscriptTools.WatchLog.name || endsTurn(c.name),
                     // an answer whose reply says the work isn't done goes on
                     checkDone = true,
                     // a loop that only reads for a while is asked whether it's time to make the change
                     exploreLimit = explore.getOrElse(DefaultExploreLimit), explores = Soness.explores,
                     // a loop whose last calls haven't moved the work forward ends, saying what it found
                     reviewEvery = review.getOrElse(DefaultReviewEvery),
                     // an edit, a deletion, a move: what a check after it shows works or doesn't
                     changes = t => Set(com.jamesward.zioacp.schema.ToolKind.Edit, com.jamesward.zioacp.schema.ToolKind.Delete,
                       com.jamesward.zioacp.schema.ToolKind.Move).contains(ToolCallView.kind(t.name, t.annotations)),
                     maxParallel = parallel.getOrElse(com.jamesward.zio_decisions.orchestration.Work.DefaultParallelism))
      // the mode every session runs in (not shown to users): `SONESS_MODE` sets it for tests and evals
      mode      <- env("SONESS_MODE").flatMap(v => ZIO.foreach(v)(m => ZIO.fromOption(HarnessMode.of(m.trim))
                     .orElseFail(s"SONESS_MODE must be one of ${HarnessMode.values.map(_.id).mkString(", ")}, not $m")))
      // `SONESS_ESCALATION_MODEL`: the stronger model Jev can hand a stuck loop's writing to (one of the available ones)
      // one of the session models, or a model of its own (checked like them, not offered in the picker)
      escalate  <- env("SONESS_ESCALATION_MODEL").map(_.map(_.trim)).flatMap:
                     case None => ZIO.none
                     case Some(e) => available.find(_._1 == e) match
                       case Some(found) => ZIO.some(found)
                       case None if access.isEmpty =>
                         ZIO.logWarning(s"escalation model $e is unavailable: Bedrock isn't configured").as(None)
                       case None =>
                         connect(access.get, e).flatMap(check).either.flatMap:
                           case Right(b)     => ZIO.some(e -> b)
                           case Left(reason) => ZIO.logWarning(s"escalation model $e is unavailable: $reason").as(None)
      _         <- ZIO.foreachDiscard(escalate)(e => ZIO.logInfo(s"escalation model: ${e._1}"))
      // `SONESS_ARGUMENTS_EFFORT`: the reasoning effort a model that takes one (gpt-oss) writes arguments at
      effort    <- env("SONESS_ARGUMENTS_EFFORT").map(_.getOrElse("low").trim).flatMap: e =>
                     ZIO.fromOption(Option.when(Set("low", "medium", "high", "default")(e))(e))
                       .orElseFail(s"SONESS_ARGUMENTS_EFFORT must be low, medium, high or default, not $e")
      writer     = (id: String, b: ZEnvironment[Bedrock]) =>
                     BedrockLlmTransport(b, argumentsEffort = Option.when(id.contains("gpt-oss") && effort != "default")(effort))
      // the ACP provider's models: the ACP agents that can write (Kiro, Claude Code, …), started on first use
      scope     <- ZIO.scope
      _         <- ZIO.logInfo(s"ACP writers: ${agents.map(_._1).mkString(", ")}").when(agents.nonEmpty)
      acp       <- ZIO.foreach(agents)((name, command) => AcpWriter.make(name, command, scope, WritingPrompts()).map(name -> _))
      http      <- ZIO.service[Client]
      // Brave's web search (the Brave Search API), with BRAVE_API_KEY, whatever the writer
      brave     <- BraveSearch.fromCredentials(DecisionModels.environment, ZEnvironment(http))
      // the writers, by model: each Bedrock model (escalating to the escalation model, at its own effort), each ACP agent
      writers    = available.map((id, b) => id -> (writer(id, b): Writer,
                     config.copy(escalation = escalate.filter(_._1 != id).map(e => BedrockLlmTransport(e._2))))) ++
                   acp.map((name, w) => s"${Provider.AcpPrefix}$name" -> (w: Writer, config))
      // the web searches: Kiro's (a prompted turn with its own tools), Bedrock's (Mantle, GPT-5.4 or later, its Regions)
      bedrockWs  = access.flatMap(a => available.iterator.flatMap((id, _) => bedrockSearch(a, id, http).map(id -> _)).nextOption())
      searches   = acp.collectFirst { case (KiroAgent, w) => SearchKiro -> WebSearch.kiro(w.search) }.toList ++
                   bedrockWs.map((_, ws) => SearchBedrock -> ws).toList ++
                   brave.map(SearchBrave -> _).toList
      // a new session's search follows its writer's provider when that has one, else the first there is
      search     = (if model.startsWith(Provider.AcpPrefix) then searches.find(_._1 == SearchKiro) else searches.find(_._1 == SearchBedrock))
                     .orElse(searches.headOption).map(_._1)
      _         <- ZIO.logInfo(s"web search: ${searches.map(_._1).mkString(", ")}; sessions start with ${search.getOrElse("none")}")
                     .when(searches.nonEmpty)
      _         <- ZIO.logWarning(s"no web search provider: Kiro (kiro-cli), a Bedrock OpenAI GPT-5.4+ model on Mantle or ${BraveSearch.Variable} gives one")
                     .when(searches.isEmpty)
      harnesses  = (for
                     d       <- deciders
                     (m, wc) <- writers
                     ws      <- None :: searches.map(Some(_))
                   yield HarnessCatalog.Selection(Some(d.name), m, ws.map(_._1)) -> DecisionHarness(d.model, wc._1, wc._2, ws.map(_._2))).toMap
      catalog   <- ZIO.fromEither(HarnessCatalog.composed(writers.map(_._1),
                     // every provider is shown: those without their credentials (or tool) as unavailable, saying why
                     deciders.map(d => HarnessCatalog.Choice(d.name, d.model.get.defaultModel.unwrap, d.provider.description, d.provider.title)) ++
                       DecisionModels.providers.filterNot(p => deciders.exists(_.provider.id == p.id)).map(p =>
                         HarnessCatalog.Choice(p.id, s"${p.title} (unavailable)", p.description, p.title, Some(s"set ${p.key}"))),
                      List(SearchKiro, SearchBedrock, SearchBrave).map: id =>
                        val choice = searchChoice(id, bedrockWs.map(_._1))
                        if searches.exists(_._1 == id) then choice
                        else choice.copy(name = s"${choice.name} (unavailable)", unavailable = Some(
                          if id == SearchKiro then "install Kiro (kiro-cli, signed in or KIRO_API_KEY)"
                          else if id == SearchBrave then s"set ${BraveSearch.Variable}"
                          else "needs a Bedrock OpenAI GPT-5.4+ model on Mantle (SONESS_BEDROCK_API=mantle) in us-east-1, us-east-2 or us-west-2")),
                     HarnessCatalog.Selection(Some(deciders.head.name), model, search), mode.getOrElse(HarnessMode.Auto),
                     Option.when(available.isEmpty)(HarnessCatalog.Choice("bedrock", "Bedrock (unavailable)", "A Bedrock model writes",
                       "Bedrock", Some("set AWS_BEARER_TOKEN_BEDROCK and BEDROCK_MODEL_ID"))).toList ++
                     Option.when(!agents.exists(_._1 == KiroAgent))(HarnessCatalog.Choice(s"${Provider.AcpPrefix}$KiroAgent", "Kiro (unavailable)",
                       "Kiro writes (an ACP agent)", "Kiro", Some("install Kiro (kiro-cli, signed in or KIRO_API_KEY)"))).toList)(harnesses.get))
      _         <- ZIO.logInfo(s"default mode: ${catalog.defaultMode.id}")
    yield catalog

  /**
   * The models to offer, in order: `BEDROCK_MODEL_ID` (the default) and then `SONESS_BEDROCK_MODELS`, a comma-separated
   * list. At least one must be set.
   */
  private val modelIds: IO[String, List[String]] =
    for
      default <- env("BEDROCK_MODEL_ID")
      listed  <- env("SONESS_BEDROCK_MODELS").map(_.toList.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty)))
      ids      = (default.toList ++ listed).distinct
      _       <- ZIO.fail("set BEDROCK_MODEL_ID or SONESS_BEDROCK_MODELS").when(ids.isEmpty)
    yield ids

  /**
   * Converse (default) or Mantle, chosen with `SONESS_BEDROCK_API`, with `AWS_BEARER_TOKEN_BEDROCK` and `AWS_REGION`.
   * `SONESS_BEDROCK_ENDPOINT` replaces the regional origin (a proxy, or the tests' fake).
   */
  private[soness] val bedrockAccess: IO[String, BedrockAccess] =
    for
      api      <- env("SONESS_BEDROCK_API").map(_.getOrElse("converse"))
      mantle   <- api match
                    case "converse" => ZIO.succeed(false)
                    case "mantle"   => ZIO.succeed(true)
                    case other      => ZIO.fail(s"SONESS_BEDROCK_API must be converse or mantle, not $other")
      apiKey   <- env("AWS_BEARER_TOKEN_BEDROCK").someOrFail("Bedrock is not configured: set AWS_BEARER_TOKEN_BEDROCK")
      code     <- env("AWS_REGION").map(_.getOrElse(Region.UsEast1.code))
      region   <- ZIO.fromOption(Region.fromCode(code)).orElseFail(s"AWS_REGION is not a Bedrock region: $code")
      endpoint <- env("SONESS_BEDROCK_ENDPOINT").flatMap: url =>
                    ZIO.foreach(url)(u => ZIO.fromEither(URL.decode(u)).orElseFail(s"SONESS_BEDROCK_ENDPOINT is not a URL: $u"))
    yield BedrockAccess(mantle, ApiKey(apiKey), region, endpoint)

  private[soness] def connect(access: BedrockAccess, id: String): ZIO[Client & Scope, String, ZEnvironment[Bedrock]] =
    val modelId = ModelId(id)
    val layer =
      if access.mantle then Mantle.layer(MantleConfig(access.apiKey, access.region, modelId, access.endpoint.orNull, mantleApi(id)))
      else Converse.layer(ConverseConfig(access.apiKey, access.region, modelId, access.endpoint.orNull))
    (layer @@ Logging.bedrock(id)).build

  /** Bedrock's web search with `id`, when it has access to it (TOOLS_SPEC.md, "Web search depends on the LLM provider"). */
  private[soness] def bedrockSearch(access: BedrockAccess, id: String, http: Client): Option[WebSearch] =
    import com.jamesward.zio_bedrock.MantleWebSearch
    Option.when(access.mantle && MantleWebSearch.supports(access.region, ModelId(id))):
      val config = MantleConfig(access.apiKey, access.region, ModelId(id), access.endpoint.orNull, MantleApi.OpenAI)
      WebSearch.bedrock((query, size) =>
        MantleWebSearch.search(config, query, Some(size match
          case ContextSize.Low    => MantleWebSearch.ContextSize.Low
          case ContextSize.Medium => MantleWebSearch.ContextSize.Medium
          case ContextSize.High   => MantleWebSearch.ContextSize.High))
          .mapError(e => RuntimeException(s"Bedrock web search: ${e.getMessage}")).provideEnvironment(ZEnvironment(http)))

  /** Mantle's API for a model: xAI's models (Grok) are served only on its OpenAI-compatible `/openai/v1` route. */
  def mantleApi(model: String): MantleApi = if model.startsWith("xai.") then MantleApi.OpenAI else MantleApi.Standard

  /**
   * Whether the model answers: a one-line chat capped at a few output tokens. Bedrock has no model listing that works
   * with an API key across both APIs, and only a call shows that the key, region and model ID together are usable.
   */
  private def check(bedrock: ZEnvironment[Bedrock]): IO[String, ZEnvironment[Bedrock]] =
    Bedrock
      .chat(RequestConfig(messages = List(Message.user("Reply with OK.")), inferenceConfig = InferenceConfig(maxTokens = 16)))
      .asResponse
      .provideEnvironment(bedrock)
      .mapError(_.getMessage)
      .timeoutFail(s"no answer within $probeTimeout")(probeTimeout)
      // a model that answers sometimes fails a call (throttling under load): it counts as unavailable only after 3 tries
      .retry(Schedule.recurs(2) && Schedule.exponential(1.second))
      .as(bedrock)

  /** The web searches' ids. */
  val SearchKiro = "kiro"
  val SearchBedrock = "bedrock"
  val SearchBrave = "brave"

  private def searchChoice(id: String, bedrockModel: Option[String]): HarnessCatalog.Choice =
    if id == SearchKiro then HarnessCatalog.Choice(id, "Kiro", "Kiro's web search (kiro-cli), with cited sources", "Kiro")
    else if id == SearchBrave then HarnessCatalog.Choice(id, "Brave", "Brave Search API's web results, each with its link", "Brave")
    else HarnessCatalog.Choice(id, "Bedrock", s"Bedrock's Web Search${bedrockModel.fold("")(m => s" (with $m)")}, with cited sources", "Bedrock")

  /** `BEDROCK_MODEL_ID` when it is available, else the first available model; none without any. */
  private def defaultModel(available: List[String]): UIO[Option[String]] =
    env("BEDROCK_MODEL_ID").flatMap:
      case Some(id) if available.contains(id) => ZIO.some(id)
      case Some(id) if available.nonEmpty =>
        ZIO.logWarning(s"BEDROCK_MODEL_ID $id is unavailable; sessions start with ${available.head}").as(available.headOption)
      case _ => ZIO.succeed(available.headOption)

  /** Whether any Bedrock setting is given: its key or a model. */
  private val bedrockConfigured: UIO[Boolean] =
    ZIO.foreach(List("AWS_BEARER_TOKEN_BEDROCK", "BEDROCK_MODEL_ID", "SONESS_BEDROCK_MODELS"))(env).map(_.exists(_.isDefined))

  /**
   * `SONESS_ACP_AGENTS`: the ACP agents that can write, `name=command args…` separated by `;` (`kiro=kiro-cli acp`);
   * unset, Kiro when `kiro-cli` is installed; empty, none.
   */
  private def acpAgents: UIO[List[(String, com.jamesward.zioacp.AgentCommand)]] =
    System.env("SONESS_ACP_AGENTS").orElseSucceed(None).flatMap:
      case Some(spec) => ZIO.succeed(parseAgents(spec))
      case None =>
        System.env("PATH").orElseSucceed(None).flatMap: path =>
          ZIO.attemptBlocking(path.toList.flatMap(_.split(java.io.File.pathSeparator)).exists(d =>
            java.nio.file.Files.isExecutable(java.nio.file.Paths.get(d, "kiro-cli")))).orElseSucceed(false)
        .map(found => if found then List(KiroAgent -> com.jamesward.zioacp.AgentCommand("kiro-cli", List("acp"))) else Nil)

  def parseAgents(spec: String): List[(String, com.jamesward.zioacp.AgentCommand)] =
    spec.split(';').toList.map(_.trim).filter(_.nonEmpty).flatMap: entry =>
      entry.split("=", 2) match
        case Array(name, command) if name.trim.nonEmpty && command.trim.nonEmpty =>
          val words = command.trim.split("\\s+").toList
          List(name.trim -> com.jamesward.zioacp.AgentCommand(words.head, words.tail))
        case _ => Nil

  private def env(name: String): UIO[Option[String]] = System.env(name).orDie.map(_.filter(_.nonEmpty))

  /**
   * The agent on stdin and stdout, with `extensions` (none in production; the development build's self-improvement);
   * fails with a configuration problem, logged by itself.
   */
  def serve(extensions: List[Extension]): ZIO[Scope, String, Unit] =
    val program =
      for
        http    <- ZIO.environment[Client]
        catalog <- catalogEnding(extensions.flatMap(_.endsTurn).toSet).tapError(reason => ZIO.logError(reason))
        _       <- ZIO.logInfo(s"models ${catalog.models.mkString(", ")}")
        parent  <- ZIO.scope
        _       <- AcpAgent.serveStdio()(client => SonessAgent.make(client, http, catalog, parent, extensions,
                       // the user's other soness processes see this one's work in /status, and it sees theirs
                       Option.unless(sys.env.get("SONESS_PEERS").exists(_.trim.equalsIgnoreCase("false")))(Peers.defaultDir)))
      yield ()
    program.provideSome[Scope](httpClient.mapError(e => s"the HTTP client couldn't start: ${e.getMessage}"))

/** The soness ACP agent on stdin and stdout; see [[Options]] for its arguments. */
object Main extends ZIOAppDefault:
  // stdout carries the protocol, so logs go to stderr (and, with --debug, a file of their own)
  override val bootstrap = Logging.fromArgs("soness")

  // a SIGTERM (or SIGINT) interrupts the agent, which closes its sessions (stopping their MCP servers and ACP writers);
  // a finalizer that hangs doesn't keep the process alive past this
  override def gracefulShutdownTimeout: Duration = 10.seconds

  def run = Soness.serve(Nil).catchSome { case _: String => exit(ExitCode.failure) }
