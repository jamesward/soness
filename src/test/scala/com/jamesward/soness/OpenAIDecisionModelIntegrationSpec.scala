package com.jamesward.soness

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.{ChoiceCriteria, Content, DynamicAnswer, Error, NoulCriteria, Question, QuestionId, ScoreCriteria}
import com.jamesward.zio_decisions.openai.OpenAI
import zio.*
import zio.http.Client
import zio.test.*

/**
 * OpenAI's Decisions API as soness's decision model, live: selected through `SONESS_DECISION_MODEL=openai` (its default
 * model) as soness selects it, each kind of question (predicate, choice, score) answered as the orchestration's, and a
 * bad key failing as a failure the orchestration doesn't retry. Paid; runs only when `OPENAI_API_KEY` is set (the
 * project's `.env` sets it for tests).
 */
object OpenAIDecisionModelIntegrationSpec extends ZIOSpecDefault:

  /** The environment, with `SONESS_DECISION_MODEL` (and any other) replaced. */
  private def credentials(overrides: (String, String)*): DecisionModels.Credentials =
    name => overrides.toMap.get(name).fold(DecisionModels.environment(name))(v => ZIO.some(v))

  private val openai: ZIO[Client & Scope, String, DecisionModel] =
    DecisionModels.connect(credentials(DecisionModels.Variable -> "openai")).map(_._2.get[DecisionModel])

  private def decide(model: DecisionModel, state: String, questions: (String, Question[?])*) =
    model.decide(DecisionModel.Request(Content(state), questions.toList.map((id, q) => QuestionId(id) -> q)))

  def spec = suite("OpenAI's Decisions API, live")(
    test("selected with SONESS_DECISION_MODEL=openai, it's OpenAI's default model") {
      openai.map(model => assertTrue(model.name == s"openai:${OpenAI.DefaultModel.unwrap}"))
    },
    test("a predicate: the clear yes and the clear no in one request, each by its id, with the usage") {
      for
        model    <- openai
        decision <- decide(model, "A file listing: build.sbt, src/main/scala/Main.scala, README.md",
                      "lists_scala" -> Question.Noul("The listing has a Scala source file.",
                        NoulCriteria("a file ends in .scala", "no file ends in .scala")),
                      "lists_python" -> Question.Noul("The listing has a Python source file.",
                        NoulCriteria("a file ends in .py", "no file ends in .py")))
      yield
        def p(id: String) = decision.answers.get(QuestionId(id)).collect { case DynamicAnswer.Noul(p) => p.unwrap }
        assertTrue(p("lists_scala").exists(_ > 0.8), p("lists_python").exists(_ < 0.2), decision.usage.inputTokens > 0)
    },
    test("a choice: the option that fits, with a probability for every option") {
      for
        model    <- openai
        criteria <- ZIO.fromEither(ChoiceCriteria(
                      "read" -> "read a file whose path is known", "search" -> "search for text whose file isn't known",
                      "edit" -> "change a file"))
        decision <- decide(model, "Request: show me what's in build.sbt. Nothing has been done yet.",
                      "next" -> Question.Choice("The agent's next step.", criteria))
      yield
        val answer = decision.answers.get(QuestionId("next")).collect { case DynamicAnswer.Choice(a) => a }
        assertTrue(answer.exists(_.choice == "read"), answer.exists(_.probabilities.keySet == Set("read", "search", "edit")))
    },
    test("a score: a level over the question's ordered levels, its legend the levels' descriptions") {
      for
        model    <- openai
        criteria <- ZIO.fromEither(ScoreCriteria("nothing of what was asked is done", "part of it is done",
                      "all of it is done and checked"))
        decision <- decide(model, "Request: fix the failing test. The agent patched the bug, ran the tests: all passed.",
                      "progress" -> Question.Score("How much of the request is done.", criteria))
      yield
        val answer = decision.answers.get(QuestionId("progress")).collect { case DynamicAnswer.Score(a) => a }
        assertTrue(answer.exists(_.score > 1.4), answer.exists(_.legend.get(2).contains("all of it is done and checked")))
    },
    test("a bad key fails as an authentication error, which isn't retried") {
      for
        model <- DecisionModels.connect(credentials(DecisionModels.Variable -> "openai", "OPENAI_API_KEY" -> "sk-not-a-key"))
                   .map(_._2.get[DecisionModel])
        exit  <- decide(model, "anything", "q" -> Question.Noul("Anything.")).exit
      yield assertTrue(exit match
        case Exit.Failure(cause) => cause.failureOption.exists(_.isInstanceOf[Error.Authentication])
        case _                   => false)
    },
  ).provideSomeLayerShared[Scope](Client.default.orDie) @@ TestAspect.ifEnvSet("OPENAI_API_KEY") @@ TestAspect.withLiveClock
    @@ TestAspect.withLiveSystem @@ TestAspect.timeout(2.minutes) @@ TestAspect.sequential
