package com.jamesward.zio_decisions.typesafe

import com.jamesward.zio_decisions.DecisionModel
import com.jamesward.zio_decisions.Decisions.{Error, ModelId}
import zio.*
import zio.http.{Method, Request, Response, Routes, Server, URL, handler}
import zio.json.*
import zio.json.ast.Json

/**
 * A stand-in for the Jev API, for tests without `TYPESAFE_API_KEY`: in process (`make`), or over HTTP for a soness
 * subprocess (`serve`, with `SONESS_TYPESAFE_ENDPOINT`). It lives in zio-decisions' `typesafe` package because it is
 * a `TypeSafe.Transport`, which (with its wire types) is package-private there.
 */
object FakeJev:
  enum Answer:
    case Choice(id: String)
    case Noul(probability: Double)

  type Answers = (Json.Obj, String, Json.Obj) => Answer

  /**
   * The auto mode's first question (is the request several parts?) is answered no, and its request isn't recorded:
   * the scripts are of the one loop that follows.
   */
  private def withAuto(answer: Answers): Answers =
    (state, id, question) =>
      if id == "several_parts" then Answer.Noul(0.0)
      // which skills to load (SkillChoice): the ones the project's instructions name
      else if id.startsWith("skill_") then Answer.Noul(if question.toJson.contains("\"named\":true") then 0.9 else 0.1)
      // what a call's writer needs (AgentLoop's briefArguments): everything
      else if briefing(id) then Answer.Noul(0.9)
      else answer(state, id, question)

  private def briefing(id: String): Boolean = id == "context" || id.startsWith("step_") || id.startsWith("read_")

  /** The requests that come before the loop's own (is it several parts? which skills?): not recorded. */
  private def isAuto(body: Json.Obj): Boolean =
    body.get("questions").flatMap(_.asObject).exists(q => (q.fields.size == 1 && q.get("several_parts").isDefined) ||
      (q.fields.nonEmpty && q.fields.forall(f => f._1.startsWith("skill_") || briefing(f._1))))

  private def respond(answer: Answers, body: Json.Obj): Json.Obj =
    val state = body.get("state").flatMap(_.asObject).getOrElse(Json.Obj())
    val questions = body.get("questions").flatMap(_.asObject).toList.flatMap(_.fields)
    val answers = questions.map: (id, question) =>
      id -> (withAuto(answer)(state, id, question.asObject.getOrElse(Json.Obj())) match
        case Answer.Choice(choice) =>
          Json.Obj("type" -> Json.Str("choice"), "choice" -> Json.Str(choice),
            "probabilities" -> Json.Obj(choice -> Json.Num(1.0)), "confidence" -> Json.Num(1.0))
        case Answer.Noul(p) => Json.Obj("type" -> Json.Str("noul"), "noul" -> Json.Num(p)))
    Json.Obj(
      "model" -> Json.Str("fake-jev"),
      "answers" -> Json.Obj(answers*),
      "usage" -> Json.Obj("input_tokens" -> Json.Num(10), "output_tokens" -> Json.Num(1)),
    )

  /** Answers every question of a request: `answer(state, questionId, question)`. Records each request body. */
  def make(answer: Answers): UIO[(ZEnvironment[DecisionModel], UIO[Vector[Json]])] =
    Ref.make(Vector.empty[Json]).map: seen =>
      val modelId: ModelId = ModelId("fake-jev")
      val transport = new TypeSafe.Transport:
        def send(request: Wire.SystemOneRequest): IO[Error, Wire.SystemOneResponse] =
          val body = request.body.asObject.getOrElse(Json.Obj())
          val state = body.get("state").flatMap(_.asObject).getOrElse(Json.Obj())
          val questions = body.get("questions").flatMap(_.asObject).toList.flatMap(_.fields)
          seen.update(s => if isAuto(body) then s else s :+ request.body).as(Wire.SystemOneResponse(
            modelId.unwrap,
            questions.map { (id, question) =>
              id -> (withAuto(answer)(state, id, question.asObject.getOrElse(Json.Obj())) match
                case Answer.Choice(choice) => Wire.Answer.Choice(choice, Map(choice -> 1.0), 1.0)
                case Answer.Noul(p)        => Wire.Answer.Noul(p))
            }.toMap,
            Wire.Usage(10, 1),
          ))
      (ZEnvironment[DecisionModel](TypeSafe.model(transport, modelId)), seen.get)

  /** Serves `answer` as the Jev API on a free port for the life of the scope, and gives its origin. */
  def serve(answer: Answers): ZIO[Scope, Throwable, URL] =
    val routes = Routes(
      Method.POST / "v1" / "systemone" -> handler { (request: Request) =>
        request.body.asString.orDie.map: body =>
          Response.json(respond(answer, body.fromJson[Json.Obj].toOption.getOrElse(Json.Obj())).toJson)
      },
    )
    for
      server <- Server.defaultWith(_.port(0).disableRequestStreaming(64 * 1024 * 1024)).build
      port   <- Server.install(routes).provideEnvironment(server)
    yield URL.decode(s"http://127.0.0.1:$port").toOption.get
