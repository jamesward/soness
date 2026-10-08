package com.jamesward.zio_typesafe_ai

import com.jamesward.zio_typesafe_ai.TypeSafeAI.{Client, Error, ModelId}
import com.jamesward.zio_typesafe_ai.internal.Wire
import zio.*
import zio.json.ast.Json

/**
 * A stand-in for the Jev API, for tests without `TYPESAFE_API_KEY`. It lives in zio-typesafe-ai's package because a
 * `TypeSafeAI.Client` is implemented against that library's package-private wire types.
 */
object FakeJev:
  enum Answer:
    case Choice(id: String)
    case Noul(probability: Double)

  /** Answers every question of a request: `answer(state, questionId, question)`. Records each request body. */
  def make(answer: (Json.Obj, String, Json.Obj) => Answer): UIO[(ZEnvironment[Client], UIO[Vector[Json]])] =
    Ref.make(Vector.empty[Json]).map: seen =>
      val client = new Client:
        val modelId: ModelId = ModelId("fake-jev")
        def send(request: Wire.SystemOneRequest): IO[Error, Wire.SystemOneResponse] =
          val body = request.body.asObject.getOrElse(Json.Obj())
          val state = body.get("state").flatMap(_.asObject).getOrElse(Json.Obj())
          val questions = body.get("questions").flatMap(_.asObject).toList.flatMap(_.fields)
          seen.update(_ :+ request.body).as(Wire.SystemOneResponse(
            modelId.unwrap,
            questions.map { (id, question) =>
              id -> (answer(state, id, question.asObject.getOrElse(Json.Obj())) match
                case Answer.Choice(choice) => Wire.Answer.Choice(choice, Map(choice -> 1.0), 1.0)
                case Answer.Noul(p)        => Wire.Answer.Noul(p))
            }.toMap,
            Wire.Usage(10, 1),
          ))
      (ZEnvironment[Client](client), seen.get)
