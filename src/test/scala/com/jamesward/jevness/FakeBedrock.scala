package com.jamesward.jevness

import com.jamesward.zio_bedrock.{Bedrock, Mantle, MantleConfig}
import com.jamesward.zio_bedrock.Bedrock.{ApiKey, ModelId}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

/** A local OpenAI-compatible Chat Completions endpoint standing in for Bedrock Mantle. */
object FakeBedrock:
  enum Reply:
    case Text(text: String)
    case ToolCall(name: String, arguments: Json.Obj)

  /** Serves `reply(requestBody)` for the life of the scope; every reply reports 100 input and 10 output tokens. */
  def serve(reply: Json.Obj => UIO[Reply]): ZIO[Scope, Throwable, ZEnvironment[Bedrock]] =
    val routes = Routes(
      Method.POST / "v1" / "chat" / "completions" -> handler { (request: Request) =>
        request.body.asString.orDie
          .flatMap(body => reply(body.fromJson[Json.Obj].toOption.get))
          .map(r => Response.json(completion(r)))
      },
    )
    for
      server <- Server.defaultWith(_.port(0).disableRequestStreaming(16 * 1024 * 1024)).build
      port   <- Server.install(routes).provideEnvironment(server)
      env    <- (Client.default >>> Mantle.layer(MantleConfig(ApiKey("dev"), URL.decode(s"http://127.0.0.1:$port").toOption.get, ModelId("fake")))).build
    yield env

  private def completion(reply: Reply): String =
    val (message, finish) = reply match
      case Reply.Text(text) => Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Str(text)) -> "stop"
      case Reply.ToolCall(name, arguments) =>
        Json.Obj("role" -> Json.Str("assistant"), "tool_calls" -> Json.Arr(Json.Obj(
          "id" -> Json.Str(s"call-$name"),
          "type" -> Json.Str("function"),
          "function" -> Json.Obj("name" -> Json.Str(name), "arguments" -> Json.Str(arguments.toJson)),
        ))) -> "tool_calls"
    Json.Obj(
      "id" -> Json.Str("chatcmpl-fake"),
      "object" -> Json.Str("chat.completion"),
      "created" -> Json.Num(1),
      "model" -> Json.Str("fake"),
      "choices" -> Json.Arr(Json.Obj("index" -> Json.Num(0), "message" -> message, "finish_reason" -> Json.Str(finish))),
      "usage" -> Json.Obj("prompt_tokens" -> Json.Num(100), "completion_tokens" -> Json.Num(10), "total_tokens" -> Json.Num(110)),
    ).toJson

  /** The request's messages. */
  def messages(body: Json.Obj): List[Json.Obj] =
    body.get("messages").flatMap(_.asArray).toList.flatten.flatMap(_.asObject)

  /** The name of the tool the request forces, if any. */
  def forcedTool(body: Json.Obj): Option[String] =
    body.get("tool_choice").flatMap(_.asObject).flatMap(_.get("function")).flatMap(_.asObject)
      .flatMap(_.get("name")).flatMap(_.asString)
