package com.jamesward.soness

import com.jamesward.zio_bedrock.{Bedrock, Mantle, MantleConfig}
import com.jamesward.zio_bedrock.Bedrock.{ApiKey, ModelId}
import zio.*
import zio.http.*
import zio.json.*
import zio.json.ast.Json

/**
 * A local OpenAI-compatible Chat Completions endpoint standing in for Bedrock Mantle. Like Mantle, it rejects a request
 * with a `tool_choice` and no `tools`.
 */
object FakeBedrock:
  enum Reply:
    case Text(text: String)
    case ToolCall(name: String, arguments: Json.Obj)
    /** An HTTP error with an OpenAI-style error body, as Mantle answers for a model it doesn't serve. */
    case Failure(status: Status, message: String)

  /** Serves `reply(requestBody)` for the life of the scope; every reply reports 100 input and 10 output tokens. */
  def serve(reply: Json.Obj => UIO[Reply]): ZIO[Scope, Throwable, ZEnvironment[Bedrock]] =
    for
      url <- serveUrl(reply)
      env <- (Client.default >>> Mantle.layer(MantleConfig(ApiKey("dev"), url, ModelId("fake")))).build
    yield env

  /** As [[serve]], giving the endpoint's origin, for a Mantle client in another process. */
  def serveUrl(reply: Json.Obj => UIO[Reply]): ZIO[Scope, Throwable, URL] =
    val routes = Routes(
      Method.POST / "v1" / "chat" / "completions" -> handler { (request: Request) =>
        request.body.asString.orDie
          .flatMap: body =>
            val json = body.fromJson[Json.Obj].toOption.get
            // as Mantle does: a tool choice needs tools
            if json.get("tool_choice").isDefined && !hasTools(json) then
              ZIO.succeed(Reply.Failure(Status.BadRequest, "When using `tool_choice`, `tools` must be set."))
            else reply(json).map(r => asForcedCall(json, r))
          .map(response)
      },
    )
    for
      server <- Server.defaultWith(_.port(0).disableRequestStreaming(16 * 1024 * 1024)).build
      port   <- Server.install(routes).provideEnvironment(server)
    yield URL.decode(s"http://127.0.0.1:$port").toOption.get

  private def response(reply: Reply): Response =
    reply match
      case Reply.Text(text) =>
        completion(Json.Obj("role" -> Json.Str("assistant"), "content" -> Json.Str(text)), "stop")
      case Reply.ToolCall(name, arguments) =>
        completion(
          Json.Obj("role" -> Json.Str("assistant"), "tool_calls" -> Json.Arr(Json.Obj(
            "id" -> Json.Str(s"call-$name"),
            "type" -> Json.Str("function"),
            "function" -> Json.Obj("name" -> Json.Str(name), "arguments" -> Json.Str(arguments.toJson)),
          ))),
          "tool_calls",
        )
      case Reply.Failure(status, message) =>
        Response.json(Json.Obj("error" -> Json.Obj("message" -> Json.Str(message), "type" -> Json.Str("invalid_request_error"))).toJson)
          .status(status)

  private def completion(message: Json.Obj, finish: String): Response =
    Response.json(Json.Obj(
      "id" -> Json.Str("chatcmpl-fake"),
      "object" -> Json.Str("chat.completion"),
      "created" -> Json.Num(1),
      "model" -> Json.Str("fake"),
      "choices" -> Json.Arr(Json.Obj("index" -> Json.Num(0), "message" -> message, "finish_reason" -> Json.Str(finish))),
      "usage" -> Json.Obj("prompt_tokens" -> Json.Num(100), "completion_tokens" -> Json.Num(10), "total_tokens" -> Json.Num(110)),
    ).toJson)

  /** The request's messages. */
  def messages(body: Json.Obj): List[Json.Obj] =
    body.get("messages").flatMap(_.asArray).toList.flatten.flatMap(_.asObject)

  /** The request's model id. */
  def model(body: Json.Obj): Option[String] = body.get("model").flatMap(_.asString)

  /** Whether the request offers tools: soness's startup model check doesn't. */
  def hasTools(body: Json.Obj): Boolean = body.get("tools").flatMap(_.asArray).exists(_.nonEmpty)

  /**
   * A text reply to a request that forces a tool call (zio-bedrock's structured output on Mantle) becomes that call,
   * the text its arguments, as the real API answers; text that isn't a JSON object stays text.
   */
  private def asForcedCall(body: Json.Obj, reply: Reply): Reply =
    (reply, forcedTool(body)) match
      // the reply as a forced call takes the text as its message
      case (Reply.Text(text), Some("reply_to_user")) => Reply.ToolCall("reply_to_user", Json.Obj("message" -> Json.Str(text)))
      case (Reply.Text(text), Some(tool)) => text.fromJson[Json.Obj].fold(_ => reply, arguments => Reply.ToolCall(tool, arguments))
      case _                              => reply

  /** Whether the request asks for a tool's arguments: structured output, as one forced tool call. */
  def isArguments(body: Json.Obj): Boolean = forcedTool(body).exists(_ != "reply_to_user")

  /** The names of the tools the request offers. */
  def toolNames(body: Json.Obj): List[String] =
    body.get("tools").flatMap(_.asArray).toList.flatten
      .flatMap(_.asObject.flatMap(_.get("function")).flatMap(_.asObject).flatMap(_.get("name")).flatMap(_.asString))

  /** A request no agent tool is given to: no tools, or only the one it forces for structured output. */
  def offersNoTools(body: Json.Obj): Boolean = toolNames(body) == forcedTool(body).toList

  /** The name of the tool the request forces, if any. */
  def forcedTool(body: Json.Obj): Option[String] =
    body.get("tool_choice").flatMap(_.asObject).flatMap(_.get("function")).flatMap(_.asObject)
      .flatMap(_.get("name")).flatMap(_.asString)
