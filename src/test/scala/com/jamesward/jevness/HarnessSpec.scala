package com.jamesward.jevness

import com.jamesward.jevness.FakeBedrock.Reply
import com.jamesward.jevness.OrdersMcpServer.customers
import com.jamesward.zio_typesafe_ai.FakeJev
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** Both harnesses against the orders MCP server, with a fake Bedrock and a fake Jev scripted like the real models. */
object HarnessSpec extends ZIOSpecDefault:

  private val prompt = "Which orders of our Platinum-tier EMEA customers are still in placed status?"
  private val platinumEmea = customers.filter(c => c.tier == "Platinum" && c.region == "EMEA").map(_.customerId)

  /** Records each tool call before making it. */
  private def recording(toolset: Toolset, calls: Ref[Vector[(String, Json.Obj)]]): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ (name -> arguments)) *> toolset.call(name, arguments)

  /** Jev's next action: fan out, else filter the customer list, else list customers, else summarize, else finish. */
  private def nextAction(options: List[(String, String)]): String =
    def find(p: String => Boolean) = options.find(o => p(o._2)).map(_._1)
    find(_.contains("\"mode\":\"FanOut\""))
      .orElse(find(o => o.contains("\"transition\":\"filter\"") && o.contains("list_customers")))
      .orElse(find(_.contains("\"name\":\"list_customers\"")))
      .orElse(find(_.contains("\"transition\":\"summarize\"")))
      .orElse(find(_.contains("\"transition\":\"finish\"")))
      .getOrElse(options.head._1)

  /** Jev's filter answers: the region and tier fields are relevant, and Platinum EMEA variants match. */
  private def jev(state: Json.Obj, id: String, question: Json.Obj): FakeJev.Answer =
    question.get("criteria").flatMap(_.asObject) match
      case Some(criteria) => FakeJev.Answer.Choice(nextAction(criteria.fields.toList.map((c, v) => c -> v.toJson)))
      case None =>
        val index = id.stripPrefix("item_").toInt
        val fields = state.get("fields").flatMap(_.asArray)
        val candidate = fields.orElse(state.get("variants").flatMap(_.asArray)).flatMap(_.lift(index)).map(_.toJson.toLowerCase).getOrElse("")
        val relevant =
          if fields.isDefined then candidate.contains("/region") || candidate.contains("/tier")
          else candidate.contains("platinum") && candidate.contains("emea")
        FakeJev.Answer.Noul(if relevant then 0.9 else 0.1)

  def spec = suite("harnesses")(
    test("jev: Jev filters and fans out; the LLM only reads the filtered evidence") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint("orders", url)))
        calls    <- Ref.make(Vector.empty[(String, Json.Obj)])
        requests <- Ref.make(Vector.empty[Json.Obj])
        fake     <- FakeJev.make(jev)
        bedrock  <- FakeBedrock.serve(body => requests.update(_ :+ body).as(Reply.Text("the placed orders")))
        result   <- JevHarness(fake._1, BedrockLlmTransport(bedrock)).run(prompt, recording(toolset, calls))
        called   <- calls.get
        llm      <- requests.get
      yield assertTrue(
        result.answer == "the placed orders",
        called.map(_._1) == Vector("list_customers") ++ Vector.fill(platinumEmea.size)("list_orders"),
        called.collect { case ("list_orders", args) => args.get("customerId").flatMap(_.asString).get }.sorted == platinumEmea.sorted,
        llm.size == 1,
        platinumEmea.forall(llm.head.toJson.contains),
        !llm.head.toJson.contains("C0001"),
        result.usage.llmCalls == 1,
        result.usage.llmInputTokens == 100L,
        result.usage.jevCalls >= 5,
      )
    },
    test("llm: the model reads every tool result, including the whole customer list") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint("orders", url)))
        calls    <- Ref.make(Vector.empty[(String, Json.Obj)])
        requests <- Ref.make(Vector.empty[Json.Obj])
        bedrock  <- FakeBedrock.serve: body =>
                      requests.update(_ :+ body).as:
                        if FakeBedrock.messages(body).exists(_.get("role").contains(Json.Str("tool"))) then Reply.Text("done")
                        else Reply.ToolCall("list_customers", Json.Obj())
        result   <- LlmHarness(bedrock).run(prompt, recording(toolset, calls))
        called   <- calls.get
        llm      <- requests.get
      yield assertTrue(
        result.answer == "done",
        called.map(_._1) == Vector("list_customers"),
        llm.size == 2,
        llm(1).toJson.contains("C0300"),
        result.usage == ModelUsage(llmCalls = 2, llmInputTokens = 200, llmOutputTokens = 20),
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
