package com.jamesward.soness

import com.jamesward.soness.FakeBedrock.Reply
import com.jamesward.zio_decisions.typesafe.FakeJev
import com.jamesward.zio_decisions.orchestration.AgentLoop
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** The Jev harness against the orders MCP server, with a fake Bedrock and a fake Jev scripted like the real models. */
object HarnessSpec extends ZIOSpecDefault:

  /** Records each tool call before making it. */
  private def recording(toolset: Toolset, calls: Ref[Vector[(String, Json.Obj)]]): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] = calls.update(_ :+ (name -> arguments)) *> toolset.call(name, arguments)

  /** A fake Bedrock that records each request, writes `{"customerId":"C0046"}` as arguments, and replies `text`. */
  private def bedrock(requests: Ref[Vector[Json.Obj]], text: String) =
    FakeBedrock.serve: body =>
      requests.update(_ :+ body).as:
        if FakeBedrock.isArguments(body) then Reply.Text("""{"customerId":"C0046"}""") else Reply.Text(text)

  private def systemPrompts(body: Json.Obj): List[String] =
    FakeBedrock.messages(body).filter(_.get("role").contains(Json.Str("system"))).map(_.toJson)

  private val orders = OrdersMcpServer.orders.filter(_.customerId == "C0046").map(_.orderId)

  def spec = suite("Jev harness")(
    test("Jev calls the tool it chooses; Bedrock writes its arguments and the reply, and is never given tools") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        calls    <- Ref.make(Vector.empty[(String, Json.Obj)])
        requests <- Ref.make(Vector.empty[Json.Obj])
        fake     <- FakeJev.make(JevScript.callThenAnswer("list_orders"))
        llm      <- bedrock(requests, "C0046's orders")
        result   <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("List customer C0046's orders.", recording(toolset, calls))
        called   <- calls.get
        sent     <- requests.get
        jevSeen  <- fake._2
        options   = jevSeen.headOption.map(_.toJson).getOrElse("")
      yield assertTrue(
        result.answer == "C0046's orders",
        result.ending == "answered",
        called.map((name, args) => name -> args.toJson) == Vector("list_orders" -> """{"customerId":"C0046"}"""),
        // Jev was offered every tool with its schema, and the endings
        List("list_customers", "list_orders", "get_order").forall(t => options.contains(s""""tool":"$t"""")),
        options.contains("\"inputSchema\""),
        options.contains("\"outcome\":\"answer\"") && options.contains("\"outcome\":\"unable\""),
        // and then saw the call's result
        jevSeen.lift(1).exists(r => orders.forall(r.toJson.contains)),
        sent.size == 2,
        FakeBedrock.isArguments(sent.head) && sent.head.get("tools").exists(_.toJson.contains("customerId")),
        orders.forall(sent(1).toJson.contains),
        sent.forall(FakeBedrock.offersNoTools),
        result.usage.llmCalls == 2,
        // the auto mode's question (one loop or several parts), and the loop's two decisions
        result.usage.jevCalls == 3,
      )
    },
    test("a failing tool is a step Jev sees, and it can end as unable; the reply explains") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        requests <- Ref.make(Vector.empty[Json.Obj])
        fake     <- FakeJev.make(JevScript.callThenUnable("get_order"))
        llm      <- bedrock(requests, "I couldn't find that order.")
        result   <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("Order O1?", toolset)
        jevSeen  <- fake._2
        sent     <- requests.get
      yield assertTrue(
        result.ending == "unable",
        result.answer == "I couldn't find that order.",
        jevSeen.lift(1).exists(_.toJson.contains("\"outcome\":\"failed\"")),
        sent.lastOption.exists(body => systemPrompts(body).exists(_.contains("can't be completed")) && body.toJson.contains("failed")),
      )
    },
    test("a question about the agent's abilities is answered from the session's tools, which the reply writer is told") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        requests <- Ref.make(Vector.empty[Json.Obj])
        fake     <- FakeJev.make(JevScript.answer)
        llm      <- bedrock(requests, "I can list customers and orders.")
        _        <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("what tools do you have available?", toolset)
        sent     <- requests.get
        reply     = sent.last.toJson
      yield assertTrue(
        List("list_customers", "list_orders", "get_order").forall(reply.contains),
        reply.contains("it has nothing else"),
        systemPrompts(sent.last).exists(_.contains("never from your own")),
      )
    },
    test("tool arguments are written from the tool's schema and the earlier errors, with a capped, deterministic generation") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        requests <- Ref.make(Vector.empty[Json.Obj])
        fake     <- FakeJev.make(JevScript.callThenAnswer("get_order"))
        llm      <- bedrock(requests, "done")
        _        <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("Order O10460?", toolset)
        sent     <- requests.get
        arguments = sent.head
      yield assertTrue(
        FakeBedrock.isArguments(arguments),
        arguments.toJson.contains("Its input schema"),
        arguments.get("max_tokens").exists(_.toJson == "16384") || arguments.get("max_completion_tokens").exists(_.toJson == "16384"),
        arguments.get("temperature").exists(_.toJson.startsWith("0")),
      )
    },
    test("a batch is written as one forced call of batch, its calls each one of the tools and its arguments") {
      import com.jamesward.zio_decisions.orchestration.AgentLoop
      val read = AgentLoop.Tool("read_file", "Reads.", Json.Obj("type" -> Json.Str("object"),
        "properties" -> Json.Obj("path" -> Json.Obj("type" -> Json.Str("string")))))
      for
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- FakeBedrock.serve(body => requests.update(_ :+ body).as(Reply.Text(
                      """{"calls":[{"tool":"read_file","arguments":{"path":"A"}},{"tool":"read_file","arguments":{"path":"B"}},{"arguments":{}}]}""")))
        written  <- BedrockLlmTransport(llm).workflow(AgentLoop.WorkflowRequest("read A and B", List(read), Vector.empty, Vector.empty,
                      None, 5, concurrent = true))
        sent     <- requests.get
      yield assertTrue(
        written.value.map(c => (c.tool, c.arguments.toJson)) == List("read_file" -> """{"path":"A"}""", "read_file" -> """{"path":"B"}"""),
        FakeBedrock.forcedTool(sent.head).contains("batch"),
        sent.head.toJson.contains("run at the same time"), sent.head.toJson.contains("\\\"enum\\\":[\\\"read_file\\\"]") ||
          sent.head.toJson.contains("\"enum\":[\"read_file\"]"),
      )
    },
    test("arguments Bedrock writes as invalid JSON are a failed step Jev sees, not a failed turn") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        calls    <- Ref.make(Vector.empty[(String, Json.Obj)])
        // as gpt-oss-120b wrote it in IntelliJ: a stray brace and single-quoted fragments
        llm      <- FakeBedrock.serve: body =>
                      ZIO.succeed:
                        if FakeBedrock.isArguments(body) then Reply.Text("""{
{"command":"list_directory_tree','path':'src'}""")
                        else Reply.Text("I couldn't list the files.")
        fake     <- FakeJev.make(JevScript.callThenUnable("list_orders"))
        result   <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("what src files are there", recording(toolset, calls))
        called   <- calls.get
        jevSeen  <- fake._2
      yield assertTrue(
        result.ending == "unable",
        result.answer == "I couldn't list the files.",
        called.isEmpty,
        jevSeen.lift(1).exists(_.toJson.contains("could not be written")),
      )
    },
    test("after a failed call, Bedrock is shown that call's arguments and asked for different ones") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        requests <- Ref.make(Vector.empty[Json.Obj])
        count    <- Ref.make(0)
        llm      <- FakeBedrock.serve: body =>
                      requests.update(_ :+ body) *> count.getAndUpdate(_ + 1).map: n =>
                        if FakeBedrock.isArguments(body) then
                          Reply.Text(if n == 0 then """{"orderId":"nope"}""" else """{"orderId":"O10460"}""")
                        else Reply.Text("done")
        fake     <- FakeJev.make(JevScript((steps, view) => if steps.size < 2 then JevScript.isTool("get_order")(view) else JevScript.isAnswer(view)))
        _        <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("Order O10460?", toolset)
        sent     <- requests.get
        second    = sent(1).toJson
      yield assertTrue(
        second.contains("already failed"),
        second.contains("""{\\\"orderId\\\":\\\"nope\\\"}""") || second.contains("nope"),
      )
    },
    test("without tools Jev is offered only the endings, and Bedrock answers with a prompt that says so") {
      for
        fake     <- FakeJev.make(JevScript.answer)
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- bedrock(requests, "42")
        result   <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("What is 17 + 25?", Toolset.empty)
        jevSeen  <- fake._2
        sent     <- requests.get
      yield assertTrue(
        result.answer == "42",
        result.usage.jevCalls == 1,
        result.usage.llmCalls == 1,
        !jevSeen.head.toJson.contains("call_tool"),
        sent.size == 1,
        FakeBedrock.offersNoTools(sent.head),
        sent.head.toJson.contains("No tools were called"),
      )
    },
    test("the project context and the harness's direction reach Jev, the arguments and the reply") {
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        fake     <- FakeJev.make(JevScript.callThenAnswer("list_orders"))
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- bedrock(requests, "done")
        _        <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("List C0046's orders.", toolset, context = Some("Project directory: /p"))
        jevSeen  <- fake._2
        sent     <- requests.get
      yield assertTrue(
        jevSeen.forall(_.toJson.contains("Project directory: /p")),
        jevSeen.head.toJson.contains("coding agent"),
        sent.size == 2,
        sent.forall(_.toJson.contains("Project directory: /p")),
        sent.forall(b => systemPrompts(b).exists(_.contains("coding agent"))),
        systemPrompts(sent(1)).exists(_.contains("never write a plan")),
      )
    },
    test("a skill Jev chooses to load (here, the one the project names) is in every decision's and writing's context; the others are only offered") {
      val skill = AgentLoop.Document("skill:zen-of-scala", "zen-of-scala", "Scala idioms",
        ZIO.succeed(AgentLoop.DocumentContent("ALWAYS-USE-ZIO-DIRECT")), Map(Skills.Required -> "true"))
      val other = AgentLoop.Document("skill:other", "other", "Other", ZIO.succeed(AgentLoop.DocumentContent("OTHER-TEXT")))
      for
        url      <- OrdersMcpServer.serve
        toolset  <- McpToolset.connect(List(McpEndpoint.Http("orders", url))).map(_.toolset)
        fake     <- FakeJev.make(JevScript.callThenAnswer("list_orders"))
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- bedrock(requests, "done")
        _        <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("List C0046's orders.", toolset, List(skill, other))
        jevSeen  <- fake._2
        sent     <- requests.get
      yield assertTrue(
        jevSeen.nonEmpty, jevSeen.forall(_.toJson.contains("ALWAYS-USE-ZIO-DIRECT")),
        sent.nonEmpty, sent.forall(_.toJson.contains("ALWAYS-USE-ZIO-DIRECT")),
        !jevSeen.head.toJson.contains("skill:zen-of-scala"), jevSeen.head.toJson.contains("skill:other"),
        !sent.exists(_.toJson.contains("OTHER-TEXT")),
      )
    },
    test("MCP server instructions join the project context") {
      for
        url       <- OrdersMcpServer.serve
        connected <- McpToolset.connect(List(McpEndpoint.Http("orders", url)))
      yield assertTrue(
        connected.instructions == List("orders" -> OrdersMcpServer.Instructions),
        ProjectContext.withServers(Some("Project directory: /p"), List("sbta" -> "Use check after edits.")).contains(
          "Project directory: /p\n\nThe MCP server sbta's instructions (for its tools):\nUse check after edits."),
        ProjectContext.withServers(None, Nil).isEmpty,
      )
    },
    test("an empty reply is asked for once more, and both calls' tokens are counted") {
      for
        fake     <- FakeJev.make(JevScript.answer)
        requests <- Ref.make(Vector.empty[Json.Obj])
        llm      <- FakeBedrock.serve(body => requests.updateAndGet(_ :+ body).map(sent => Reply.Text(if sent.size == 1 then " " else "Hello!")))
        result   <- DecisionHarness(fake._1, BedrockLlmTransport(llm)).run("say hello", Toolset.empty)
        sent     <- requests.get
      yield assertTrue(
        result.answer == "Hello!",
        sent.size == 2,
        result.usage.llmInputTokens == 200,
        result.usage.llmOutputTokens == 20,
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock
