package com.jamesward.soness

import com.jamesward.soness.AcpSubprocess.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}
import zio.test.*

import java.nio.file.Path

/**
 * Live evals of the IntelliJ IDEA use case, as close to it as can be run unattended. soness runs as a subprocess and
 * is driven with IntelliJ IDEA 2026.2.3's own messages: its captured `initialize`, and the `session/new` and
 * `session/prompt` it sends for an IDE chat (`intellij-idea-2026.2.3-ide-session.jsonl`), with only the project, the
 * IDE server's command and the prompt text substituted. The session's one MCP server is `IntellijMcpFixture`, a mock of
 * IntelliJ's router-only server that `IntellijRouterSpec` checks against the real one's captured exchanges; its skill
 * root holds IntelliJ's own skill views (`intellij-skill-views`, copied from IntelliJ's cache). The project is a small
 * Scala project whose sources are deeper than `list_directory_tree`'s default depth, as real ones are.
 *
 * Each typical prompt runs in a fresh session with the real Jev and Bedrock (`BEDROCK_MODEL_ID` only), and is checked
 * deterministically; the report lists each prompt's routed commands, so argument-writing mistakes show apart from
 * wrong answers. Needs `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID`; `SONESS_EVAL_SAMPLES`
 * (default 1) sets the samples. It calls paid APIs.
 *
 * Against the real IDE instead of the mock: run IntelliJ IDEA's MCP server headless on a copy of [[project]], in router
 * mode (`idea mcpServer --project=<dir> --port=<port> --invocation-mode=router --allowed-tools='*'`, with its own
 * `IDEA_PROPERTIES` config and system paths so it doesn't clash with a running IDE), and set `SONESS_IJ_MCP_PORT` to
 * its port, `SONESS_IJ_PROJECT` to that directory and `SONESS_IJ_IDEA` to the `idea` launcher: the session then gets
 * the stdio bridge IntelliJ IDEA gives ACP agents (`idea stdioMcpServer`). `SONESS_IJ_SKILL_ROOT` replaces the skill
 * root (to try other skills, such as IntelliJ's bundled ones its registry leaves off).
 */
object IntellijEvalSpec extends ZIOSpecDefault:

  /**
   * A prompt, what its answer must satisfy, and whether it needs no tool call (`noTools`) or must be answered from a
   * completed call (`needsTools`: an answer the model guessed doesn't count) of the right routed tool (`tool`, the one
   * IntelliJ IDEA's tools are meant for the request: `search_file` lists files, `list_directory_tree` shows structure).
   */
  final case class Prompt(
    name: String,
    prompt: String,
    check: String => Boolean,
    expectation: String,
    noTools: Boolean = false,
    needsTools: Boolean = true,
    tool: Option[String] = None,
  )

  val tasks: List[Prompt] = List(
    Prompt("list files", "list the src files",
      a => List("Main.scala", "Greeter.scala", "GreeterSpec.scala").forall(a.contains), "names Main.scala, Greeter.scala, GreeterSpec.scala",
      tool = Some("search_file")),
    Prompt("read a file", "what scala version does build.sbt set?", _.contains("3.7.2"), "says 3.7.2", tool = Some("read_file")),
    Prompt("find a symbol", "in which file is greetLoudly defined?", _.contains("Greeter.scala"), "names Greeter.scala",
      tool = Some("search_symbol")),
    Prompt("explain code", "what does Greeter.greet return for \"Ann\"?", _.contains("Hello, Ann!"), "says Hello, Ann!",
      tool = Some("read_file")),
    Prompt("own tools", "what tools do you have available?",
      // the IDE has Python tools; a browser, calculator or web search would be invented
      a => !"(?i)browse|calculator|web search".r.findFirstIn(a).isDefined && a.contains("read_file") && a.contains("search_file"),
      "names the IDE's tools (read_file, search_file) and no invented ones", needsTools = false),
    Prompt("greeting", "say hello", _.toLowerCase.contains("hello"), "says hello, with no tool call", noTools = true, needsTools = false),
  )

  /** The project: a small sbt project, its sources five levels under `src`, as real ones are. */
  val project: Seq[(String, String)] = Seq(
    "build.sbt" ->
      """name := "greeter"
        |
        |scalaVersion := "3.7.2"
        |
        |libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
        |""".stripMargin,
    "README.md" -> "# greeter\n\nPrints greetings.\n",
    "src/main/scala/com/example/greeter/Main.scala" ->
      """package com.example.greeter
        |
        |@main def run(): Unit = println(Greeter.greet("world"))
        |""".stripMargin,
    "src/main/scala/com/example/greeter/Greeter.scala" ->
      """package com.example.greeter
        |
        |object Greeter:
        |  def greet(name: String): String = s"Hello, $name!"
        |  def greetLoudly(name: String): String = greet(name).toUpperCase
        |""".stripMargin,
    "src/test/scala/com/example/greeter/GreeterSpec.scala" ->
      """package com.example.greeter
        |
        |class GreeterSpec extends munit.FunSuite:
        |  test("greets")(assertEquals(Greeter.greet("Ann"), "Hello, Ann!"))
        |""".stripMargin,
  )

  /** One sample: whether it passed, its tool calls with how each ended, and the turn. */
  final case class Sample(passed: Boolean, commands: List[(String, String)], answer: String, turn: IntellijSession.Turn):
    def metrics: Option[Json] = turn.metrics

  /** The IDE tool a call ran: the tool itself, or for a call of the router, the tool its `{"command": …}` names. */
  def routedTool(title: String, input: String): Option[String] =
    if title != RoutedTools.Router then Some(title)
    else input.fromJson[Json.Obj].toOption.flatMap(_.get("command")).flatMap(_.asString).flatMap(IntellijRouter.words(_).headOption)

  def spec = suite("IntelliJ IDEA evals")(
    test("typical IDE chat prompts, against IntelliJ's router-only MCP server (mocked, or real) and its skills") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
        ide     <- IntellijSession.ide
        root    <- ide.project.fold(SkillsSpec.tree(project*).map(_.toRealPath()))(ZIO.succeed(_))
        driver  <- IntellijSession.driver(ide)
        results <- ZIO.foreach(for t <- tasks; _ <- 1 to samples yield t): task =>
                     driver.turn(task.prompt, root, 5.minutes).flatMap: turn =>
                       val completed = turn.calls.filter(_.status == "completed")
                       val checks = List(
                         "answered" -> turn.ending.contains("answered"),
                         "answer" -> task.check(turn.answer),
                         "tools" -> (if task.noTools then turn.calls.isEmpty else !task.needsTools || completed.nonEmpty),
                         "right tool" -> task.tool.forall(t => completed.exists(c => routedTool(c.tool, c.input).contains(t))),
                       )
                       val sample = Sample(turn.error.isEmpty && checks.forall(_._2),
                         turn.calls.map(c => s"${c.tool} ${c.input}" -> c.status), turn.error.fold(turn.answer)(e => s"error: $e"), turn)
                       IntellijSession.record("intellij-prompts", task.name, ide, turn, checks).as(task -> sample)
        _       <- Console.printLine(report(results))
      yield assertTrue(results.nonEmpty)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK")
      @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID"),
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(60.minutes)

  private def report(results: List[(Prompt, Sample)]): String =
    val lines = results.flatMap: (task, s) =>
      def metric(name: String) = s.metrics.flatMap(m => field(m, name)).map(_.toJson).getOrElse("?")
      val tool = task.tool.fold("")(t => s", from a $t call")
      val head = s"${if s.passed then "PASS" else "FAIL"} ${task.name} — \"${task.prompt}\" (expects: ${task.expectation}$tool)"
      val stats = s"     ${s.commands.size} calls, ${s.commands.count(_._2 == "failed")} failed; " +
        s"jev ${metric("jevCalls")} (${metric("jevInputTokens")}/${metric("jevOutputTokens")} tokens), " +
        s"llm ${metric("llmCalls")} (${metric("llmInputTokens")}/${metric("llmOutputTokens")} tokens), ${metric("latencyMs")} ms, ending ${metric("ending")}"
      val calls = s.commands.map((input, status) => s"     $status: $input")
      head :: stats :: calls ++ List(s"     answer: ${s.answer.replace('\n', ' ').take(240)}")
    val passed = results.count(_._2.passed)
    val calls = results.map(_._2.commands.size).sum
    val failed = results.map(_._2.commands.count(_._2 == "failed")).sum
    (s"== IntelliJ IDEA evals: $passed/${results.size} passed; $calls tool calls, $failed failed" :: lines).mkString("\n")
