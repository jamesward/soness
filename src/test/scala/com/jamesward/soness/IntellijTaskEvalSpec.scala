package com.jamesward.soness

import com.jamesward.soness.IntellijSession.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Live evals of real-world IDE tasks: a user's request, in their words, that takes several steps (finding the code,
 * reading it, changing it), run through soness as IntelliJ IDEA runs it ([[IntellijSession]]), checked by what's on
 * disk afterwards and by the answer, and measured: Jev and LLM calls and tokens, tool calls (and failed ones) and
 * latency, printed and appended to `target/evals/intellij-tasks.jsonl` (with `SONESS_EVAL_LABEL`, to compare
 * orchestration changes). Each sample starts from a fresh copy of the task's project; with the real IDE, its project
 * (`SONESS_IJ_PROJECT`, opened from a copy written by `WriteProject`) is reset through the IDE's own tools.
 * Each task runs through each arm in `SONESS_EVAL_ARMS` (default `soness,kiro,claude`): harness against harness,
 * each an ACP agent given the same IntelliJ session ([[IntellijArms]]). `SONESS_EVAL_SAMPLES` (default 1) sets the
 * samples per arm. Needs `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and
 * `BEDROCK_MODEL_ID`; it calls paid APIs and, with the real IDE, edits its project.
 */
object IntellijTaskEvalSpec extends ZIOSpecDefault:

  /** A task: the user's request, its project, and the checks on the project and the answer after the turn. */
  final case class UserTask(name: String, prompt: String, project: Seq[(String, String)], checks: (Path, Turn) => List[(String, Boolean)])

  /** A small web shop's checkout. Discount codes are stored lowercase but looked up uppercased, so none ever applies. */
  val shop: Seq[(String, String)] = Seq(
    "build.sbt" ->
      """name := "shop"
        |
        |scalaVersion := "3.7.2"
        |
        |libraryDependencies += "org.scalameta" %% "munit" % "1.1.1" % Test
        |""".stripMargin,
    "README.md" ->
      """# shop
        |
        |The checkout of our web shop. Customers can enter a discount code (SAVE10, WELCOME5) or FREESHIP at checkout.
        |""".stripMargin,
    "src/main/scala/shop/Model.scala" ->
      """package shop
        |
        |final case class Line(sku: String, unitPrice: BigDecimal, quantity: Int)
        |final case class Order(id: String, customerId: String, lines: List[Line])
        |final case class Discount(code: String, percent: Int)
        |final case class Receipt(orderId: String, subtotal: BigDecimal, discount: BigDecimal, shipping: BigDecimal, total: BigDecimal)
        |""".stripMargin,
    "src/main/scala/shop/Pricing.scala" ->
      """package shop
        |
        |object Pricing:
        |  def subtotal(order: Order): BigDecimal = order.lines.map(l => l.unitPrice * l.quantity).sum
        |
        |  /** The amount a discount takes off a subtotal, rounded to the cent. */
        |  def discountAmount(subtotal: BigDecimal, discount: Option[Discount]): BigDecimal =
        |    discount.fold(BigDecimal(0))(d => (subtotal * d.percent / 100).setScale(2, BigDecimal.RoundingMode.HALF_UP))
        |""".stripMargin,
    "src/main/scala/shop/Shipping.scala" ->
      """package shop
        |
        |object Shipping:
        |  val FreeAbove: BigDecimal = BigDecimal(50)
        |  val Flat: BigDecimal = BigDecimal("4.99")
        |
        |  def cost(subtotal: BigDecimal, code: Option[String]): BigDecimal =
        |    if subtotal >= FreeAbove || code.exists(_.equalsIgnoreCase("FREESHIP")) then BigDecimal(0) else Flat
        |""".stripMargin,
    "src/main/scala/shop/Discounts.scala" ->
      """package shop
        |
        |/** The discount codes customers can enter at checkout. */
        |object Discounts:
        |  private val active: Map[String, Discount] = Map(
        |    "save10" -> Discount("save10", 10),
        |    "welcome5" -> Discount("welcome5", 5),
        |  )
        |
        |  /** The discount for a code as the customer typed it, if it is active. */
        |  def lookup(code: String): Option[Discount] = active.get(code.trim.toUpperCase)
        |""".stripMargin,
    "src/main/scala/shop/Checkout.scala" ->
      """package shop
        |
        |/** Turns an order and the code the customer entered into a receipt. */
        |object Checkout:
        |  def receipt(order: Order, code: Option[String]): Receipt =
        |    val subtotal = Pricing.subtotal(order)
        |    val discount = code.flatMap(Discounts.lookup)
        |    val off = Pricing.discountAmount(subtotal, discount)
        |    val shipping = Shipping.cost(subtotal - off, code)
        |    Receipt(order.id, subtotal, off, shipping, subtotal - off + shipping)
        |""".stripMargin,
    "src/main/scala/shop/Main.scala" ->
      """package shop
        |
        |@main def demo(code: String*): Unit =
        |  val order = Order("o1", "c1", List(Line("mug", BigDecimal(12), 2), Line("tea", BigDecimal("7.50"), 1)))
        |  println(Checkout.receipt(order, code.headOption))
        |""".stripMargin,
    "src/test/scala/shop/CheckoutSpec.scala" ->
      """package shop
        |
        |class CheckoutSpec extends munit.FunSuite:
        |  private val order = Order("o1", "c1", List(Line("mug", BigDecimal(12), 2)))
        |
        |  test("no code: full price plus shipping"):
        |    assertEquals(Checkout.receipt(order, None).total, BigDecimal("28.99"))
        |
        |  test("free shipping code"):
        |    assertEquals(Checkout.receipt(order, Some("freeship")).total, BigDecimal(24))
        |""".stripMargin,
  )

  private def read(root: Path, file: String): String =
    val path = root.resolve(file)
    if Files.isRegularFile(path) then Files.readString(path) else ""

  /**
   * Whether `Discounts.scala` finds each of `codes` whatever its case, with its percentage: the map's keys and the
   * lookup's normalization agree, or the lookup ignores case.
   */
  def codesWork(source: String, codes: Map[String, Int]): Boolean =
    val keys = """"(\w+)"\s*->\s*Discount""".r.findAllMatchIn(source).map(_.group(1)).toList
    val lookup = source.drop(source.indexOf("def lookup").max(0))
    val lowers = lookup.contains("toLowerCase")
    val uppers = lookup.contains("toUpperCase")
    val agree =
      lookup.contains("equalsIgnoreCase") ||
        (keys.nonEmpty && keys.forall(k => k == k.toLowerCase) && lowers && !uppers) ||
        (keys.nonEmpty && keys.forall(k => k == k.toUpperCase) && uppers && !lowers)
    val percents = codes.forall: (code, percent) =>
      s"""Discount\\("(?i:$code)",\\s*$percent\\)""".r.findFirstIn(source).isDefined &&
        keys.exists(_.equalsIgnoreCase(code))
    agree && percents

  /** Whether `Discounts.scala` now finds SAVE10 and WELCOME5 whatever their case. */
  def discountsFixed(source: String): Boolean = codesWork(source, Map("save10" -> 10, "welcome5" -> 5))

  /** The shop with its discount bug fixed: the codes are looked up lowercased. */
  val shopFixed: Seq[(String, String)] = shop.map: (file, text) =>
    file -> (if file.endsWith("Discounts.scala") then text.replace("code.trim.toUpperCase", "code.trim.toLowerCase") else text)

  /** The source files a fix of the discount codes has no reason to change. */
  private val untouched = List("Model", "Pricing", "Shipping", "Checkout", "Main").map(n => s"src/main/scala/shop/$n.scala")

  private def unchanged(root: Path, project: Seq[(String, String)], files: List[String]): Boolean =
    files.forall(f => read(root, f) == project.toMap.apply(f))

  val tasks: List[UserTask] = List(
    UserTask(
      "fix a bug from a user's report",
      "Customers are telling us the SAVE10 code doesn't do anything at checkout, they still pay full price. " +
        "Can you find out why and fix it?",
      shop,
      (root, turn) => List(
        "the turn ended with an answer" -> turn.ending.contains("answered"),
        "Discounts.scala finds codes whatever their case" -> discountsFixed(read(root, "src/main/scala/shop/Discounts.scala")),
        "the other sources are unchanged" -> unchanged(root, shop, untouched),
        "the answer names the case mismatch" ->
          "(?i)upper[- ]?case|lower[- ]?case|case[- ]?(in)?sensitiv|toUpperCase|toLowerCase".r.findFirstIn(turn.answer).isDefined,
      ),
    ),
    UserTask(
      "answer a question about the code",
      "When does a customer get free shipping at checkout?",
      shopFixed,
      (root, turn) => List(
        "the turn ended with an answer" -> turn.ending.contains("answered"),
        "nothing changed" -> shopFixed.forall((f, t) => read(root, f) == t),
        "the answer gives the threshold" -> """\b50\b""".r.findFirstIn(turn.answer).isDefined,
        "the answer names the FREESHIP code" -> turn.answer.toLowerCase.contains("freeship"),
      ),
    ),
    UserTask(
      "add a feature",
      "Add a new discount code, SPRING20, that takes 20% off at checkout.",
      shopFixed,
      (root, turn) => List(
        "the turn ended with an answer" -> turn.ending.contains("answered"),
        "Discounts.scala finds SPRING20 and the old codes whatever their case" ->
          codesWork(read(root, "src/main/scala/shop/Discounts.scala"), Map("save10" -> 10, "welcome5" -> 5, "spring20" -> 20)),
        "the other sources are unchanged" -> unchanged(root, shopFixed, untouched),
        "the answer names SPRING20" -> turn.answer.toUpperCase.contains("SPRING20"),
      ),
    ),
  )

  /**
   * A fresh copy of a task's project for one sample: a new directory for the mock; for the real IDE, its project
   * rewritten through its own tools (so the IDE sees the files as they are), and source files the task doesn't have
   * deleted.
   */
  private def fresh(task: UserTask, ide: Ide): ZIO[Client & Scope, Throwable, Path] =
    ide.project match
      case None => SkillsSpec.tree(task.project*).map(_.toRealPath())
      case Some(root) =>
        for
          tools  <- McpToolset.connect(List(McpEndpoint.Stdio("jetbrains_air_ide", ide.idea.get, List("stdioMcpServer"),
                      Map("IJ_MCP_SERVER_PORT" -> ide.port.get, "IJ_MCP_SERVER_PROJECT_PATH" -> root.toString)))).map(_.toolset)
          extra  <- ZIO.attemptBlocking:
                      val src = root.resolve("src")
                      if !Files.isDirectory(src) then Nil
                      else Files.walk(src).iterator.asScala.filter(Files.isRegularFile(_)).map(root.relativize(_).toString)
                        .filterNot(task.project.toMap.contains).toList
          _      <- ZIO.foreachDiscard(extra): file =>
                      tools.call("apply_patch", Json.Obj("input" -> Json.Str(s"*** Begin Patch\n*** Delete File: $file\n*** End Patch")))
          _      <- ZIO.foreachDiscard(task.project): (file, text) =>
                      tools.call("create_new_file", Json.Obj("pathInProject" -> Json.Str(file), "text" -> Json.Str(text),
                        "overwrite" -> Json.Bool(true)))
          same   <- ZIO.attemptBlocking(task.project.forall((file, text) => read(root, file) == text))
          _      <- ZIO.fail(IllegalStateException(s"couldn't reset $root through the IDE")).unless(same)
        yield root

  def spec = suite("IntelliJ IDEA task evals")(
    test("the discount check accepts the fixes and rejects the bug") {
      val bug = shop.toMap.apply("src/main/scala/shop/Discounts.scala")
      assertTrue(
        !discountsFixed(bug),
        discountsFixed(bug.replace("toUpperCase", "toLowerCase")),
        discountsFixed(bug.replace("\"save10\" ->", "\"SAVE10\" ->").replace("\"welcome5\" ->", "\"WELCOME5\" ->")),
        discountsFixed(bug.replace("active.get(code.trim.toUpperCase)", "active.collectFirst { case (k, d) if k.equalsIgnoreCase(code.trim) => d }")),
        !discountsFixed(bug.replace("toUpperCase", "toLowerCase").replace("Discount(\"save10\", 10)", "Discount(\"save10\", 100)")),
        !discountsFixed(bug.replace("\"save10\" ->", "\"SAVE10\" ->")),
        discountsFixed(shopFixed.toMap.apply("src/main/scala/shop/Discounts.scala")),
        !codesWork(shopFixed.toMap.apply("src/main/scala/shop/Discounts.scala"), Map("spring20" -> 20)),
        codesWork(shopFixed.toMap.apply("src/main/scala/shop/Discounts.scala")
          .replace("    \"welcome5\"", "    \"spring20\" -> Discount(\"spring20\", 20),\n    \"welcome5\""), Map("spring20" -> 20, "save10" -> 10)),
      )
    },
    test("real-world tasks, measured") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
        names   <- System.env("SONESS_EVAL_ARMS").map(_.getOrElse(IntellijArms.Names.mkString(",")).split(",").toList.map(_.trim).filter(_.nonEmpty))
        arms    <- ZIO.foreach(names)(IntellijArms.arm(_))
        ide     <- IntellijSession.ide
        _       <- Console.printLine(s"IDE MCP server: ${ide.describe}; arms: ${arms.map(a => s"${a.name} (${if a.model.isEmpty then "default model" else a.model})").mkString(", ")}")
        results <- ZIO.foreach(for t <- tasks; a <- arms; n <- 1 to samples yield (t, a, n)): (task, a, n) =>
                     ZIO.scoped:
                       val arm = a.name
                       for
                         root   <- fresh(task, ide)
                         turn   <- IntellijArms.turn(a, task.prompt, ide, root)
                         checks  = task.checks(root, turn)
                         diff   <- ZIO.attemptBlocking(task.project.collect { case (f, t) if read(root, f) != t => f })
                         _      <- IntellijSession.record("intellij-tasks", task.name, ide, turn, checks, arm)
                         _      <- Console.printLine(report(task, s"$arm #$n", turn, checks, diff))
                       yield (task, arm, turn, checks)
        _       <- ZIO.foreachDiscard(names): arm =>
                     Console.printLine(summary(arm, results.collect { case (t, `arm`, turn, checks) => (t, turn, checks) }))
      yield assertTrue(results.nonEmpty)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK")
      @@ TestAspect.ifEnvSet("BEDROCK_MODEL_ID"),
  ).provide(Client.default) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(120.minutes)

  private def report(task: UserTask, n: String, turn: Turn, checks: List[(String, Boolean)], changed: Seq[String]): String =
    def m(name: String) = turn.metric(name).map(j => j.asString.getOrElse(j.toString)).getOrElse("?")
    val passed = turn.error.isEmpty && checks.forall(_._2)
    (List(
      s"${if passed then "PASS" else "FAIL"} ${task.name} [$n] — \"${task.prompt}\"",
      s"     ending ${m("ending")}, ${m("latencyMs")} ms (jev ${m("jevLatencyMs")}, llm ${m("llmLatencyMs")}, tools ${m("toolLatencyMs")}); tool calls ${turn.calls.size} (${turn.failedCalls} failed); " +
        s"jev ${m("jevCalls")} calls, ${m("jevInputTokens")} in / ${m("jevOutputTokens")} out tokens; " +
        s"llm ${m("llmCalls")} calls, ${m("llmInputTokens")} in / ${m("llmOutputTokens")} out tokens" +
        turn.metric("costUsd").fold("")(c => s", $$$c"),
    ) ++ checks.map((name, ok) => s"     ${if ok then "✓" else "✗"} $name") ++
      turn.calls.zipWithIndex.map((c, i) => s"     ${i + 1}. ${c.status} ${c.tool} ${c.input.take(160)}${
        if c.tool == "apply_patch" || c.status == "failed" then s" → ${c.output.replace('\n', ' ').take(120)}" else ""}") ++
      List(s"     changed: ${if changed.isEmpty then "nothing" else changed.mkString(", ")}") ++
      turn.error.map(e => s"     error: $e") ++
      List(s"     answer: ${turn.answer.replace('\n', ' ').take(300)}")).mkString("\n")

  private def summary(arm: String, results: List[(UserTask, Turn, List[(String, Boolean)])]): String =
    val turns = results.map(_._2)
    val passed = results.count((_, t, checks) => t.error.isEmpty && checks.forall(_._2))
    List(
      s"== IntelliJ IDEA task evals, $arm: $passed/${results.size} passed",
      s"   per turn, mean (min–max): latencyMs ${IntellijSession.summary(turns, "latencyMs")} (jev ${IntellijSession.summary(turns, "jevLatencyMs")}, " +
        s"llm ${IntellijSession.summary(turns, "llmLatencyMs")}, tools ${IntellijSession.summary(turns, "toolLatencyMs")}), " +
        s"toolCalls ${IntellijSession.summary(turns, "toolCalls")}, failed ${turns.map(_.failedCalls).sum}",
      s"   jev: calls ${IntellijSession.summary(turns, "jevCalls")}, in ${IntellijSession.summary(turns, "jevInputTokens")}, " +
        s"out ${IntellijSession.summary(turns, "jevOutputTokens")}",
      s"   llm: calls ${IntellijSession.summary(turns, "llmCalls")}, in ${IntellijSession.summary(turns, "llmInputTokens")}, " +
        s"out ${IntellijSession.summary(turns, "llmOutputTokens")}",
    ).mkString("\n")

/** Writes a task's project to a directory, for a headless IDE to open: `Test/runMain …WriteProject <dir>`. */
object WriteProject extends ZIOAppDefault:
  def run =
    for
      args <- getArgs
      dir  <- ZIO.fromOption(args.headOption).orElseFail(IllegalArgumentException("usage: WriteProject <dir>")).map(Path.of(_))
      _    <- ZIO.attemptBlocking:
                IntellijTaskEvalSpec.shop.foreach: (file, text) =>
                  Files.createDirectories(dir.resolve(file).getParent)
                  Files.writeString(dir.resolve(file), text)
    yield ()
