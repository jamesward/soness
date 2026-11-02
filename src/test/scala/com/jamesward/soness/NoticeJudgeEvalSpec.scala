package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.Notices
import zio.*
import zio.http.Client
import zio.test.*

/**
 * The decision model judging notices, live: labelled cases (a task at work, what it's doing now, and another task's
 * edit or failed check), each judged `SONESS_EVAL_SAMPLES` times (default 3) by the configured decision model (the
 * first of `DecisionModels.connectAll`: Jev, else OpenAI's). A notice that makes the work in flight wrong or out of date
 * should be judged urgent (at least `Notices.UrgentAt`), one that doesn't touch it not. Its accuracy, and how often it
 * would stop work for nothing (false urgent) or let stale work go on (missed), are what `Judge` and `Interrupt` are
 * worth over `Note`; its requests' cost is printed too. Needs `TYPESAFE_API_KEY` or `OPENAI_API_KEY`.
 */
object NoticeJudgeEvalSpec extends ZIOSpecDefault:

  private val add = "Add a new discount code, SPRING20, that takes 20% off at checkout."
  private val bug = "Customers say SAVE10 does nothing at checkout. Find out why and fix it."
  private val ship = "Make shipping free for orders over 75 instead of 50."

  private def edit(file: String, patch: String) = s"[other/task-1/main] edited /p/src/main/scala/shop/$file:\n$patch"

  private val discountsPatch =
    "--- a/Discounts.scala\n+++ b/Discounts.scala\n@@ -6,3 +6,4 @@\n   val active = Map(\n+    \"summer15\" -> Discount(\"summer15\", 15),\n     \"save10\" -> Discount(\"save10\", 10),\n"
  private val lookupPatch =
    "--- a/Discounts.scala\n+++ b/Discounts.scala\n@@ -12,1 +12,1 @@\n-  def lookup(code: String) = active.get(code.trim.toUpperCase)\n+  def lookup(code: String) = active.get(code.trim.toLowerCase)\n"
  private val shippingPatch =
    "--- a/Shipping.scala\n+++ b/Shipping.scala\n@@ -3,1 +3,1 @@\n-  val freeOver = 50\n+  val freeOver = 75\n"
  private val readmePatch = "--- a/README.md\n+++ b/README.md\n@@ -1,1 +1,2 @@\n # shop\n+Discount codes are case-insensitive.\n"

  private val D = "/p/src/main/scala/shop/Discounts.scala"
  private val S = "/p/src/main/scala/shop/Shipping.scala"

  /** A case: its name, the task's request, what it's doing now, the notice, whether it should be urgent, what the task's work touched. */
  private val cases0: List[(String, String, List[String], String, String, Boolean)] = List(
    ("writing an edit of the file another task just changed", add, List("writing the arguments of edit_file"),
      "edit", edit("Discounts.scala", discountsPatch), true),
    ("a test run of code another task just changed", add, List("calling run_command {\"command\":\"sbt test\"}"),
      "edit", edit("Discounts.scala", lookupPatch), true),
    ("another task did this task's own fix", bug, List("writing the arguments of edit_file"),
      "edit", edit("Discounts.scala", lookupPatch), true),
    ("about to read a file, another file changed", add, List("calling read_file {\"path\":\"src/main/scala/shop/Discounts.scala\"}"),
      "edit", edit("Shipping.scala", shippingPatch), false),
    ("between steps, a README line added", add, Nil, "edit", "[other/task-1/main] edited /p/README.md:\n" + readmePatch, false),
    ("writing a shipping change, a discount added", ship, List("writing the arguments of edit_file"),
      "edit", edit("Discounts.scala", discountsPatch), false),
    ("a test run, another task's check failed on its own new test", ship, List("calling run_command {\"command\":\"sbt test\"}"),
      "check failed", "[other/task-1] Run sbt test failed:\n[error] DiscountsSpec: SPRING20 takes 20% off: expected 80.0, got 100.0", false),
    ("writing an edit, the build now fails to compile in that file", add, List("writing the arguments of edit_file"),
      "check failed", "[other/task-1] Run sbt compile failed:\n[error] src/main/scala/shop/Discounts.scala:7:5: ')' expected", true),
  )
  /** What each case's task has read, edited or checked (its interests, as `Workspace.bind` adds them). */
  private val touched: List[Set[String]] = List(Set(D, "<build>"), Set(D, "<build>"), Set(D), Set(D), Set(D), Set(S, "<build>"),
    Set(S, "<build>"), Set(D, "<build>"))
  private val cases = cases0.zip(touched)

  /** Run only with a decision model's key (Jev's or OpenAI's). */
  private val keyed = if sys.env.contains("TYPESAFE_API_KEY") || sys.env.contains("OPENAI_API_KEY") then TestAspect.identity else TestAspect.ignore

  def spec = suite("notice judgments")(
    test("the decision model judges which notices make the work in flight out of date") {
      ZIO.scoped:
        for
          models  <- DecisionModels.connectAll().mapError(IllegalStateException(_))
          model   <- ZIO.fromOption(models.headOption).orElseFail(IllegalStateException("no decision model is configured"))
          samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
          judged  <- ZIO.foreachPar(cases)((c, works) =>
                       val (name, request, doing, kind, text, urgent) = c
                       ZIO.foreachPar((1 to samples).toList)(_ =>
                         Notices.judge(request, doing, Notices.Notice(0, "other/task-1/main", kind, Set.empty, text, java.time.Instant.EPOCH), works)
                           .provideEnvironment(model.model).timed)
                         .map(ps => (name, urgent, ps)))
          lines    = judged.map: (name, urgent, ps) =>
                       val right = ps.count((_, p) => (p >= Notices.UrgentAt) == urgent)
                       (right, ps.size, urgent, ps.count((_, p) => p >= Notices.UrgentAt),
                         f"${if right == ps.size then "PASS" else "FAIL"} $right/${ps.size} $name%-62s ${if urgent then "urgent" else "not   "} " +
                           s"[p ${ps.map((_, p) => f"$p%.2f").mkString(" ")}] ${ps.map(_._1.toMillis).sum / ps.size}ms")
          _       <- ZIO.foreachDiscard(lines)(l => Console.printLine(l._5))
          right    = lines.map(_._1).sum
          all      = lines.map(_._2).sum
          falseUrgent = lines.filterNot(_._3).map(_._4).sum
          missed   = lines.filter(_._3).map(l => l._2 - l._4).sum
          _       <- Console.printLine(s"notice judgments (${model.name}): $right/$all right; $falseUrgent false urgent, $missed missed")
        yield assertTrue(right * 10 >= all * 7)
    } @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes) @@ keyed,
  ).provideShared(Client.default)
