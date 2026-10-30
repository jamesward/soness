package com.jamesward.soness

import com.jamesward.zio_bedrock.Bedrock
import com.jamesward.zio_bedrock.Bedrock.*
import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}
import zio.test.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Reproducers of strange LLM behavior met in the evals: each fixture in `src/test/resources/llm-behaviors/` is one
 * Bedrock exchange soness had (captured from a debug log with `scripts/capture-llm-behavior.py`): the model, the exact
 * request (system prompt, messages, inference config, the tool its arguments are for) and what the model answered. The
 * spec sends the request again, as soness sends it (zio-bedrock's `asJsonObject`, on Mantle a forced tool call),
 * `SONESS_LLM_BEHAVIOR_SAMPLES` times (default 3), and applies the fixture's checks to each answer. It reports how often
 * the behavior happened and fails while it still does, so a prompt or protocol change that fixes it shows here in
 * seconds rather than in a 10-minute eval. `SONESS_LLM_BEHAVIORS` picks fixtures by name (default all); a fixture's
 * model can be replaced with `SONESS_LLM_BEHAVIOR_MODEL`, to see whether another model behaves the same.
 *
 * Checks, by name:
 *   - `calls-the-tool`: the model gave arguments (a call, or arguments zio-bedrock accepts as text), not prose
 *   - `single-patch-format`: a patch is either Codex's (`*** Begin Patch` with `*** Update/Add/Delete File:` headers)
 *     or a unified diff (`---`/`+++`), not both mixed
 *   - `differs-from-rejected`: the arguments aren't any the request listed as already rejected
 *
 * Needs `AWS_BEARER_TOKEN_BEDROCK` and `SONESS_BEDROCK_API=mantle` (the fixtures are Mantle exchanges); calls Bedrock.
 */
object LlmBehaviorSpec extends ZIOSpecDefault:

  final case class ToolSpec(name: String, description: String, schema: Json.Obj) derives JsonDecoder
  final case class Captured(system: String, messages: List[String], maxTokens: Option[Int], temperature: Option[Double], tool: ToolSpec)
      derives JsonDecoder
  final case class Fixture(name: String, description: String, model: String, checks: List[String], request: Captured) derives JsonDecoder

  /** One answer: the arguments, or why there were none. */
  type Answer = Either[String, Json.Obj]

  def check(name: String, request: Captured, answer: Answer): Either[String, Unit] =
    name match
      case "calls-the-tool" => answer.map(_ => ()).left.map(e => s"no arguments: ${e.take(200)}")
      case "single-patch-format" =>
        answer.toOption.flatMap(a => a.get("patch").orElse(a.get("input")).flatMap(_.asString)) match
          case None => Right(())
          case Some(patch) =>
            val codex = patch.contains("*** Begin Patch")
            val codexHeaders = """(?m)^\*\*\* (Update|Add|Delete) File: """.r.findFirstIn(patch).isDefined
            val unified = """(?m)^(---|\+\+\+) """.r.findFirstIn(patch).isDefined
            if codex && unified then Left(s"Codex and unified formats mixed: ${patch.take(160).replace('\n', '⏎')}")
            else if codex && !codexHeaders then Left(s"a Codex patch without file headers: ${patch.take(160).replace('\n', '⏎')}")
            else Right(())
      case "differs-from-rejected" =>
        val rejected = request.messages.mkString("\n").linesIterator.dropWhile(!_.contains("which repeat a failed call"))
          .drop(1).takeWhile(_.startsWith("- ")).map(_.drop(2).trim).flatMap(_.fromJson[Json.Obj].toOption).map(_.toJson).toSet
        answer.toOption.filter(a => rejected(a.toJson)).fold(Right(()))(_ => Left("repeats arguments the request said were rejected"))
      case other => Left(s"unknown check $other")

  /** The request again, as soness sends it. */
  private def replay(fixture: Fixture, bedrock: ZEnvironment[Bedrock]): UIO[Answer] =
    val r = fixture.request
    Bedrock.chat(RequestConfig(
      messages = r.messages.map(Message.user),
      system = r.system,
      inferenceConfig = InferenceConfig(maxTokens = r.maxTokens.getOrElse(16384), temperature = r.temperature.getOrElse(0.0)),
    )).asJsonObject(r.tool.schema, BedrockLlmTransport.functionName(r.tool.name), Some(r.tool.description))
      .map(_.output).provideEnvironment(bedrock).either.map(_.left.map(_.getMessage))

  private val fixtures: Task[List[Fixture]] =
    AcpSubprocess.property("user.dir").flatMap: dir =>
      ZIO.attemptBlocking(Files.list(Path.of(dir, "src/test/resources/llm-behaviors")).iterator.asScala.filter(_.toString.endsWith(".json")).toList.sorted)
        .flatMap(ZIO.foreach(_)(p => ZStream.fromPath(p).via(ZPipeline.utf8Decode).runCollect.map(_.mkString)
          .flatMap(t => ZIO.fromEither(t.fromJson[Fixture]).mapError(e => IllegalStateException(s"$p: $e")))))

  def spec = suite("LLM behavior reproducers")(
    test("the checks recognize the captured behaviors") {
      val request = Captured("", List("…You just wrote these, which repeat a failed call; don't write them again:\n- {\"rebuild\":true}\n"), None, None,
        ToolSpec("t", "", Json.Obj()))
      val mixed = Json.Obj("patch" -> Json.Str("*** Begin Patch\n--- a/Main.java\n+++ b/Main.java\n@@ -1 +1 @@\n-a\n+b\n*** End Patch"))
      val codex = Json.Obj("patch" -> Json.Str("*** Begin Patch\n*** Update File: Main.java\n@@\n-a\n+b\n*** End Patch"))
      val unified = Json.Obj("input" -> Json.Str("--- a/Main.java\n+++ b/Main.java\n@@ -1 +1 @@\n-a\n+b\n"))
      assertTrue(
        check("single-patch-format", request, Right(mixed)).isLeft,
        check("single-patch-format", request, Right(codex)).isRight,
        check("single-patch-format", request, Right(unified)).isRight,
        check("calls-the-tool", request, Left("Expected one tool use for t, but the model returned none")).isLeft,
        check("differs-from-rejected", request, Right(Json.Obj("rebuild" -> Json.Bool(true)))).isLeft,
        check("differs-from-rejected", request, Right(Json.Obj("rebuild" -> Json.Bool(false)))).isRight,
      )
    },
    test("each captured behavior, replayed against its model") {
      for
        samples  <- System.env("SONESS_LLM_BEHAVIOR_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(3))
        which    <- System.env("SONESS_LLM_BEHAVIORS").map(_.map(_.split(",").map(_.trim).toSet))
        model    <- System.env("SONESS_LLM_BEHAVIOR_MODEL")
        access   <- Soness.bedrockAccess.mapError(IllegalStateException(_))
        all      <- fixtures
        chosen    = all.filter(f => which.forall(_(f.name)))
        results  <- ZIO.foreach(chosen): fixture =>
                      val id = model.getOrElse(fixture.model)
                      for
                        bedrock <- Soness.connect(access, id).mapError(IllegalStateException(_))
                        answers <- ZIO.foreachPar(1 to samples)(_ => replay(fixture, bedrock)).withParallelism(3)
                        failures = answers.toList.map(a => fixture.checks.flatMap(c => check(c, fixture.request, a).left.toOption.map(e => s"$c: $e")))
                        _       <- Console.printLine(
                                     s"${fixture.name} [$id]: ${failures.count(_.nonEmpty)}/$samples show the behavior — ${fixture.description}\n" +
                                       failures.zipWithIndex.collect { case (f, i) if f.nonEmpty => s"   #${i + 1}: ${f.mkString("; ")}" }.mkString("\n"))
                      yield fixture.name -> failures.count(_.nonEmpty)
      yield assertTrue(results.nonEmpty, results.forall(_._2 == 0))
    } @@ TestAspect.ifEnvSet("AWS_BEARER_TOKEN_BEDROCK"),
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(30.minutes)
