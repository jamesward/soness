package com.jamesward.zio_decisions.typesafe

import com.jamesward.zio_decisions.Decisions.Content
import com.jamesward.zio_decisions.internal.Canonical
import com.jamesward.zio_decisions.orchestration
import zio.*
import zio.http.{Body, Client, Header, MediaType}
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Jev's orchestration decisions, replayed in seconds: each fixture in `src/test/resources/jev-decisions/` is one Jev
 * request soness made in an eval run (captured with `scripts/capture-jev-decisions.py`), with one question and the
 * answer the run's own outputs show was right (did the draft's part pass, did a check show the work working, do the
 * failures share a cause). The spec sends each again, `SONESS_EVAL_SAMPLES` times (default 1), and reports accuracy
 * per kind of decision, with the questions that went wrong. A change to how a question is put, or to what its state
 * shows, can be measured here before an eval run. `SONESS_JEV_DECISIONS` picks kinds (default all). Needs
 * `TYPESAFE_API_KEY`. It's in zio-decisions' `typesafe` package to build requests in the wire's (canonical) shape; the
 * captured ones are sent to the System One API as they are.
 */
object JevDecisionEvalSpec extends ZIOSpecDefault:

  final case class Fixture(kind: String, question: String, expected: Boolean, body: Json.Obj) derives JsonDecoder

  private val fixtures: Task[List[Fixture]] =
    ZIO.attemptBlocking:
      val dir = Path.of(java.lang.System.getProperty("user.dir"), "src/test/resources/jev-decisions")
      Files.list(dir).iterator.asScala.filter(_.toString.endsWith(".jsonl")).toList.sorted.flatMap(p => Files.readAllLines(p).asScala)
        .filter(_.trim.nonEmpty).map(l => l.fromJson[Fixture].fold(e => throw IllegalStateException(e), identity))

  /**
   * The request as the current code would make it: a kind whose question the code builds from captured inputs
   * (`shared-cause`: from the run's output) is rebuilt with today's question, so a change to it is measured here;
   * `SONESS_JEV_DECISIONS_AS_CAPTURED=true` sends every request as it was captured.
   */
  private def current(f: Fixture, asCaptured: Boolean): Json.Obj =
    if asCaptured && f.kind != "shared-cause" then f.body
    else f.kind match
      case "shared-cause" =>
        // captured as the run's output and its parts: the question is today's
        val parts = f.body.get("parts").flatMap(_.asArray).toList.flatten.flatMap(_.asString)
        val run = f.body.get("run").flatMap(_.asString).getOrElse("")
        val (id, question) = orchestration.ParallelLoop.sharedCauseQuestion(parts, run)
        Canonical.request(new Content(Json.Str(f.body.get("state").flatMap(_.asString).getOrElse(""))),
          TypeSafe.JevLatest.unwrap, List(id -> question)).asObject.getOrElse(Json.Obj())
      case _ => f.body

  /** A noul's probability, or a choice's probability of anything but "none" / "wait". */
  private def yes(answer: Json): Option[Double] =
    val o = answer.asObject.getOrElse(Json.Obj())
    def num(j: Option[Json]) = j.flatMap(_.asNumber).map(_.value.doubleValue)
    o.get("type").flatMap(_.asString) match
      case Some("noul")   => num(o.get("noul"))
      case Some("choice") =>
        val choice = o.get("choice").flatMap(_.asString)
        Some(1.0 - num(o.get("probabilities").flatMap(_.asObject).flatMap(_.get("none")))
          .getOrElse(if choice.contains("none") then 1.0 else 0.0))
      case _ => None

  /** One captured request, sent to the System One API as it is: the answer to `question`. */
  private def send(client: Client, key: String, body: Json.Obj, question: String): Task[Option[Double]] =
    ZIO.scoped:
      client.url(TypeSafe.DefaultEndpoint).addHeader(Header.Authorization.Bearer(key))
        .addHeader(Header.ContentType(MediaType.application.json))
        .post(TypeSafe.Path)(Body.fromString(body.toJson))
        .flatMap(r => r.body.asString.flatMap: text =>
          if r.status.isSuccess then ZIO.fromEither(text.fromJson[Json.Obj]).mapError(RuntimeException(_))
          else ZIO.fail(RuntimeException(s"${r.status.code}: ${text.take(300)}")))
        .map(_.get("answers").flatMap(_.asObject).flatMap(_.get(question)).flatMap(yes))

  def spec = suite("Jev decisions, replayed")(
    test("each captured decision, against what its run showed") {
      for
        samples <- System.env("SONESS_EVAL_SAMPLES").map(_.flatMap(_.toIntOption).getOrElse(1))
        kinds   <- System.env("SONESS_JEV_DECISIONS").map(_.map(_.split(",").map(_.trim).toSet))
        raw     <- System.env("SONESS_JEV_DECISIONS_AS_CAPTURED").map(_.contains("true"))
        all     <- fixtures.map(_.filter(f => kinds.forall(_(f.kind))))
        client  <- ZIO.service[Client]
        key     <- System.env("TYPESAFE_API_KEY").someOrFailException
        started <- Clock.nanoTime
        results <- ZIO.foreachPar(all.flatMap(f => List.fill(samples)(f))): f =>
                     send(client, key, current(f, raw), f.question).either.map(f -> _)
                   .withParallelism(12)
        ended   <- Clock.nanoTime
        byKind   = results.groupBy(_._1.kind).toList.sortBy(_._1)
        _       <- ZIO.foreachDiscard(byKind): (kind, rs) =>
                     val judged = rs.collect { case (f, Right(Some(p))) => (f, p) }
                     val right = judged.count((f, p) => (p >= 0.5) == f.expected)
                     val falsePos = judged.count((f, p) => p >= 0.5 && !f.expected)
                     val falseNeg = judged.count((f, p) => p < 0.5 && f.expected)
                     Console.printLine(f"$kind%-14s $right%3d/${rs.size}%-3d right (wrongly yes $falsePos, wrongly no $falseNeg, errors ${rs.size - judged.size})")
        right    = results.count { case (f, Right(Some(p))) => (p >= 0.5) == f.expected; case _ => false }
        _       <- Console.printLine(s"jev decisions: $right/${results.size} in ${(ended - started) / 1000000} ms")
        _       <- ZIO.foreachDiscard(results.collect { case (f, Right(Some(p))) if (p >= 0.5) != f.expected => (f, p) }.distinctBy(_._1).take(6)): (f, p) =>
                     Console.printLine(f"  wrong: ${f.kind} expected ${f.expected} got $p%.2f")
      yield assertTrue(results.nonEmpty)
    } @@ TestAspect.ifEnvSet("TYPESAFE_API_KEY") @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(10.minutes),
  ).provideShared(Client.default)
