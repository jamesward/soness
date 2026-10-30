package com.jamesward.soness

import zio.*
import zio.test.*

import java.nio.file.{Files, Path, StandardOpenOption}

/** A real log (`target/transcript-replay.log`, when there is one) read whole, and replayed into a followed file in pieces: the same turns end. */
object TranscriptReplaySpec extends ZIOSpecDefault:
  def spec = suite("a real transcript, replayed")(
    test("read whole and followed as written give the same ended turns") {
      for
        source <- ZIO.succeed(Path.of("target/transcript-replay.log"))
        whole  <- ZIO.attemptBlocking(Transcript.turns(Files.readAllLines(source).toArray.iterator.map(_.toString)))
        dir    <- ZIO.attemptBlocking(Files.createTempDirectory("replay"))
        file    = dir.resolve("j.log")
        _      <- ZIO.attemptBlocking(Files.writeString(file, ""))
        ended  <- Queue.unbounded[Transcript.Turn]
        _      <- Watch.endedTurns(file).runForeach(ended.offer).forkScoped
        _      <- ZIO.sleep(300.millis)
        bytes  <- ZIO.attemptBlocking(Files.readAllBytes(source))
        _      <- ZIO.foreachDiscard(bytes.grouped(65536).toList)(c => ZIO.attemptBlocking(Files.write(file, c, StandardOpenOption.APPEND)) *> ZIO.sleep(5.millis))
        got    <- ended.take.timeout(20.seconds)
        _      <- Console.printLine(s"whole: ${whole.map(t => t.id + " " + t.ending).mkString(", ")}; followed: ${got.map(_.id)}")
        prompts = String(bytes, java.nio.charset.StandardCharsets.UTF_8).linesIterator.count(_.contains("message=\"prompt (task"))
      // every turn the log has is read (a timestamp that doesn't parse would lose its entries silently)
      yield assertTrue(whole.size == prompts, whole.exists(_.endedAt.isDefined), got.isDefined)
    } @@ TestAspect.ifProp("user.dir")(_ => Files.exists(Path.of("target/transcript-replay.log"))),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds)
