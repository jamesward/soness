package com.jamesward.soness

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files

/**
 * `list_files` and a build-output directory: skipped by a glob that doesn't name it, listed by one that does (a run
 * couldn't find its own debug log, `target/logs/soness-*.log`, because every listing skipped `target`).
 */
object LocalToolsListSkippedSpec extends ZIOSpecDefault:

  def spec = suite("list_files in a skipped directory")(
    test("a glob naming a build-output directory lists its files; other globs still skip it") {
      for
        root  <- ZIO.attemptBlocking:
                   val r = Files.createTempDirectory("soness-list")
                   Files.createDirectories(r.resolve("target/logs"))
                   Files.writeString(r.resolve("target/logs/soness-goal.log"), "log")
                   Files.createDirectories(r.resolve("src"))
                   Files.writeString(r.resolve("src/a.log"), "src")
                   r
        tools  = LocalTools.toolset(root)
        named <- tools.call("list_files", Json.Obj("glob" -> Json.Str("target/logs/*.log")))
        any   <- tools.call("list_files", Json.Obj("glob" -> Json.Str("**/*.log")))
      yield assertTrue(
        named.toJson.contains("target/logs/soness-goal.log"),
        any.toJson.contains("src/a.log"),
        !any.toJson.contains("target/logs"),
      )
    }
  )
