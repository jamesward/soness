package com.jamesward.soness

import zio.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}

/**
 * Source dependencies (sibling checkouts a build compiles from source) are part of the work: named in the project's
 * context, and reachable by soness's file tools (with an IDE's, by [[LocalTools.reaching]], `LocalToolsSpec`).
 */
object SourceDependenciesSpec extends ZIOSpecDefault:

  private def layout =
    ZIO.attemptBlocking:
      val top = Files.createTempDirectory("deps")
      val app = Files.createDirectories(top.resolve("app"))
      val lib = Files.createDirectories(top.resolve("lib").resolve("src"))
      Files.writeString(lib.resolve("Lib.scala"), "object Lib:\n  val x = 1\n")
      Files.writeString(app.resolve("App.scala"), "object App\n")
      Files.writeString(app.resolve("build.sbt"),
        "lazy val lib = RootProject(file(\"../lib\"))\nlazy val gone = ProjectRef(file(\"../missing\"), \"m\")\n")
      (top, app, top.resolve("lib"))

  def spec = suite("source dependencies")(
    test("sbt's RootProject and ProjectRef, and Gradle's includeBuild, name them") {
      assertTrue(
        SourceDependencies.named("lazy val a = RootProject(file(\"../a\"))\nlazy val b = ProjectRef( file( \"../b\" ), \"core\")") == List("../a", "../b"),
        SourceDependencies.named("includeBuild(\"../c\")\nincludeBuild '../d'") == List("../c", "../d"),
        SourceDependencies.named("libraryDependencies += \"x\" %% \"y\" % \"1\"").isEmpty,
      )
    },
    test("a project's existing source dependencies, resolved; they're in its context") {
      for
        (_, app, lib) <- layout
        found          = SourceDependencies.of(app)
        described     <- ProjectContext.describe(app)
      yield assertTrue(found.map(_.toString) == List(lib.normalize.toString), described.exists(_.contains("Source dependencies")),
        described.exists(_.contains(lib.normalize.toString)))
    },
    test("without an IDE's file tools: the file tools reach the source dependencies too; nothing else outside the project") {
      for
        (top, app, lib) <- layout
        tools            = LocalTools.toolset(app, SourceDependencies.of(app))
        edited          <- tools.call("edit_file", Json.Obj("path" -> Json.Str(lib.resolve("src/Lib.scala").toString),
                             "old_text" -> Json.Str("val x = 1"), "new_text" -> Json.Str("val x = 2")))
        relative        <- tools.call("read_file", Json.Obj("path" -> Json.Str("../lib/src/Lib.scala")))
        listed          <- tools.call("list_files", Json.Obj("glob" -> Json.Str("**/*.scala"), "directory" -> Json.Str(lib.toString)))
        outside         <- tools.call("read_file", Json.Obj("path" -> Json.Str(top.resolve("elsewhere.txt").toString))).either
        text            <- ZIO.attemptBlocking(Files.readString(lib.resolve("src/Lib.scala")))
      yield assertTrue(text.contains("val x = 2"), relative.toString.contains("val x = 2"), listed.toString.contains(lib.resolve("src/Lib.scala").toString),
        edited.toString.contains(lib.toString), outside.left.exists(_.getMessage.contains("outside the project and its source dependencies")))
    },
  )
