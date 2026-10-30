package com.jamesward.soness

import zio.*
import zio.test.*

import java.nio.file.Path

object ProjectContextSpec extends ZIOSpecDefault:

  def spec = suite("ProjectContext")(
    test("lists the project's files, build and languages, leaving out VCS, IDE and build output, with its AGENTS.md") {
      for
        root    <- SkillsSpec.tree(
                     "build.sbt" -> "scalaVersion := \"3.7.2\"",
                     "AGENTS.md" -> "Run ./sbt test before you finish.",
                     "src/main/scala/shop/Discounts.scala" -> "object Discounts",
                     "src/test/scala/shop/DiscountsSpec.scala" -> "class DiscountsSpec",
                     "web/app.ts" -> "",
                     "target/scala-3.7.2/classes/X.class" -> "",
                     ".idea/workspace.xml" -> "",
                     ".git/HEAD" -> "",
                     "node_modules/x/index.js" -> "",
                   )
        context <- ProjectContext.describe(root).someOrFail(IllegalStateException("no context"))
      yield assertTrue(
        context.contains("Build: sbt (Scala) (build.sbt)"),
        context.contains("Languages: Scala (2 files), TypeScript (1 files)"),
        context.contains("src/main/scala/shop/Discounts.scala"),
        context.contains("web/app.ts"),
        !context.contains("X.class"), !context.contains("workspace.xml"), !context.contains("HEAD"), !context.contains("index.js"),
        context.contains("The project's agent instructions (AGENTS.md):\nRun ./sbt test before you finish."),
      )
    },
    test("a large project's listing is cut, saying how many files are left out") {
      for
        root    <- SkillsSpec.tree((1 to ProjectContext.MaxFiles + 5).map(i => f"src/F$i%04d.java" -> "")*)
        context <- ProjectContext.describe(root).someOrFail(IllegalStateException("no context"))
      yield assertTrue(context.contains("… and 5 more files"), context.contains("Java (305 files)"))
    },
    test("a directory that can't be read gives no context") {
      ProjectContext.describe(Path.of("/nonexistent/soness")).map(c => assertTrue(c.isEmpty))
    },
  )
