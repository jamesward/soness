package com.jamesward.soness

import zio.*
import zio.http.Client
import zio.json.*
import zio.json.ast.Json
import zio.stream.{ZPipeline, ZStream}
import zio.test.*

import scala.jdk.CollectionConverters.*

import java.nio.file.{Files, Path}

/**
 * The mock of IntelliJ IDEA's MCP server answers as the real one: every `execute_tool` exchange captured from IntelliJ
 * IDEA 2026.2.3 (`intellij-mcp-router-2026.2.3.jsonl`, from soness's debug logs of real sessions) gives the same result
 * or error, over a project shaped like the one captured; and over stdio it lists the same tool and identifies itself the
 * same way.
 */
object IntellijRouterSpec extends ZIOSpecDefault:

  final case class Exchange(command: String, kind: String, text: String) derives JsonDecoder

  private val captured: Task[List[Exchange]] =
    ZStream.fromResource("intellij-mcp-router-2026.2.3.jsonl").via(ZPipeline.utf8Decode >>> ZPipeline.splitLines)
      .filter(_.nonEmpty).mapZIO(line => ZIO.fromEither(line.fromJson[Exchange]).mapError(RuntimeException(_))).runCollect.map(_.toList)

  /** A project shaped as the captured one was (soness's `src`), its files' contents beside the point. */
  private val capturedProject: ZIO[Scope, Throwable, Path] =
    SkillsSpec.tree(
      "src/main/scala/com/jamesward/soness/Soness.scala" -> "",
      "src/test/resources/intellij-idea-2026.2.3.jsonl" -> "",
      "src/test/scala/com/jamesward/soness/HarnessSpec.scala" -> "",
    ).map(_.toRealPath())

  def spec = suite("IntelliJ IDEA's MCP server, mocked")(
    test("answers every captured exchange as the real server did") {
      for
        exchanges <- captured
        project   <- capturedProject
        replayed   = exchanges.map: e =>
                       val expected = e.text.replace("${PROJECT}", project.toString)
                       val actual = IntellijRouter.run(e.command, project) match
                         case Left(error)   => ("error", error)
                         case Right(result) => ("result", result.fromJson[Json].fold(_ => result, _.toJson))
                       val normalized = if e.kind == "result" then expected.fromJson[Json].fold(_ => expected, _.toJson) else expected
                       (e.command, (e.kind, normalized), actual)
        mismatches = replayed.filter((_, expected, actual) => expected != actual)
      yield assertTrue(exchanges.size >= 10, exchanges.exists(_.kind == "result"), mismatches == Nil)
    },
    test("apply_patch and create_new_file answer as the real ones did, in the same sequence") {
      def q(s: String) = "'" + s.replace("'", "'\"'\"'") + "'"
      val discounts = "src/main/scala/shop/Discounts.scala"
      val update = s"""*** Begin Patch
        |*** Update File: $discounts
        |@@
        |-  def lookup(code: String): Option[Discount] = active.get(code.trim.toUpperCase)
        |+  def lookup(code: String): Option[Discount] = active.get(code.trim.toLowerCase)
        |*** End Patch""".stripMargin
      val unified = s"""--- a/$discounts
        |+++ b/$discounts
        |@@ -11 +11 @@
        |-  def lookup(code: String): Option[Discount] = active.get(code.trim.toLowerCase)
        |+  def lookup(code: String): Option[Discount] = active.get(code.trim.toLowerCase())
        |""".stripMargin
      val add = """*** Begin Patch
        |*** Add File: src/test/scala/shop/DiscountsSpec.scala
        |+package shop
        |+
        |+class DiscountsSpec extends munit.FunSuite
        |*** End Patch""".stripMargin
      val mixed = s"""*** Begin Patch
        |*** Update File: $discounts
        |@@ object Discounts:
        |   private val active: Map[String, Discount] = Map(
        |-    "save10" -> Discount("save10", 10),
        |+    "save10" -> Discount("save10", 15),
        |*** Delete File: src/main/scala/shop/X.scala
        |*** Update File: src/main/scala/shop/Missing.scala
        |@@
        |-a
        |+b
        |*** End Patch""".stripMargin
      val noIndent = s"""*** Begin Patch
        |*** Update File: $discounts
        |@@
        |-"welcome5" -> Discount("welcome5", 5),${"   "}
        |+"welcome5" -> Discount("welcome5", 7),
        |*** End Patch""".stripMargin
      val sequence = List(
        "apply_patch --input " + q(update) -> Right("1 out of 1 operations applied."),
        "apply_patch --input " + q(update) -> Right(s"0 out of 1 operations applied. 1 operations failed to be applied due to errors:\n$discounts: Hunk context not found"),
        "apply_patch --input " + q(unified) -> Right("1 out of 1 operations applied."),
        "apply_patch --input " + q(add) -> Right("1 out of 1 operations applied."),
        "apply_patch --input " + q(add) -> Right("0 out of 1 operations applied. 1 operations failed to be applied due to errors:\nsrc/test/scala/shop/DiscountsSpec.scala: File already exists: src/test/scala/shop/DiscountsSpec.scala"),
        "apply_patch --input 'nonsense'" -> Left("patch must include *** Begin Patch"),
        "apply_patch" -> Left("input must be a non-empty string"),
        "create_new_file --pathInProject 'src/main/scala/shop/X.scala' --text 'object X'" -> Right("[success]"),
        "create_new_file --pathInProject 'src/main/scala/shop/X.scala' --text 'object Y'" -> Left("File already exists: src/main/scala/shop/X.scala. Specify 'overwrite=true' to overwrite it"),
        "apply_patch --input " + q(mixed) -> Right("2 out of 3 operations applied. 1 operations failed to be applied due to errors:\nsrc/main/scala/shop/Missing.scala: File not found: src/main/scala/shop/Missing.scala"),
        "apply_patch --input " + q(noIndent) -> Right("1 out of 1 operations applied."),
      )
      for
        root   <- SkillsSpec.tree(discounts -> """package shop
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
                    |""".stripMargin).map(_.toRealPath())
        actual <- ZIO.succeed(sequence.map((command, _) => IntellijRouter.run(command, root)))
        after  <- ZIO.attempt(java.nio.file.Files.readString(root.resolve(discounts)))
        x       = root.resolve("src/main/scala/shop/X.scala")
      yield assertTrue(
        actual == sequence.map(_._2),
        after.contains("""    "save10" -> Discount("save10", 15),""" + "\n" + """"welcome5" -> Discount("welcome5", 7),"""),
        after.contains("active.get(code.trim.toLowerCase())"),
        !java.nio.file.Files.exists(x),
      )
    },
    test("the mock's terminal runs build, test and read-only commands, and refuses the rest") {
      assertTrue(
        IntellijRouter.allowed("./gradlew test --tests 'MainTest.scenario2'"),
        IntellijRouter.allowed("./sbt -batch testQuick 2>&1 | tail -30"),
        IntellijRouter.allowed("ls -la && cat build.gradle.kts"),
        !IntellijRouter.allowed("docker rm -f $(docker ps -aq) || true"),
        !IntellijRouter.allowed("docker ps"),
        !IntellijRouter.allowed("rm -rf build"),
        !IntellijRouter.allowed("ls; rm -rf ~"),
        !IntellijRouter.allowed("sed -i s/a/b/ Main.java"),
        !IntellijRouter.allowed("find . -delete"),
        !IntellijRouter.allowed("echo `whoami`"),
      )
    },
    test("the search tools take a glob, a symbol or a text, and a paths filter, as the real ones do") {
      for
        root   <- SkillsSpec.tree(IntellijEvalSpec.project*).map(_.toRealPath())
        files  <- ZIO.succeed(IntellijRouter.run("""search_file --q 'src/**'""", root))
        scala  <- ZIO.succeed(IntellijRouter.run("search_file --q '*.scala' --paths '[\"src/main/\"]'", root))
        symbol <- ZIO.succeed(IntellijRouter.run("search_symbol --q greetLoudly", root))
        text   <- ZIO.succeed(IntellijRouter.run("search_text --q greetLoudly --paths '[\"!**/test/**\"]'", root))
        noQ    <- ZIO.succeed(IntellijRouter.run("search_symbol --symbol greetLoudly", root))
      yield assertTrue(
        List("Main.scala", "Greeter.scala", "GreeterSpec.scala").forall(f => files.exists(_.contains(f))),
        scala.exists(r => r.contains("Greeter.scala") && !r.contains("GreeterSpec")),
        symbol.exists(r => r.contains("Greeter.scala") && r.contains("\"startLine\":5")),
        text.exists(r => r.contains("Greeter.scala") && !r.contains("GreeterSpec")),
        noQ == Left("Missing required parameters: q"),
      )
    },
    test("over stdio, soness offers the tools it routes, described from the catalog, each called through execute_tool") {
      for
        project   <- SkillsSpec.tree(IntellijEvalSpec.project*).map(_.toRealPath())
        javaHome  <- AcpSubprocess.property("java.home")
        classpath <- AcpSubprocess.property("soness.testClasspath")
        connected <- McpToolset.connect(List(McpEndpoint.Stdio(
                       "jetbrains_air_ide",
                       s"$javaHome/bin/java",
                       List("-cp", classpath, "com.jamesward.soness.IntellijMcpFixture"),
                       Map("IJ_MCP_SERVER_PROJECT_PATH" -> project.toString),
                     )))
        catalog   <- RoutedTools.catalog
        tools      = connected.toolset.tools
        readFile   = tools.find(_.name == "read_file")
        read      <- connected.toolset.call("read_file", Json.Obj("file_path" -> Json.Str("build.sbt"))).map(_.toJson)
        tree      <- connected.toolset.call("list_directory_tree", Json.Obj("directory" -> Json.Str("src"))).either
      yield assertTrue(
        connected.failed.isEmpty,
        tools.map(_.name).toList == IntellijRouter.routed.filterNot(_ == "execute_tool"),
        readFile.exists(_.inputSchema.toJson == catalog("read_file").inputSchema.toJson),
        readFile.exists(_.description.startsWith(catalog("read_file").description.trim)),
        read.contains("scalaVersion := \\\"3.7.2\\\""),
        // the router's errors come back as the routed tool's
        tree.left.exists(_.getMessage == "tool 'list_directory_tree' failed: Missing required parameters: directoryPath"),
      )
    },
    test("the commands soness writes split into the words meant, as the router splits them") {
      val value = "it's \"odd\" \\ $HOME\nline two"
      val command = RoutedTools.command("search_text",
        Json.Obj("q" -> Json.Str(value), "paths" -> Json.Arr(Json.Str("src/")), "limit" -> Json.Num(5), "x" -> Json.Null), known = true)
      assertTrue(IntellijRouter.words(command) == List("search_text", "--q", value, "--paths", "[\"src/\"]", "--limit", "5"))
    },
    test("against a real IntelliJ IDEA (SONESS_IJ_MCP_PORT), the catalog covers the routed tools and the mock answers as it does") {
      // read-only commands on whatever project the IDE opened; search results are compared without their order
      def normalized(text: String): String =
        text.fromJson[Json.Obj].toOption.fold(text): obj =>
          obj.get("items").flatMap(_.asArray).fold(obj.toJson)(items =>
            Json.Obj(obj.fields.filterNot(_._1 == "items") :+ ("items" -> Json.Arr(Chunk.from(items.map(_.toJson).sorted.flatMap(_.fromJson[Json].toOption))))).toJson)
      for
        port    <- System.env("SONESS_IJ_MCP_PORT").someOrFail(IllegalStateException("unset"))
        dir     <- System.env("SONESS_IJ_PROJECT").someOrFail(IllegalStateException("unset")).map(Path.of(_))
        idea    <- System.env("SONESS_IJ_IDEA").someOrFail(IllegalStateException("unset"))
        conn    <- McpToolset.connect(List(McpEndpoint.Stdio("jetbrains_air_ide", idea, List("stdioMcpServer"),
                     Map("IJ_MCP_SERVER_PORT" -> port, "IJ_MCP_SERVER_PROJECT_PATH" -> dir.toString))))
        catalog <- RoutedTools.catalog
        names    = conn.toolset.tools.map(_.name).toList
        sources <- ZIO.attemptBlocking(java.nio.file.Files.walk(dir.resolve("src")).iterator.asScala
                     .filter(p => p.toString.endsWith(".scala")).map(dir.relativize(_).toString).toList.sorted)
        word     = "object"
        calls    = List(
                     "search_file" -> Json.Obj("q" -> Json.Str("*.scala")),
                     "search_file" -> Json.Obj("q" -> Json.Str("*.scala"), "paths" -> Json.Arr(Json.Str("src/main/"))),
                     "search_text" -> Json.Obj("q" -> Json.Str(word)),
                     "search_regex" -> Json.Obj("q" -> Json.Str("def \\w+\\(")),
                     "list_directory_tree" -> Json.Obj("directoryPath" -> Json.Str("src"), "maxDepth" -> Json.Num(10)),
                     "read_file" -> Json.Obj("file_path" -> Json.Str(sources.head)),
                     "read_file" -> Json.Obj("file_path" -> Json.Str(sources.head), "limit" -> Json.Num(3)),
                     "read_file" -> Json.Obj("file_path" -> Json.Str("nope.txt")),
                   )
        compared <- ZIO.foreach(calls): (tool, arguments) =>
                      for
                        real   <- conn.toolset.call(tool, arguments).either
                        mocked  = IntellijRouter.run(RoutedTools.command(tool, arguments, known = true), dir)
                        realText = real.fold(e => Left(e.getMessage.replaceFirst(s"^tool '$tool' failed: ", "")),
                                     j => Right(j.asObject.flatMap(_.get("text")).flatMap(_.asString).getOrElse(j.toJson)))
                      yield (s"$tool ${arguments.toJson}", realText.map(normalized), mocked.map(normalized))
        differ   = compared.filter((_, real, mocked) => real != mocked)
        _       <- ZIO.foreachDiscard(differ)((call, real, mocked) => Console.printLine(s"differs: $call\n  real: $real\n  mock: $mocked"))
      yield assertTrue(names.nonEmpty, names.forall(catalog.contains), sources.nonEmpty, differ.isEmpty)
    } @@ TestAspect.ifEnvSet("SONESS_IJ_MCP_PORT"),
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem
