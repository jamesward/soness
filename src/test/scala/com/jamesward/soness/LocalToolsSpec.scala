package com.jamesward.soness

import zio.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}

/** soness's own file and command tools: offered only for what the session lacks, confined to the project. */
object LocalToolsSpec extends ZIOSpecDefault:
  private def project = ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("local"))): d =>
    ZIO.attemptBlocking(Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).ignore
  .tap(d => ZIO.attemptBlocking {
    Files.createDirectories(d.resolve("src")); Files.writeString(d.resolve("src/A.scala"), "object A:\n  val x = 1\n  val y = 1\n")
    Files.createDirectories(d.resolve("target")); Files.writeString(d.resolve("target/A.class"), "x")
  })

  private def str(j: Json, k: String) = j.asObject.flatMap(_.get(k)).flatMap(_.asString).getOrElse("")
  private def spec(name: String) = ToolSpec(name, name, Json.Obj(), None, None)

  def spec = suite("local tools")(
    test("read, list, search, edit (exact and unique), write; paths outside the project are refused") {
      for
        root    <- project
        tools    = LocalTools.toolset(root)
        read    <- tools.call("read_file", Json.Obj("path" -> Json.Str("src/A.scala"), "line" -> Json.Num(2), "limit" -> Json.Num(1)))
        listed  <- tools.call("list_files", Json.Obj("glob" -> Json.Str("**/*")))
        found   <- tools.call("search_text", Json.Obj("pattern" -> Json.Str("val [xy]")))
        twice   <- tools.call("edit_file", Json.Obj("path" -> Json.Str("src/A.scala"), "old_text" -> Json.Str("= 1"), "new_text" -> Json.Str("= 2"))).either
        missing <- tools.call("edit_file", Json.Obj("path" -> Json.Str("src/A.scala"), "old_text" -> Json.Str("val z"), "new_text" -> Json.Str("v"))).either
        _       <- tools.call("edit_file", Json.Obj("path" -> Json.Str("src/A.scala"), "old_text" -> Json.Str("val y = 1"), "new_text" -> Json.Str("val y = 2")))
        _       <- tools.call("write_file", Json.Obj("path" -> Json.Str("src/b/B.scala"), "content" -> Json.Str("object B")))
        text    <- ZIO.attemptBlocking(Files.readString(root.resolve("src/A.scala")))
        b       <- ZIO.attemptBlocking(Files.readString(root.resolve("src/b/B.scala")))
        outside <- tools.call("read_file", Json.Obj("path" -> Json.Str("../../etc/passwd"))).either
      yield assertTrue(
        str(read, "text") == "     2\t  val x = 1",
        listed.toString.contains("src/A.scala"), !listed.toString.contains("target"),
        found.toString.contains("src/A.scala:2:"), found.toString.contains("src/A.scala:3:"),
        twice.left.exists(_.getMessage.contains("2 times")), missing.left.exists(_.getMessage.contains("isn't in the file")),
        text == "object A:\n  val x = 1\n  val y = 2\n", b == "object B",
        outside.left.exists(_.getMessage.contains("outside the project")),
      )
    },
    test("logs and build output, natively: a directory newest or largest first, a log's end, counts, build output by glob or flag") {
      for
        root   <- project
        _      <- ZIO.attemptBlocking:
                    val logs = Files.createDirectories(root.resolve("target/logs"))
                    List(("a.log", 3, 30_000L), ("b.log", 50, 10_000L), ("c.log", 7, 20_000L)).foreach: (n, lines, ago) =>
                      val f = logs.resolve(n)
                      Files.writeString(f, (1 to lines).map(i => if i % 2 == 0 then s"ERROR $i" else s"INFO $i").mkString("", "\n", "\n"))
                      Files.setLastModifiedTime(f, java.nio.file.attribute.FileTime.fromMillis(java.lang.System.currentTimeMillis - ago))
        tools   = LocalTools.toolset(root)
        newest <- tools.call("list_directory", Json.Obj("path" -> Json.Str("target/logs"), "sort" -> Json.Str("modified")))
        largest <- tools.call("list_directory", Json.Obj("path" -> Json.Str("target"), "sort" -> Json.Str("size"), "depth" -> Json.Num(3)))
        tail   <- tools.call("read_file", Json.Obj("path" -> Json.Str("target/logs/b.log"), "tail" -> Json.Num(2)))
        counts <- tools.call("search_text", Json.Obj("pattern" -> Json.Str("ERROR"), "glob" -> Json.Str("target/logs/*.log"), "count" -> Json.Bool(true)))
        hidden <- tools.call("list_files", Json.Obj("glob" -> Json.Str("**/*.log")))
        flagged <- tools.call("list_files", Json.Obj("glob" -> Json.Str("**/*.log"), "include_build_output" -> Json.Bool(true), "sort" -> Json.Str("size")))
        outside <- tools.call("list_directory", Json.Obj("path" -> Json.Str("../"))).either
      yield
        def paths(j: Json, k: String) = j.asObject.flatMap(_.get(k)).flatMap(_.asArray).toList.flatten.flatMap(_.asObject).flatMap(_.get("path")).flatMap(_.asString)
        assertTrue(
          // paths as the file tools take them: relative to the project
          paths(newest, "entries") == List("target/logs/b.log", "target/logs/c.log", "target/logs/a.log"),
          paths(largest, "entries").head == "target/logs/b.log",
          str(tail, "text").contains("    49\tINFO 49"), str(tail, "text").contains("    50\tERROR 50"), !str(tail, "text").contains("ERROR 48"),
          tail.asObject.flatMap(_.get("lines")).contains(Json.Num(50)),
          counts.asObject.flatMap(_.get("total")).contains(Json.Num(1 + 25 + 3)),
          !hidden.toString.contains("b.log"), paths(flagged, "files").head == "target/logs/b.log",
          outside.isLeft,
        )
    },
    test("an edit whose lines match but for indentation is made, re-indented; an ambiguous one isn't") {
      val text = "object A:\n  def f =\n    1\n  def g =\n    1\n"
      assertTrue(
        LocalTools.reindented(text, "def f =\n  1", "def f =\n  2\n  // two").contains("object A:\n  def f =\n    2\n    // two\n  def g =\n    1\n"),
        LocalTools.reindented(text, "    1", "2").isEmpty,
        LocalTools.reindented(text, "def h =", "x").isEmpty,
      )
    },
    test("a command's exit code and output; one past its timeout is killed; one interrupted is killed") {
      for
        root    <- project
        tools    = LocalTools.toolset(root)
        ok      <- tools.call("run_command", Json.Obj("command" -> Json.Str("ls src; echo oops >&2; exit 3")))
        slow    <- tools.call("run_command", Json.Obj("command" -> Json.Str("sleep 30"), "timeout_seconds" -> Json.Num(1))).timed
        started <- tools.call("run_command", Json.Obj("command" -> Json.Str("echo $$ > pid; sleep 30"))).fork
        _       <- ZIO.attemptBlocking(Files.exists(root.resolve("pid")) && Files.size(root.resolve("pid")) > 0).repeatUntil(identity)
        _       <- started.interrupt
        pid     <- ZIO.attemptBlocking(Files.readString(root.resolve("pid")).trim.toLong)
        alive   <- ZIO.attemptBlocking(ProcessHandle.of(pid).map(_.isAlive).orElse(false)).repeatWhile(identity).timeout(5.seconds)
      yield assertTrue(
        ok.toString.contains("\"exit_code\":3"), str(ok, "output").contains("A.scala"), str(ok, "output").contains("oops"),
        slow._2.toString.contains("timed_out"), slow._1 < 10.seconds,
        alive.contains(false),
      )
    } @@ TestAspect.withLiveClock,
    test("the local tools are all there for the resolver to choose from; with an IDE and sbta, none of them is offered") {
      def from(source: ToolResolver.ToolSource, family: String, names: String*) =
        ToolResolver.candidates(new Toolset:
          val tools = Chunk.from(names.map(spec))
          def call(name: String, arguments: Json.Obj) = ZIO.succeed(Json.Str(name)), source, family)
      val local = LocalTools.toolset(Path.of("/p"))
      val r = ToolResolver.resolve(from(ToolResolver.ToolSource.IntellijMcp, "intellij", "read_file", "apply_patch", "create_new_file",
          "search_file", "list_directory_tree", "search_text", "execute_terminal_command") ++ from(ToolResolver.ToolSource.ProjectMcp, "sbta", "compile", "check") ++
        ToolResolver.candidates(local, ToolResolver.ToolSource.Local, "local"))
      assertTrue(local.tools.map(_.name).toSet == Set("read_file", "list_directory", "list_files", "search_text", "edit_file", "write_file", "run_command"),
        !r.offered.exists(_.source == ToolResolver.ToolSource.Local))
    },
    test("an IDE's edits and listings reach the source dependencies through soness; its reads go to the IDE; file:// URLs are paths") {
      for
        root    <- project
        proj     = root.resolve("app")
        dep      = root.resolve("lib")
        _       <- ZIO.attemptBlocking { Files.createDirectories(proj); Files.createDirectories(dep.resolve("src")) }
        seen    <- Ref.make(Vector.empty[(String, Json.Obj)])
        ide      = new Toolset:
                     val tools = Chunk("read_file", "create_new_file", "list_directory_tree", "apply_patch", "get_file_problems").map(spec)
                     def call(name: String, arguments: Json.Obj) = seen.update(_ :+ (name -> arguments)).as(Json.Obj("text" -> Json.Str("ide")))
        tools    = LocalTools.reaching(proj, ide, List(dep))
        created <- tools.call("create_new_file", Json.Obj("pathInProject" -> Json.Str("../lib/src/N.scala"), "text" -> Json.Str("object N:\n  val n = 1\n")))
        again   <- tools.call("create_new_file", Json.Obj("pathInProject" -> Json.Str("../lib/src/N.scala"), "text" -> Json.Str("x"))).either
        patched <- tools.call("apply_patch", Json.Obj("input" -> Json.Str(
                     s"*** Begin Patch\n*** Update File: ${dep.resolve("src/N.scala")}\n@@\n object N:\n-  val n = 1\n+  val n = 2\n*** End Patch")))
        missed  <- tools.call("apply_patch", Json.Obj("input" -> Json.Str(
                     "*** Begin Patch\n*** Update File: ../lib/src/N.scala\n@@\n-  val z = 1\n+  val z = 2\n*** End Patch"))).either
        read    <- tools.call("read_file", Json.Obj("file_path" -> Json.Str("../lib/src/N.scala"), "offset" -> Json.Num(2)))
        listed  <- tools.call("list_directory_tree", Json.Obj("directoryPath" -> Json.Str(dep.toString)))
        problem <- tools.call("get_file_problems", Json.Obj("filePath" -> Json.Str("../lib/src/N.scala"))).either
        _       <- tools.call("create_new_file", Json.Obj("pathInProject" -> Json.Str(s"file://${proj.resolve("x.txt")}")))
        _       <- tools.call("read_file", Json.Obj("file_path" -> Json.Str("src/A.scala")))
        ideSaw  <- seen.get
      yield assertTrue(
        str(created, "text") == "[success]", again.left.exists(_.getMessage.contains("already exists")),
        str(patched, "text").startsWith("1 out of 1"), missed.left.exists(_.getMessage.contains("0 out of 1")),
        // the IDE's read reaches the dependency's files itself
        str(read, "text") == "ide",
        listed.toString.contains(dep.resolve("src/N.scala").toString),
        // a tool the IDE may handle itself is the IDE's
        problem.isRight,
        // the project's own calls and reads reach the IDE, a file:// URL as its path
        ideSaw.map(_._1) == Vector("read_file", "get_file_problems", "create_new_file", "read_file"),
        str(ideSaw(2)._2, "pathInProject") == proj.resolve("x.txt").toString,
      )
    },
    test("a file that doesn't exist: the error says so and lists the nearest directory's files, the likeliest first") {
      for
        root  <- project
        _     <- ZIO.attemptBlocking { Files.writeString(root.resolve("src/Sessions.scala"), "x"); Files.writeString(root.resolve("src/Zebra.scala"), "x") }
        tools  = LocalTools.toolset(root)
        gone  <- tools.call("read_file", Json.Obj("path" -> Json.Str("src/SessionUpdate.scala"))).either
        deep  <- tools.call("read_file", Json.Obj("path" -> Json.Str("src/nope/A.scala"))).either
        dir   <- tools.call("read_file", Json.Obj("path" -> Json.Str("src"))).either
        msg    = gone.left.map(_.getMessage).left.getOrElse("")
      yield assertTrue(msg.startsWith("no such file:"), msg.contains("Sessions.scala"), msg.indexOf("Sessions.scala") < msg.indexOf("Zebra.scala"),
        deep.left.exists(_.getMessage.contains("nearest directory that exists")), dir.left.exists(_.getMessage.contains("is a directory")))
    },
    test("globs as models write them: a ** directory may be none, and a name-only glob matches in any directory") {
      val base = Path.of("/p")
      def m(g: String, p: String) = LocalTools.globMatcher(base, g)(Path.of(p))
      assertTrue(
        m("**/src/main/**/*.scala", "src/main/A.scala"), m("**/src/main/**/*.scala", "x/src/main/a/A.scala"),
        m("*.scala", "src/main/A.scala"), m("src/**", "src/A.scala"), m("**/*.scala", "A.scala"),
        !m("src/*.scala", "src/main/A.scala"), !m("*.java", "src/A.scala"),
      )
    },
  ) @@ TestAspect.timeout(60.seconds)
