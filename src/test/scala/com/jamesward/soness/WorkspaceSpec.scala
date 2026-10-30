package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ParallelLoop
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}

/** A project's shared state: other sessions' work in a loop's context, and the shared result log. */
object WorkspaceSpec extends ZIOSpecDefault:

  private def project = ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("workspace"))): d =>
    ZIO.attemptBlocking(Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).ignore

  private def as[A](loop: String)(zio: Task[A]) = ParallelLoop.currentLoop.locally(Some(loop))(zio)

  private def tools(fetched: Ref[Int], gate: Option[Promise[Nothing, Unit]] = None) = new Toolset:
    val tools = Chunk(
      ToolSpec("web_fetch", "Fetches a page.", Json.Obj(), None, Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))),
      ToolSpec("read_file", "Reads a file.", Json.Obj(), None, Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))),
      ToolSpec("apply_patch", "Edits.", Json.Obj(), None, None),
    )
    def call(name: String, arguments: Json.Obj): Task[Json] =
      (if name == "web_fetch" then fetched.update(_ + 1) *> ZIO.foreachDiscard(gate)(_.await) else ZIO.unit)
        .as(Json.Obj("text" -> Json.Str(s"$name ${arguments.toJson}")))

  def spec = suite("workspace")(
    test("a fetch another session made is answered from the shared log, in this process and another; the project's reads aren't") {
      for
        root    <- project
        fetched <- Ref.make(0)
        one     <- Workspace.make(Some(root))
        two     <- Workspace.make(Some(root))
        url      = Json.Obj("url" -> Json.Str("https://example.com/doc"))
        first   <- as("s1/task-1/main")(one.bind(tools(fetched)).call("web_fetch", url))
        // the same page, its arguments in another order: same call
        again   <- as("s2/task-1/main")(one.bind(tools(fetched)).call("web_fetch", Json.Obj("url" -> Json.Str("https://example.com/doc"))))
        // another process (its own workspace, the same project's log on disk)
        other   <- as("s3/task-1/main")(two.bind(tools(fetched)).call("web_fetch", url))
        _       <- as("s1/task-1/main")(one.bind(tools(fetched)).call("read_file", Json.Obj("path" -> Json.Str("A"))))
        _       <- as("s2/task-1/main")(one.bind(tools(fetched)).call("read_file", Json.Obj("path" -> Json.Str("A"))))
        n       <- fetched.get
        kept    <- ZIO.attemptBlocking(Files.list(root.resolve(".soness/results")).count())
      yield assertTrue(n == 1, kept == 1L, !first.toJson.contains("sharedLog"),
        again.toJson.contains("task s1/task-1"), other.toJson.contains("sharedLog"),
        again.asObject.flatMap(_.get("text")).map(_.toJson) == first.asObject.flatMap(_.get("text")).map(_.toJson))
    },
    test("the same fetch made at the same time by two tasks is made once") {
      for
        root    <- project
        fetched <- Ref.make(0)
        gate    <- Promise.make[Nothing, Unit]
        ws      <- Workspace.make(Some(root))
        url      = Json.Obj("url" -> Json.Str("https://example.com/slow"))
        a       <- as("s1/task-1/main")(ws.bind(tools(fetched, Some(gate))).call("web_fetch", url)).fork
        _       <- fetched.get.repeatUntil(_ == 1)
        b       <- as("s2/task-1/main")(ws.bind(tools(fetched, Some(gate))).call("web_fetch", url)).fork
        // the second call gets time to find the first in flight (yields alone lose the race under a loaded suite)
        _       <- ZIO.sleep(300.millis) *> gate.succeed(())
        both    <- a.join <*> b.join
        n       <- fetched.get
      yield assertTrue(n == 1, both._2.toJson.contains("at the same time"))
    } @@ TestAspect.withLiveClock,
    test("a loop's context has other sessions' tasks and calls, edits above reads; nothing of its own session") {
      for
        fetched <- Ref.make(0)
        ws      <- Workspace.make(None)
        _       <- ZIO.scoped(ws.begin("s2/task-1", "add web search to zio-bedrock") *>
                     as("s2/task-1/main")(ws.bind(tools(fetched)).call("apply_patch", Json.Obj("input" -> Json.Str("*** Begin Patch")))) *>
                     as("s2/task-1/main")(ws.bind(tools(fetched)).call("read_file", Json.Obj("path" -> Json.Str("Bedrock.scala")))))
        _       <- ZIO.scoped(ws.begin("s1/task-1", "my own task") *>
                     as("s1/task-1/main")(ws.bind(tools(fetched)).call("read_file", Json.Obj("path" -> Json.Str("Mine.scala")))))
        view    <- ws.view("s1/task-2/main")
        text     = view.render
        edit     = view.sections.find(_.title.contains("apply_patch")).map(_.priority)
        read     = view.sections.find(_.title.contains("Bedrock.scala")).map(_.priority)
      yield assertTrue(text.contains("add web search to zio-bedrock"), text.contains("[s2/task-1]"), !text.contains("Mine.scala"),
        !text.contains("my own task"), edit.exists(e => read.exists(e > _)))
    },
    test("an earlier task's read of a file still unchanged is in a later task's context, its text and all; a changed one isn't") {
      for
        root  <- project
        _     <- ZIO.attemptBlocking { Files.writeString(root.resolve("A.scala"), "object A"); Files.writeString(root.resolve("B.scala"), "object B") }
        ws    <- Workspace.make(Some(root))
        files  = new Toolset:
                   val tools = Chunk(ToolSpec("read_file", "Reads.", Json.Obj(), None, Some(Json.Obj("readOnlyHint" -> Json.Bool(true)))))
                   def call(name: String, arguments: Json.Obj): Task[Json] =
                     ZIO.attemptBlocking(Files.readString(root.resolve(arguments.get("file_path").flatMap(_.asString).get)))
                       .map(t => Json.Obj("text" -> Json.Str(s"1: $t")))
        read   = (task: String, f: String) => as(s"$task/main")(ws.bind(files).call("read_file", Json.Obj("file_path" -> Json.Str(f))))
        _     <- read("s1/task-1", "A.scala") *> read("s1/task-1", "B.scala")
        // B changes (another tool, an editor) after the read
        _     <- ZIO.sleep(20.millis) *> ZIO.attemptBlocking(Files.writeString(root.resolve("B.scala"), "object B { val changed = 1 }"))
        later <- ws.known("s1/task-2/main").map(_.render)
        own   <- ws.known("s1/task-1/main").map(_.render)
        other <- ws.context("s2/task-1/main").map(_.render)
      yield assertTrue(later.contains("1: object A"), later.contains("earlier task of this session"), !later.contains("object B"),
        own.isEmpty, other.contains("1: object A"), other.contains("another session's task [s1/task-1]"))
    } @@ TestAspect.withLiveClock,
    test("a call's key ignores its arguments' order") {
      assertTrue(Workspace.keyOf("web_fetch", Json.Obj("url" -> Json.Str("u"), "max_length" -> Json.Num(5))) ==
        Workspace.keyOf("web_fetch", Json.Obj("max_length" -> Json.Num(5), "url" -> Json.Str("u"))))
    },
  )
