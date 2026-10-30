package com.jamesward.soness

import zio.*
import zio.json.ast.Json
import zio.test.*

import java.nio.file.Files

/** The extractors: the part of a large file a step asks for, whole lines with their numbers, never cut by soness. */
object ExtractorsSpec extends ZIOSpecDefault:
  import Extractors.*

  private def lines(texts: String*) = numbered(texts.mkString("\n"))

  private def str(j: Json, k: String) = j.asObject.flatMap(_.get(k)).flatMap(_.asString).getOrElse("")

  private def project = ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("extract"))): d =>
    ZIO.attemptBlocking(Files.walk(d).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).ignore

  def spec = suite("extractors")(
    test("a region is its lines, numbered") {
      assertTrue(region(lines("a", "b", "c", "d", "e"), Region(2, 4)) == Vector(Line(2, "b"), Line(3, "c"), Line(4, "d")))
    },
    test("matches come with their context, overlapping regions joined") {
      assertTrue(matching(lines("x", "a", "x", "b", "c", "d", "e", "x"), "x".r, 1) == List(Region(1, 4), Region(7, 8)))
    },
    test("an outline is the declarations and headings, with their line numbers") {
      assertTrue(outline(lines("object A:", "  // x", "  private[soness] def f = 1", "  val y = 2", "# Title", "  y + 1")).map(_.number) ==
        Vector(1, 3, 4, 5))
    },
    test("the tools read a file far longer than any cut: the lines asked for, the matches, the outline; outside the project refused") {
      for
        root    <- project
        _       <- ZIO.attemptBlocking(Files.writeString(root.resolve("Big.scala"),
                     (1 to 5000).map(i => if i % 1000 == 0 then s"  def f$i = $i" else s"  // line $i of a long file").mkString("object Big:\n", "\n", "\n")))
        tools    = Extractors.toolset(root)
        tail    <- tools.call("extract_lines", Json.Obj("path" -> Json.Str("Big.scala"), "from" -> Json.Num(4990), "to" -> Json.Num(5001)))
        found   <- tools.call("grep_file", Json.Obj("path" -> Json.Str("Big.scala"), "pattern" -> Json.Str("def f3000"), "context" -> Json.Num(1)))
        shape   <- tools.call("outline_file", Json.Obj("path" -> Json.Str("Big.scala")))
        outside <- tools.call("outline_file", Json.Obj("path" -> Json.Str("../../etc/passwd"))).either
      yield assertTrue(
        str(tail, "text").linesIterator.size == 12, str(tail, "text").endsWith("5001\t  def f5000 = 5000"),
        str(found, "text").linesIterator.size == 3, str(found, "text").contains("3001\t  def f3000 = 3000"),
        str(shape, "text").linesIterator.size == 6, str(shape, "text").contains("1001\t  def f1000 = 1000"),
        outside.left.exists(_.getMessage.contains("outside the project")),
      )
    },
  )
