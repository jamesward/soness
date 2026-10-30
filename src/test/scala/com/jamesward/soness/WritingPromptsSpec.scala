package com.jamesward.soness

import zio.json.*
import zio.json.ast.Json
import zio.test.*

/** A tool result cut to the writer's limit ends on a whole line and, for a numbered listing, says where the rest starts. */
object WritingPromptsSpec extends ZIOSpecDefault:

  private val listing = (1 to 100).map(i => s"L$i: line $i of the file").mkString("\n")

  /** The last line number shown before the cut. */
  private def lastShown(cut: String): Int =
    "L(\\d+):".r.findAllMatchIn(cut.takeWhile(_ != '…')).map(_.group(1).toInt).toList.last

  def spec = suite("writing prompts")(
    test("a numbered listing is cut after a whole line, and the marker names the line to read again from") {
      val cut = WritingPrompts(200).cut(listing)
      val last = lastShown(cut)
      assertTrue(cut.takeWhile(_ != '…').endsWith(s"line $last of the file"), cut.contains(s"read again from line ${last + 1}"))
    },
    test("so is a listing inside a JSON result, its line breaks escaped") {
      val cut = WritingPrompts(200).cut(Json.Obj("text" -> Json.Str(listing)).toJson)
      val last = lastShown(cut)
      assertTrue(cut.takeWhile(_ != '…').endsWith(s"line $last of the file"), cut.contains(s"read again from line ${last + 1}"))
    },
    test("a result within the limit is whole") {
      assertTrue(WritingPrompts(200).cut("short") == "short")
    },
  )
