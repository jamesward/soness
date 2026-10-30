package com.jamesward.soness

import com.jamesward.zio_decisions.typesafe.FakeJev
import zio.json.*
import zio.json.ast.Json

/**
 * Scripted Jev answers for the fakes. Jev's agent loop offers one option per tool (`"action":"call_tool"`, with the tool's
 * name and schemas) and two endings (`"action":"end"`, `"outcome":"answer"` or `"unable"`); the state holds the steps
 * taken so far.
 */
object JevScript:

  /** Picks, given the steps taken so far, the first option whose description satisfies the rule. */
  def apply(rule: (Vector[Json], String) => Boolean): FakeJev.Answers =
    withReadings((steps, _, view) => rule(steps, view))

  /** Picks, given the steps taken and the documents read so far, the first option whose description satisfies the rule. */
  def withReadings(rule: (Vector[Json], Vector[Json], String) => Boolean): FakeJev.Answers =
    (state, _, question) =>
      val steps = state.get("steps").flatMap(_.asArray).toVector.flatten
      val read = state.get("documentsRead").flatMap(_.asArray).toVector.flatten
      val options = question.get("criteria").flatMap(_.asObject).toList.flatMap(_.fields).map((id, view) => id -> view.toJson)
      FakeJev.Answer.Choice(options.find((_, view) => rule(steps, read, view)).orElse(options.headOption).map(_._1).getOrElse(""))

  def isDocument(title: String): String => Boolean =
    view => view.contains("\"action\":\"read_document\"") && view.contains(s"\"title\":\"$title\"")

  /** Reads the documents titled `titles` in order, then calls `tool` once, then answers. */
  def readThenCall(titles: List[String], tool: String): FakeJev.Answers =
    withReadings: (steps, read, view) =>
      if read.size < titles.size then isDocument(titles(read.size))(view)
      else if steps.isEmpty then isTool(tool)(view)
      else isAnswer(view)

  def isTool(name: String): String => Boolean = view => view.contains("\"action\":\"call_tool\"") && view.contains(s"\"tool\":\"$name\"")
  val isAnswer: String => Boolean = _.contains("\"outcome\":\"answer\"")
  val isUnable: String => Boolean = _.contains("\"outcome\":\"unable\"")

  /** Calls `tool` once, then ends with an answer. */
  def callThenAnswer(tool: String): FakeJev.Answers =
    JevScript((steps, view) => if steps.isEmpty then isTool(tool)(view) else isAnswer(view))

  /** Ends with an answer straight away. */
  val answer: FakeJev.Answers = JevScript((_, view) => isAnswer(view))

  /** Calls `tool` once and, when that fails, ends as unable. */
  def callThenUnable(tool: String): FakeJev.Answers =
    JevScript((steps, view) => if steps.isEmpty then isTool(tool)(view) else isUnable(view))
