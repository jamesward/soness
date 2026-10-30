package com.jamesward.soness

import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

/**
 * The project's event log: everything soness's sessions in a project did, append-only, in `<project>/.soness/
 * events.jsonl` (one event a line), shared by every session and every soness process in the project. It's the
 * record; what a session shows and what a request's context holds are views of it:
 *
 *  - [[sessions]]: the project's sessions, for `session/list`;
 *  - [[replay]]: a session's history as the updates a client shows, for `session/load`;
 *  - [[tasks]]: the project's earlier tasks, from which Jev recalls what bears on a new request
 *    (zio-decisions' `Recall`), so no session starts from nothing and nothing needs clearing.
 *
 * `.soness/` has a `.gitignore` of its own (`*`): the log stays out of version control unless a project opts in.
 */
object EventLog:

  val Directory = ".soness"
  val FileName = "events.jsonl"

  /** A tool call's arguments and result are kept to this many characters. */
  val MaxDetail = 4000

  enum Event derives JsonCodec, CanEqual:
    case SessionStarted(session: String, at: String, cwd: String)
    case TaskStarted(session: String, task: Int, at: String, request: String)
    case ToolCalled(session: String, task: Int, at: String, call: String, tool: String, title: String, kind: String,
        arguments: String, succeeded: Boolean, result: String, files: List[String] = Nil)
    case TaskEnded(session: String, task: Int, at: String, ending: String, reply: Option[String])

    def session: String

  /** A project's log, appended to under a lock (this process's and, across processes, the file's own). */
  final class Log private[EventLog] (val file: Path, lock: Semaphore):
    def append(event: Event): UIO[Unit] =
      lock.withPermit:
        ZIO.attemptBlocking:
          Files.createDirectories(file.getParent)
          val ignore = file.getParent.resolve(".gitignore")
          if !Files.exists(ignore) then Files.writeString(ignore, "*\n")
          val channel = java.nio.channels.FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE,
            StandardOpenOption.APPEND)
          try
            val held = channel.lock()
            try channel.write(java.nio.ByteBuffer.wrap((event.toJson + "\n").getBytes(UTF_8)))
            finally held.release()
          finally channel.close()
        .unit
      .catchAll(e => ZIO.logDebug(s"event log $file: ${e.getMessage}"))

    /** Every event so far, in order; lines that don't parse (a torn write, a newer version's event) are skipped. */
    def read: UIO[List[Event]] =
      ZIO.attemptBlocking(if Files.isRegularFile(file) then Files.readAllLines(file, UTF_8).toArray.toList.map(_.toString) else Nil)
        .map(_.flatMap(_.fromJson[Event].toOption))
        .catchAll(e => ZIO.logDebug(s"event log $file: ${e.getMessage}").as(Nil))

  private val locks = new java.util.concurrent.ConcurrentHashMap[Path, Semaphore]()

  /** The log of the project at `cwd`. */
  def at(cwd: Path): Log =
    val file = cwd.toAbsolutePath.normalize.resolve(Directory).resolve(FileName)
    Log(file, locks.computeIfAbsent(file, _ => Unsafe.unsafely(Semaphore.unsafe.make(1))))

  def now: UIO[String] = Clock.instant.map(_.toString)

  /** A session's summary: its first request is its title, its last event when it was updated. */
  final case class Summary(session: String, cwd: String, title: Option[String], updatedAt: String)

  def sessions(events: List[Event]): List[Summary] =
    events.groupBy(_.session).toList.flatMap: (id, es) =>
      es.collectFirst { case e: Event.SessionStarted => e }.map: started =>
        Summary(id, started.cwd, es.collectFirst { case t: Event.TaskStarted => t.request.linesIterator.nextOption().getOrElse("").take(80) },
          es.map(at).max)
    .sortBy(_.updatedAt).reverse

  private def at(e: Event): String = e match
    case e: Event.SessionStarted => e.at
    case e: Event.TaskStarted    => e.at
    case e: Event.ToolCalled     => e.at
    case e: Event.TaskEnded      => e.at

  /** An earlier task: its request, how it ended, its reply, its calls' files, and when it ended. */
  final case class Task(session: String, task: Int, request: String, ending: String, reply: Option[String],
      files: List[String], endedAt: String)

  /** The project's ended tasks, oldest first. */
  def tasks(events: List[Event]): List[Task] =
    val started = events.collect { case t: Event.TaskStarted => (t.session, t.task) -> t }.toMap
    val calls = events.collect { case c: Event.ToolCalled => c }.groupBy(c => (c.session, c.task))
    events.collect:
      case e: Event.TaskEnded if started.contains((e.session, e.task)) =>
        Task(e.session, e.task, started((e.session, e.task)).request, e.ending, e.reply,
          calls.getOrElse((e.session, e.task), Nil).flatMap(_.files).distinct, e.at)

  /** The highest task number `session` has used, so a resumed session's tasks go on from it. */
  def lastTask(events: List[Event], session: String): Int =
    events.collect { case t: Event.TaskStarted if t.session == session => t.task }.maxOption.getOrElse(0)

  /** `session`'s history as a client shows it: each request, its tool calls (with their outcome) and its reply. */
  def replay(events: List[Event], session: String): List[SessionUpdate] =
    events.filter(_.session == session).flatMap:
      case t: Event.TaskStarted => List(SessionUpdate.UserMessageChunk(ContentBlock.text(t.request)))
      case c: Event.ToolCalled =>
        val id = ToolCallId(s"${c.call}-t${c.task}")
        List(
          SessionUpdate.ToolCallStarted(ToolCall(id, c.title, Some(c.tool), kindOf(c.kind),
            if c.succeeded then ToolCallStatus.Completed else ToolCallStatus.Failed,
            locations = c.files.map(ToolCallLocation(_)), rawInput = c.arguments.fromJson[Json].toOption,
            rawOutput = Some(Json.Str(c.result)))),
        )
      case e: Event.TaskEnded => e.reply.toList.map(r => SessionUpdate.AgentMessageChunk(ContentBlock.text(r)))
      case _                  => Nil

  /** A kind as the log keeps it (ACP's wire name, `execute`), and back. */
  def kindName(kind: ToolKind): String = kind.toJson.stripPrefix("\"").stripSuffix("\"")
  private def kindOf(kind: String): ToolKind = s"\"$kind\"".fromJson[ToolKind].getOrElse(ToolKind.Other)

  /** A detail kept in the log, cut to [[MaxDetail]]. */
  def cut(text: String): String = if text.length > MaxDetail then text.take(MaxDetail) + "… (cut)" else text

  /** How long ago `at` was, in minutes (0 for a time that doesn't parse). */
  def minutesAgo(at: String, now: Instant): Long =
    scala.util.Try(java.time.Duration.between(Instant.parse(at), now).toMinutes).getOrElse(0L)
