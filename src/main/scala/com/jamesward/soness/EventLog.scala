package com.jamesward.soness

import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.stream.ZStream
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
        arguments: String, succeeded: Boolean, result: String, files: List[String] = Nil, loop: String = "")
    case TaskEnded(session: String, task: Int, at: String, ending: String, reply: Option[String])
    /**
     * A file an edit call changed, as it found and left it: `patch` is the change as a unified diff of `file` (an
     * absolute path), so what a task changed is read from the log, never from a copy of the project.
     */
    case FileChanged(session: String, task: Int, at: String, call: String, file: String, patch: String, loop: String = "")

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
    case e: Event.FileChanged    => e.at

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

  /**
   * The log's events as they're appended, by any session in any soness process in the project, from the end it has
   * now: the file system says when the log grows (nothing polls), and each new complete line is an event.
   */
  def follow(file: Path): ZStream[Any, Throwable, Event] =
    ZStream.unwrapScoped:
      for
        _       <- ZIO.attemptBlocking(Files.createDirectories(file.getParent))
        watcher <- ZIO.fromAutoCloseable(ZIO.attemptBlocking(file.getFileSystem.newWatchService()))
        _       <- ZIO.attemptBlocking(file.getParent.register(watcher, java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY,
                     java.nio.file.StandardWatchEventKinds.ENTRY_CREATE))
        offset  <- ZIO.attemptBlocking(if Files.exists(file) then Files.size(file) else 0L).flatMap(Ref.make(_))
      yield
        // what's new since the offset, its complete lines only (a line being written is read when it's done)
        val fresh: zio.Task[Chunk[Event]] =
          offset.get.flatMap: from =>
            ZIO.attemptBlocking:
              if !Files.exists(file) || Files.size(file) <= from then (from, Chunk.empty[String])
              else
                val channel = java.nio.channels.FileChannel.open(file, StandardOpenOption.READ)
                try
                  val buffer = java.nio.ByteBuffer.allocate((Files.size(file) - from).toInt)
                  channel.read(buffer, from)
                  val text = String(buffer.array(), 0, buffer.position(), UTF_8)
                  val done = text.lastIndexOf('\n') + 1
                  (from + text.take(done).getBytes(UTF_8).length, Chunk.from(text.take(done).linesIterator.toList))
                finally channel.close()
            .flatMap((next, lines) => offset.set(next).as(lines.flatMap(_.fromJson[Event].toOption)))
        ZStream.repeatZIOChunk(ZIO.attemptBlockingInterrupt(watcher.take()).flatMap(key =>
          ZIO.succeed { key.pollEvents(); key.reset() } *> fresh))

  /** The files `task` of `session` changed, in the order it changed them: each change's file and patch. */
  def changes(events: List[Event], session: String, task: Int): List[Event.FileChanged] =
    events.collect { case c: Event.FileChanged if c.session == session && c.task == task => c }

  /** A patch kept in the log is at most this many characters (a whole new file of more is cut). */
  val MaxPatch = 200_000

  /**
   * The change from `before` to `after` (none: the file didn't exist, or no longer does) as a unified diff of `label`:
   * one hunk, from the first line that differs to the last, with no context lines, which `patch` applies.
   */
  def patchOf(label: String, before: Option[String], after: Option[String]): String =
    val b = before.fold(Vector.empty[String])(_.linesIterator.toVector)
    val a = after.fold(Vector.empty[String])(_.linesIterator.toVector)
    val prefix = b.zip(a).takeWhile(_ == _).size
    val suffix = b.drop(prefix).reverse.zip(a.drop(prefix).reverse).takeWhile(_ == _).size
    val (removed, added) = (b.slice(prefix, b.size - suffix), a.slice(prefix, a.size - suffix))
    def range(start: Int, n: Int) = s"${if n == 0 then start else start + 1},$n"
    val text = (Vector(s"--- ${if before.isEmpty then "/dev/null" else s"a/$label"}", s"+++ ${if after.isEmpty then "/dev/null" else s"b/$label"}",
      s"@@ -${range(prefix, removed.size)} +${range(prefix, added.size)} @@") ++ removed.map("-" + _) ++ added.map("+" + _))
      .mkString("", "\n", "\n")
    if text.length > MaxPatch then text.take(MaxPatch) + "\n… (cut)\n" else text

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
