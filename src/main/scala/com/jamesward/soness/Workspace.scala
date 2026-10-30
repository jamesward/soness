package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.{ParallelLoop, Sections}
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant

/**
 * A project's shared state, for every session working in it (a client's several chats, and, through the shared log on
 * disk, other soness processes): what each session's tasks are doing, and the results that don't depend on the
 * project's files, so no session redoes another's work.
 *
 *  - **The activity log**: every call a session's tasks make through the project's tools, by task (`<session>/task-N`,
 *    from [[ParallelLoop.currentLoop]]), with the task's request. A task's loops see the other sessions' work as part
 *    of their context ([[view]]): their requests, what they edit, the runs they make and how those ended, the pages
 *    they fetched; their own calls are their steps, so they aren't repeated.
 *  - **The shared result log**: a call whose result is the world's, not the project's (a web page, a versioned
 *    artifact's docs: read-only and fetching, or naming a group, artifact and version), is kept in
 *    `.soness/results/` for [[ttl]]; the same call from any session, any process, is answered from it, marked as such,
 *    and one made while the same call is in flight waits for that one rather than making its own.
 */
final class Workspace private (
  root: Option[Path],
  activity: Ref[Vector[Workspace.Activity]],
  tasks: Ref[Map[String, Workspace.TaskInfo]],
  inFlight: Ref[Map[String, Promise[Throwable, Json]]],
  sequence: Ref[Long],
  ttl: Duration,
):
  import Workspace.*

  /** `task` (`<session>/task-N`) as working on `request`, for the life of the scope. */
  def begin(task: String, request: String): URIO[Scope, Unit] =
    Clock.instant.flatMap: now =>
      ZIO.acquireRelease(tasks.update(_.updated(task, TaskInfo(request, now, None))))(_ =>
        Clock.instant.flatMap(end => tasks.update(_.updatedWith(task)(_.map(_.copy(ended = Some(end)))))))

  /** `toolset`'s calls recorded in the activity log, and the shareable ones answered from the shared result log. */
  def bind(toolset: Toolset): Toolset =
    val readOnly = SharedTools.reads(toolset.tools)
    def shareable(name: String, arguments: Json.Obj) =
      readOnly(name) && (ToolCallView.kind(name, toolset.tools.find(_.name == name).flatMap(_.annotations)) ==
        com.jamesward.zioacp.schema.ToolKind.Fetch || Versioned.forall(arguments.contains))
    new Toolset:
      val tools: Chunk[ToolSpec] = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        ParallelLoop.currentLoop.get.flatMap: loop =>
          val work = if shareable(name, arguments) then shared(name, arguments, loop, toolset.call(name, arguments)) else toolset.call(name, arguments)
          recorded(loop, name, arguments, readOnly(name), work)

  /** The activity log's entry for the call, running while it runs, then with how it ended. */
  private def recorded(loop: Option[String], name: String, arguments: Json.Obj, reads: Boolean, work: Task[Json]): Task[Json] =
    for
      n    <- sequence.updateAndGet(_ + 1)
      now  <- Clock.instant
      _    <- activity.update(a => (a :+ Activity(n, loop.getOrElse(""), name, arguments, reads, now, None)).takeRight(MaxActivity))
      out  <- work.exit
      end  <- Clock.instant
      // a read of a file: the file as it was read, so a later view knows whether what was read is still what's there
      file  = if reads then fileOf(arguments) else None
      seen <- ZIO.foreach(file)(stamp).map(_.flatten)
      _    <- activity.update(_.map(a => if a.sequence == n then a.copy(ended = Some(end -> out.foldExit(c => Left(message(c.squash)), r => Right(r))),
                file = file, stamp = seen) else a))
      res  <- out
    yield res

  /**
   * A shareable call: from the shared result log when it holds the call (younger than [[ttl]]), else the same call in
   * flight, else made and kept.
   */
  private def shared(name: String, arguments: Json.Obj, loop: Option[String], work: Task[Json]): Task[Json] =
    val key = keyOf(name, arguments)
    stored(key).flatMap:
      case Some(entry) =>
        Clock.instant.flatMap: now =>
          val ago = java.time.Duration.between(Instant.parse(entry.at), now).toMinutes
          ZIO.logInfo(s"shared log: $name answered from ${entry.by}'s call $ago minutes ago") *>
            ZIO.succeed(marked(entry.result, s"From the project's shared log: the same call made by ${entry.by} $ago minutes ago; not made again."))
      case None =>
        Promise.make[Throwable, Json].flatMap: mine =>
          inFlight.modify(m => m.get(key).fold((None, m.updated(key, mine)))(other => (Some(other), m))).flatMap:
            case Some(other) =>
              ZIO.logInfo(s"shared log: $name joins the same call in flight") *>
                other.await.map(marked(_, "From the project's shared log: the same call, made at the same time by another task."))
            case None =>
              work.exit.flatMap: exit =>
                (exit match
                  case Exit.Success(result) => keep(key, name, arguments, loop, result)
                  case _                    => ZIO.unit) *>
                  mine.done(exit) *> inFlight.update(_ - key) *> exit

  private def stored(key: String): UIO[Option[Entry]] =
    ZIO.foreach(root)(r => ZIO.attemptBlocking {
      val file = resultsDir(r).resolve(s"$key.json")
      Option.when(Files.isRegularFile(file))(Files.readString(file, UTF_8)).flatMap(_.fromJson[Entry].toOption)
    }.orElseSucceed(None)).map(_.flatten).flatMap: found =>
      Clock.instant.map(now => found.filter(e => scala.util.Try(Instant.parse(e.at).plus(java.time.Duration.ofMillis(ttl.toMillis)).isAfter(now)).getOrElse(false)))

  private def keep(key: String, name: String, arguments: Json.Obj, loop: Option[String], result: Json): UIO[Unit] =
    ZIO.foreachDiscard(root): r =>
      Clock.instant.flatMap: now =>
        ZIO.attemptBlocking:
          val dir = resultsDir(r)
          Files.createDirectories(dir)
          val ignore = r.resolve(EventLog.Directory).resolve(".gitignore")
          if !Files.exists(ignore) then Files.writeString(ignore, "*\n")
          // written whole, then moved into place: a reader in another process never sees half of it
          val tmp = Files.createTempFile(dir, key, ".tmp")
          Files.writeString(tmp, Entry(name, arguments.toJson, now.toString, by(loop), result).toJson, UTF_8)
          Files.move(tmp, dir.resolve(s"$key.json"), java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
        .catchAll(e => ZIO.logDebug(s"shared log: couldn't keep $name: ${e.getMessage}"))

  /** A file argument's path (an IDE's `file://` URL as its path), resolved in the project. */
  private def fileOf(arguments: Json.Obj): Option[Path] =
    PathKeys.flatMap(k => arguments.get(k).flatMap(_.asString)).headOption.map(p => p.stripPrefix("file://")).flatMap: p =>
      scala.util.Try(root.fold(Path.of(p))(_.resolve(p)).normalize).toOption

  /** A file's size and last change, as it is now; none when it can't be read. */
  private def stamp(file: Path): UIO[Option[(Long, Long)]] =
    ZIO.attemptBlocking(Option.when(Files.isRegularFile(file))((Files.size(file), Files.getLastModifiedTime(file).toMillis))).orElseSucceed(None)

  /**
   * The project's file reads that still hold, for `self`'s loops: the latest read of each file (with its arguments: a
   * range is its own read) by any task but `self`'s (its own reads are its steps), in this session or another, whose
   * file hasn't changed since (same size and last change, and no call but a read of it since). Its text is in the
   * context, so a follow-up ("go ahead", "try again") or another session works from what's been read instead of reading
   * it again; a file that changed is left out, and is read again.
   */
  def known(self: String): UIO[Sections] =
    val mine = taskOf(self)
    for
      all   <- activity.get
      fresh <- ZIO.filter(all.filter(a => a.reads && taskOf(a.loop) != mine && a.file.isDefined && a.stamp.isDefined &&
                 a.ended.exists(_._2.isRight)).reverse.distinctBy(a => (a.file, a.arguments.toJson))): a =>
                 // a call that isn't a read, after it, naming the file (an edit): it may have changed, whatever the clock says
                 val touched = all.exists(b => b.sequence > a.sequence && !b.reads && b.arguments.toJson.contains(a.file.get.getFileName.toString))
                 if touched then ZIO.succeed(false) else stamp(a.file.get).map(_ == a.stamp)
    yield Sections(fresh.take(MaxKnown).map: a =>
      val text = a.ended.flatMap(_._2.toOption).fold("")(SharedTools.textOf)
      val who = if sessionOf(a.loop) == sessionOf(self) then "an earlier task of this session" else s"another session's task [${taskOf(a.loop)}]"
      Sections.Section(s"Already read by $who, and unchanged since (no need to read it again): ${a.tool} ${cut(a.arguments.toJson, 300)}",
        cut(text, KnownText), KnownPriority, KnownFloor))

  /**
   * The other sessions' work, as `self`'s loops see it (`self`: a loop, `<session>/task-N/...`): each other task that
   * is running or ended in the last [[Recent]], with its request and its calls (edits above runs above reads, newer
   * above older), fitted by priority with the rest of the context. Nothing of `self`'s own session: its tasks' work is in
   * their own context already.
   */
  def view(self: String): UIO[Sections] =
    val session = sessionOf(self)
    for
      now  <- Clock.instant
      all  <- activity.get
      info <- tasks.get
    yield
      val since = now.minusMillis(Recent.toMillis)
      val others = info.filter((t, i) => sessionOf(t) != session && i.ended.forall(_.isAfter(since)))
      val heads = others.toList.sortBy(_._2.started).map: (t, i) =>
        val state = i.ended.fold("working on it now")(e => s"ended ${java.time.Duration.between(e, now).toMinutes} minutes ago")
        Sections.Section(s"Another session's task [$t] in this project, $state", cut(i.request, 1500), TaskPriority, 300)
      val calls = all.filter(a => others.contains(taskOf(a.loop))).map: a =>
        val what = s"[${taskOf(a.loop)}] ${a.tool} ${cut(a.arguments.toJson, 300)}"
        val (priority, text) = a.ended match
          case None                    => (RunPriority, "still running")
          case Some((_, Left(error)))  => (if a.reads then ReadPriority else EditPriority, s"failed: ${cut(error, 300)}")
          case Some((_, Right(result))) =>
            if !a.reads && SharedTools.runs(a.tool) then (RunPriority, cut(SharedTools.textOf(result), 600))
            else if !a.reads then (EditPriority, "done (its change is in the files)")
            else (ReadPriority, "done" + (if a.tool == "web_fetch" then " (calling it again is answered from the shared log)" else ""))
        Sections.Section(s"Another session's call, $what", text, priority)
      Sections(heads.toVector ++ calls.reverse)

  /** What a loop of `self` is given of the project's shared state: the other sessions' work, and the reads that still hold. */
  def context(self: String): UIO[Sections] = view(self).zipWith(known(self))(_ ++ _)

object Workspace:

  /** How long a shared result is kept (`SONESS_SHARED_TTL_MINUTES`, default 60). */
  val DefaultTtl: Duration = 60.minutes

  /** How long an ended task of another session stays in a loop's view. */
  val Recent: Duration = 30.minutes

  /** How many calls the activity log keeps. */
  val MaxActivity = 400

  /** The arguments that name a versioned artifact, whose docs and sources never change. */
  val Versioned: List[String] = List("groupId", "artifactId", "version")

  /** An argument naming a file. */
  val PathKeys: List[String] = List("path", "file_path", "filePath", "pathInProject")

  /** At most this many earlier reads are in a loop's context, each cut to [[KnownText]] and, when the context is tight, to [[KnownFloor]]. */
  val MaxKnown = 12
  val KnownText = 20000
  val KnownFloor = 1500

  /** The view's priorities, among the context's sections (the project context's is 40, steering's above). */
  val TaskPriority = 36
  val RunPriority = 32
  val EditPriority = 30
  val ReadPriority = 18
  /** Earlier reads still fresh: below the other sessions' runs and edits, above their reads' one-line notes. */
  val KnownPriority = 24

  /** A task: its request, when it started, when it ended. */
  final case class TaskInfo(request: String, started: Instant, ended: Option[Instant])

  /** A call of the activity log: by which loop, of what, and how it ended (running while `ended` is none). */
  final case class Activity(sequence: Long, loop: String, tool: String, arguments: Json.Obj, reads: Boolean, at: Instant,
      ended: Option[(Instant, Either[String, Json])], file: Option[Path] = None, stamp: Option[(Long, Long)] = None)

  /** A shared result: the call, when, by which task, and its result. */
  final case class Entry(tool: String, arguments: String, at: String, by: String, result: Json) derives JsonCodec

  def make(root: Option[Path]): UIO[Workspace] =
    for
      ttl      <- System.env("SONESS_SHARED_TTL_MINUTES").map(_.flatMap(_.trim.toLongOption)).orElseSucceed(None)
      activity <- Ref.make(Vector.empty[Activity])
      tasks    <- Ref.make(Map.empty[String, TaskInfo])
      inFlight <- Ref.make(Map.empty[String, Promise[Throwable, Json]])
      sequence <- Ref.make(0L)
    yield Workspace(root, activity, tasks, inFlight, sequence, ttl.fold(DefaultTtl)(_.minutes))

  /** A loop's task: its first two parts (`e951b73f/task-1/main` is `e951b73f/task-1`). */
  def taskOf(loop: String): String = loop.split('/').take(2).mkString("/")

  /** A loop's session: its first part. */
  def sessionOf(loop: String): String = loop.takeWhile(_ != '/')

  /** The call's key in the shared result log: the tool and its arguments, canonical (keys sorted). */
  def keyOf(tool: String, arguments: Json.Obj): String =
    def canonical(j: Json): Json = j match
      case Json.Obj(fields) => Json.Obj(Chunk.from(fields.sortBy(_._1).map((k, v) => k -> canonical(v))))
      case Json.Arr(items)  => Json.Arr(items.map(canonical))
      case other            => other
    val digest = java.security.MessageDigest.getInstance("SHA-256").digest(s"$tool\n${canonical(arguments).toJson}".getBytes(UTF_8))
    digest.take(16).map(b => f"$b%02x").mkString

  private def resultsDir(root: Path): Path = root.resolve(EventLog.Directory).resolve("results")

  private def by(loop: Option[String]): String = loop.map(l => s"task ${taskOf(l)}").getOrElse("another task")

  /** A result with a note of where it came from: a field of an object result, else the result wrapped. */
  private def marked(result: Json, note: String): Json = result match
    case o: Json.Obj => o.add("sharedLog", Json.Str(note))
    case other       => Json.Obj("sharedLog" -> Json.Str(note), "result" -> other)

  private def cut(s: String, n: Int) = if s.length <= n then s else s.take(n) + s"… (${s.length - n} more)"

  private def message(e: Throwable) = Option(e.getMessage).getOrElse(e.toString)
