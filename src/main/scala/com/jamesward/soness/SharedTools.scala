package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ParallelLoop
import zio.*
import zio.json.*
import zio.json.ast.Json

object SharedTools:
  /**
   * The session's tools for loops working at the same time (a session's turns, a [[ParallelLoop]]'s parts; each fiber
   * carries the loop it works for, [[ParallelLoop.currentLoop]]). Reads run freely. Calls that change files (not read-only, and not a build or a
   * command) run one at a time, so two edits of one file never interleave their read and write. Builds, tests and
   * commands run one at a time too (one build tool on one project: concurrent Gradle runs in one project are slower
   * than one after another, and time out), as a validation service:
   *
   *   - a run waits a moment (`gather`, at most as long as recent runs took) for other loops' requests to join it;
   *   - the calls waiting while one runs are merged ([[merge]]): identical ones run once, and test commands that differ
   *     only in the tests they select run as one selecting all of them;
   *   - with [[Policy.widen]], a test run is widened with the tests of the other parts that have edited since their
   *     test last ran (off by default: see [[Policy]]);
   *   - a part asking for tests that a finished run covered, started after the part's last edit, gets that run's
   *     output at once instead of a new run.
   *
   * A merged or widened run's output goes to every call it answers.
   *
   *   - a check (a build, a test run, a type check: [[supersedable]]) that another task's edit lands during (another
   *     session's, or another task of the same one; not one of its own callers' tasks), is stale before it ends: it's stopped and run again, its callers answered by the new run
   *     (at most [[MaxSupersede]] times, so edits that keep coming don't starve it). A command that may change things (a
   *     commit, a publish) is never stopped.
   */
  def coordinated(toolset: Toolset, gather: Duration = 5.seconds, policy: Policy = Policy()): ZIO[Scope, Nothing, Toolset] =
    Coordinator.make(gather, policy).map(_.bind(toolset))

  /**
   * One project's coordination (its edit lock, its run queue, its ledger of edits and runs), shared by every toolset
   * bound to it: a session's tasks, and the sessions of several ACP clients working in the same project at once (two
   * IDE chats, an IDE and a CLI), so their edits don't interleave and their builds and tests run one at a time, merged.
   */
  final class Coordinator private[SharedTools] (bindTo: Toolset => Toolset):
    /** `toolset`'s calls, coordinated with every other toolset bound to this project. */
    def bind(toolset: Toolset): Toolset = bindTo(toolset)

  object Coordinator:
    /**
     * `locks`, when given, is a directory of lock files shared with other soness processes (other ACP clients) in the
     * same project: a run, and an edit, holds its file's lock while it runs, so processes take turns too. Merging is
     * within a process.
     */
    def make(gather: Duration = 5.seconds, policy: Policy = Policy(), locks: Option[java.nio.file.Path] = None): ZIO[Scope, Nothing, Coordinator] =
      build(gather, policy, locks)

    /** The lock directory of the project at `project`, under the user's cache, so the project gets no files of ours. */
    def lockDir(project: java.nio.file.Path): java.nio.file.Path =
      val home = java.nio.file.Path.of(java.lang.System.getProperty("user.home"), ".cache", "soness", "locks")
      val digest = java.security.MessageDigest.getInstance("SHA-256").digest(project.toString.getBytes("UTF-8"))
      home.resolve(digest.take(12).map(b => f"$b%02x").mkString)

  /** Holds the lock of file `name` in `dir` (made when needed) while `zio` runs; no lock without a directory. */
  private def locked[A](dir: Option[java.nio.file.Path], name: String)(zio: Task[A]): Task[A] =
    dir.fold(zio): d =>
      // a file lock is the process's: within one, the lock's holders take turns on a semaphore first
      inProcess(d.resolve(name).toString).withPermit(fileLocked(d, name)(zio))

  private val semaphores = new java.util.concurrent.ConcurrentHashMap[String, Semaphore]()

  private def inProcess(key: String): Semaphore =
    semaphores.computeIfAbsent(key, _ => Unsafe.unsafely(Semaphore.unsafe.make(1)))

  private def fileLocked[A](d: java.nio.file.Path, name: String)(zio: Task[A]): Task[A] =
      ZIO.acquireReleaseWith(
        ZIO.attemptBlockingInterrupt:
          java.nio.file.Files.createDirectories(d)
          val channel = java.nio.channels.FileChannel.open(d.resolve(name), java.nio.file.StandardOpenOption.CREATE,
            java.nio.file.StandardOpenOption.WRITE)
          (channel, channel.lock())
      )((channel, lock) => ZIO.attemptBlocking { lock.release(); channel.close() }.ignore)(_ => zio)

  // a build tool's (sbta's `compile`, `test`, `run`, `sbt-task`, `clean`) too; not its `check`, a type check that doesn't
  // wait behind a running build
  private[soness] def runs(prefixed: String) =
    val name = ToolResolver.baseName(prefixed)
    name.contains("build") || name.contains("terminal") || name.contains("command") ||
    name.startsWith("run_") || name.contains("test") || name == "get_file_problems" ||
    Set("compile", "run", "clean", "sbt-task", "publish-local")(name)

  private def build(gather: Duration, policy: Policy, locks: Option[java.nio.file.Path]): ZIO[Scope, Nothing, Coordinator] =
    for
      edits   <- Semaphore.make(1)
      waiting <- Queue.unbounded[Run]
      ledger  <- Ref.make(Ledger())
      seen    <- Ref.make(Map.empty[Option[String], java.time.Instant])
      created <- Ref.make(Map.empty[String, Option[String]])
      // the runner: one run at a time, of everything waiting merged and widened. When other loops are at work, a run
      // waits a moment first for their requests to join it (a run's cost is mostly its start, so one run for several
      // is the saving); a loop working alone isn't held up.
      // a run waits at most as long as runs like it take (the median of the last ones): waiting longer than a run would
      // take to do it again doesn't pay
      durations <- Ref.make(Vector.empty[Long])
      // the check running now, if it can be superseded: an edit that lands completes it with the editing loop
      current <- Ref.make(Option.empty[Stale])
      _       <- waiting.take.flatMap: first =>
                   others(seen, first.loop).zip(durations.get).flatMap: (busy, ds) =>
                     val window = ds.sorted.lift(ds.size / 2).fold(gather)(ms => gather.min(Duration.fromMillis(ms)))
                     ZIO.sleep(window).when(busy)
                   *> waiting.takeAll.map(first +: _)
                 .flatMap: batch =>
                   // a run whose callers have all gone (their task stopped, interrupted) isn't made
                   ZIO.filter(batch)(_.wanted).flatMap(live => ZIO.foreachDiscard(
                       if policy.merge then merge(Chunk.from(live), policy.maxTests) else live.toList.map(r => (r.name, r.arguments, List(r)))
                     ): (name, arguments, calls) =>
                     (for
                       book     <- ledger.get
                       widened   = if policy.widen then widen(name, arguments, book, policy.widenFailing, policy.maxTests)
                                   else widen(name, arguments, Ledger(), true)
                       _        <- ZIO.logInfo(s"validation: one run for ${calls.size} calls${if widened.extra.nonEmpty then s", widened with ${widened.extra.mkString(", ")}" else ""}: " +
                                     widened.arguments.get("command").flatMap(_.asString).getOrElse(name)).when(calls.size > 1 || widened.extra.nonEmpty)
                       started  <- ledger.modify(l => (l.clock, l))
                       // the run stops when every call it's for has gone (stopped as hung, its task stopped): a run
                       // nobody waits for mustn't hold up the runs behind it
                       // `exit` first: a race waits for the first *success*, so a run that fails would wait for the
                       // abandonment check forever, and every run behind it with it
                       // through the first caller's toolset (its session's connection to the project's tools)
                       // in the first caller's fiber context (its tool call's id, its loop), not the runner's
                       stale    <- Promise.make[Nothing, Option[String]]
                       _        <- current.set(Some(Stale(stale, calls.flatMap(_.loop).map(taskOf).toSet)))
                                     .when(supersedable(name, widened.arguments) && calls.forall(_.restarts < MaxSupersede))
                       timed    <- locked(locks, "run")(inContext(calls.head.refs)(calls.head.via.call(name, capped(widened.arguments)))).exit
                                     // told when the last caller goes, not polled
                                     .raceEither(ZIO.foreachParDiscard(calls)(_.gone.await))
                                     .map:
                                       case Left(exit) => Right(exit)
                                       case Right(_)   => Right(Exit.fail(RuntimeException("stopped: every call waiting for it had gone")))
                                     // an edit landed: the run is stale (interrupting the call stops it: an MCP call's
                                     // `notifications/cancelled`, a command's process)
                                     .raceEither(stale.await).map(_.fold(identity, Left(_)))
                                     .timed
                                     .ensuring(current.set(None))
                       (took, ran) = timed
                       command   = widened.arguments.get("command").flatMap(_.asString).getOrElse(name)
                     yield ran match
                       case Left(by) =>
                         ZIO.logInfo(s"validation: $command superseded after ${took.toMillis}ms by an edit of ${by.getOrElse("a loop")}; running it again") *>
                           waiting.offerAll(calls.map(c => c.copy(restarts = c.restarts + 1))).unit
                       case Right(exit) =>
                         finish(name, widened, calls, started, took, exit, durations, ledger)
                     ).flatten)
                 .forever.forkScoped
    yield Coordinator(toolset => bound(toolset, edits, waiting, ledger, seen, created, current, locks))

  /** A run that ended: its time noted, its result recorded in the ledger and given to each of its calls. */
  private def finish(name: String, widened: Widened, calls: List[Run], started: Long, took: Duration, exit: Exit[Throwable, Json],
      durations: Ref[Vector[Long]], ledger: Ref[Ledger]): UIO[Unit] =
                     for
                       _        <- durations.update(ds => (ds :+ took.toMillis).takeRight(9))
                       _        <- ZIO.logInfo(s"validation: ran ${widened.arguments.get("command").flatMap(_.asString).getOrElse(name)} in " +
                                     s"${took.toMillis}ms for ${calls.flatMap(_.loop).distinct.mkString(", ")}" +
                                     (if exit.isSuccess then "" else " (failed)"))
                       // a command that isn't a test selection may have changed files (a script, a formatter): it
                       // counts as an edit, so no later request is answered from a run before it
                       _        <- ledger.update(l => if widened.selection.isEmpty then l.edited(calls.head.loop) else l)
                       _        <- ledger.update(_.ran(Finished(name, widened.selection, started, exit)))
                       all       = widened.selection.toList.flatMap(_._2)
                       _        <- ZIO.foreachDiscard(calls): call =>
                                     val own = call.arguments.get("command").flatMap(_.asString).flatMap(Selection.of).toList.flatMap(_.tests)
                                     call.done.done(exit.mapExit(json => forCaller(json, own, all)))
                     yield ()

  private def bound(toolset: Toolset, edits: Semaphore, waiting: Queue[Run], ledger: Ref[Ledger],
      seen: Ref[Map[Option[String], java.time.Instant]], created: Ref[Map[String, Option[String]]],
      current: Ref[Option[Stale]], locks: Option[java.nio.file.Path]): Toolset =
    new Toolset:
      val readOnly = reads(toolset.tools)
      val tools = toolset.tools
      def call(name: String, arguments: Json.Obj): Task[Json] =
        ParallelLoop.currentLoop.get.flatMap: loop =>
          Clock.instant.flatMap(now => seen.update(_.updated(loop, now))) *> (
          if runs(name) then
            val selection = arguments.get("command").flatMap(_.asString).flatMap(Selection.of)
            // a test run that ran no test checked nothing: a failed step, saying why, not a success
            (ledger.get.flatMap: book =>
              book.covering(name, selection, loop) match
                case Some(done) =>
                  val own = selection.toList.flatMap(_.tests)
                  ZIO.logInfo(s"validation: ${loop.getOrElse("a loop")}'s run answered by a finished one")
                    .as(done.exit.mapExit(json => forCaller(json, own, done.selection.toList.flatMap(_._2), finished = true))).flatten
                case None =>
                  ledger.update(_.requested(name, selection, loop)) *>
                    Promise.make[Throwable, Json].zip(Promise.make[Nothing, Unit]).zip(ClientTools.currentCall.get).flatMap: (done, gone, refs) =>
                      waiting.offer(Run(name, arguments, done, loop, gone, toolset, refs)) *> done.await.onInterrupt(gone.succeed(()))
            ).flatMap(json => ZIO.fromEither(SharedTools.ranTests(name, json)).mapError(IllegalStateException(_)))
          else if readOnly(name) then toolset.call(name, arguments)
          else edit(name, arguments, loop)
          )

      /**
       * An edit, one at a time. While other loops work, a loop may overwrite only a file it created itself:
       * overwriting a shared file replaces the others' work in it.
       */
      private def edit(name: String, arguments: Json.Obj, loop: Option[String]): Task[Json] =
        val target = List("pathInProject", "path", "file_path", "filePath").flatMap(k => arguments.get(k).flatMap(_.asString)).headOption
        val overwrite = arguments.get("overwrite").contains(Json.Bool(true))
        created.get.zip(others(seen, loop)).flatMap: (made, busy) =>
          target.filter(t => overwrite && busy && !made.get(t).contains(loop)) match
            case Some(t) =>
              ZIO.fail(RuntimeException(s"not overwritten: other agents are working in the project at the same time, and " +
                s"$t may hold their work; change your part of it with a patch (apply_patch), or create a file of your own"))
            case None =>
              edits.withPermit(locked(locks, "edit")(toolset.call(name, arguments)))
                .tap(_ => ZIO.foreachDiscard(target.filter(_ => !overwrite))(t => created.update(m => if m.contains(t) then m else m.updated(t, loop))))
                // a check another task is running now is stale: it's stopped and run again
                .tap(_ => ZIO.foreachDiscard(loop.filter(_.contains('/')).map(taskOf)): editor =>
                  current.modify:
                    case Some(st) if !st.tasks(editor) => (Some(st), None)
                    case other                         => (None, other)
                  .flatMap(ZIO.foreachDiscard(_)(_.promise.succeed(loop))))
                .ensuring(ledger.update(_.edited(loop)))

  /**
   * A command's timeout, at most [[MaxRunTimeout]]: every other loop's runs wait for this one, so one that hangs (a
   * test of a race done wrong) mustn't hold them for the 10 minutes a model may ask for.
   */
  private def capped(arguments: Json.Obj): Json.Obj =
    arguments.get("timeout").flatMap(_.asNumber).map(_.value.longValue) match
      case Some(t) if t > MaxRunTimeout.toMillis => arguments.add("timeout", Json.Num(MaxRunTimeout.toMillis))
      case _                                     => arguments

  val MaxRunTimeout: Duration = 3.minutes

  /**
   * The check running now that an edit can supersede, and the tasks it's for: an edit by one of those tasks doesn't (a
   * task's own parts' runs are merged as they come, not restarted), another task's, another session's, does.
   */
  private final case class Stale(promise: Promise[Nothing, Option[String]], tasks: Set[String])

  /** A loop's task (`e951b73f/task-1/scenario 2` is `e951b73f/task-1`). */
  private def taskOf(loop: String): String = loop.split('/').take(2).mkString("/")

  /** How many times one run is superseded by edits before it's let finish. */
  val MaxSupersede = 3

  private val Checks = """(?i)(^|[\s/"])(test|testOnly|testFull|compile|build|check|lint|pytest|jest|vitest|tsc)([\s"]|$)""".r
  private val Mutates = """(?i)(^|\s)(git|rm|mv|publish|publishLocal|deploy|install|release|docker|kubectl)(\s|$)|>""".r

  /**
   * A run an edit makes stale: a build, a test run, a type check, by its tool (`compile`, `test`, `build_project`,
   * `get_file_problems`) or its command (`./gradlew test`, `sbt testOnly …`, `npm test`); never a command that may
   * change something beyond the build's output (git, a publish, a redirect into a file).
   */
  def supersedable(prefixed: String, arguments: Json.Obj): Boolean =
    val name = ToolResolver.baseName(prefixed)
    arguments.get("command").flatMap(_.asString) match
      case Some(c) => Checks.findFirstIn(c).isDefined && Mutates.findFirstIn(c).isEmpty
      case None    => name.contains("test") || name.contains("build") || Set("compile", "check", "get_file_problems")(name)


  /**
   * A shared run's result as one of its callers sees it: as it is when the run was the caller's own; otherwise wrapped
   * with what the caller needs to read it, since the run's exit status is about every test it ran: which of the
   * caller's tests passed or failed (from the test events in the output, Gradle's `Class > method() PASSED`), and that
   * the others are other agents' (their failures aren't the caller's to fix).
   */
  def forCaller(result: Json, own: List[String], all: List[String], finished: Boolean = false): Json =
    val others = all.filterNot(own.contains)
    if others.isEmpty && !finished then result
    else
      val output = textOf(result)
      val yours = own.map(t => t -> testStatus(output, t))
      val summary = yours.map((t, st) => s"$t ${st.getOrElse("(not in the output)")}").mkString(", ")
      val note =
        (if finished then "Your tests were already run, after your last edit, by a run of the shared test service; its result follows. "
         else "") +
        (if others.nonEmpty then s"This run also ran other agents' tests (${others.mkString(", ")}), so its exit status and failures may be " +
          "theirs, not yours: judge your part by your own tests. " else "") +
        (if yours.nonEmpty then s"Your tests: $summary." else "")
      Json.Obj("sharedRun" -> Json.Str(note.trim), "result" -> result)

  /** A test's status in a test run's output: `PASSED`, `FAILED` or `SKIPPED`, from Gradle's test events. */
  def testStatus(output: String, test: String): Option[String] =
    val (cls, method) = test.lastIndexOf('.') match
      case -1 => (test, "")
      case i  => (test.take(i), test.drop(i + 1))
    val simpleClass = cls.split('.').last
    val events = s"""(?m)^\\s*(?:[\\w.$$]*\\.)?${java.util.regex.Pattern.quote(simpleClass)} > ([^\\s(]+)\\(\\)[^\\n]*? (PASSED|FAILED|SKIPPED)\\s*$$""".r
    val matching = events.findAllMatchIn(output).filter(m => method.isEmpty || m.group(1) == method.stripSuffix("*") ||
      (method.endsWith("*") && m.group(1).startsWith(method.stripSuffix("*")))).map(_.group(2)).toList
    if matching.isEmpty then None
    else if matching.contains("FAILED") then Some("FAILED")
    else if matching.forall(_ == "SKIPPED") then Some("SKIPPED")
    else Some("PASSED")

  /** A tool result's text: a string, or every string in it joined. */
  private[soness] def textOf(json: Json): String = json match
    case Json.Str(s) => s
    case Json.Obj(fields) => fields.map(f => textOf(f._2)).mkString("\n")
    case Json.Arr(items) => items.map(textOf).mkString("\n")
    case other => other.toString

  /**
   * A test run's result as what it means: one that ran no test (no test matched its selection, none was due, every one
   * it found is ignored) succeeded at nothing, so it fails, saying so and what to do instead; any other is as it is.
   * Read from the shape build servers answer with (sbta's `summary` counts, and its log's own words).
   */
  def ranTests(name: String, result: Json): Either[String, Json] =
    val base = ToolResolver.baseName(name)
    if !base.contains("test") || base.contains("names") then Right(result)
    else
      val obj = result.asObject
      val summary = obj.flatMap(_.get("summary")).flatMap(_.asObject)
      def count(k: String) = summary.flatMap(_.get(k)).flatMap(_.asNumber).map(_.value.intValue)
      val log = obj.flatMap(o => o.get("log").orElse(o.get("text"))).flatMap(_.asString).getOrElse("")
      val unmatched = """No tests match the patterns: ([^\n]*)""".r.findFirstMatchIn(log).map(_.group(1).trim)
      val total = count("total")
      val skipped = count("skipped").getOrElse(0)
      val ran = total.map(_ - skipped)
      unmatched.map(p => s"This test run ran no tests: none matches $p. It checked nothing. List the project's test " +
          "classes (a test-names tool, or the test sources) and run those by their names; if none tests what's asked, " +
          "write one, or say so.")
        .orElse(Option.when(log.contains("No tests to run for"))("This test run ran no tests: none was due (an " +
          "incremental run only reruns failed, new or changed tests). It checked nothing. Run the tests by name, or all of them."))
        .orElse(Option.when(ran.contains(0) && total.exists(_ > 0))(s"This test run ran no tests: all ${total.getOrElse(0)} " +
          "it found are ignored (they're skipped: a paid or live test without its credentials, or turned off). It checked " +
          "nothing. Say what they need to run (their requirements are in the test or the project's docs), or ask for it."))
        .orElse(Option.when(total.contains(0))("This test run ran no tests. It checked nothing. Run the tests by name."))
        .toLeft(result)

  /**
   * Which of `tools` only read: annotated read-only, or (a routed IDE tool carries no annotations) named as a read,
   * `web_fetch` included. A build or a command is not (its runs are coordinated, and it may change files).
   */
  def reads(tools: Seq[ToolSpec]): String => Boolean =
    val annotated = tools.filter(_.annotations.exists(_.get("readOnlyHint").contains(Json.Bool(true)))).map(_.name).toSet
    name =>
      val base = ToolResolver.baseName(name)
      annotated(name) || base == "web_fetch" || RoutedTools.reads(base) || base == "open_file_in_editor"

  /** `zio` as the caller's tool call (its id, for a tool that adds to the call's updates: a terminal). */
  private def inContext[A](call: Option[com.jamesward.zioacp.schema.ToolCallId])(zio: Task[A]): Task[A] =
    ClientTools.currentCall.locally(call)(zio)

  /** Whether loops other than `loop` have called a tool in the last minute. */
  private def others(seen: Ref[Map[Option[String], java.time.Instant]], loop: Option[String]): UIO[Boolean] =
    Clock.instant.flatMap(now => seen.get.map(_.exists((l, at) => l != loop && at.isAfter(now.minusSeconds(60)))))

  /**
   * How the validation service combines runs: `merge` the calls waiting into fewer; `widen` a test run with the
   * pending tests of the loops that edited since theirs last ran, and with `widenFailing`, those whose last run failed
   * too (a failing test, a hanging one most of all, costs every run it's in).
   */
  // widening is off by default: in simulation it saved nothing, and in the 11-scenario eval it made runs of 20–54 s
  // out of 15 s ones (the tests it adds are another part's, often still failing at their timeout)
  final case class Policy(merge: Boolean = true, widen: Boolean = false, widenFailing: Boolean = false, maxTests: Option[Int] = None)

  /** A build, test or command call waiting to run, and the loop it's for. */
  final case class Run(name: String, arguments: Json.Obj, done: Promise[Throwable, Json], loop: Option[String] = None,
      gone: Promise[Nothing, Unit] = Unsafe.unsafely(Promise.unsafe.make[Nothing, Unit](FiberId.None)), via: Toolset = Toolset.empty,
      refs: Option[com.jamesward.zioacp.schema.ToolCallId] = None, restarts: Int = 0):
    /** Whether its caller still waits for it. */
    def wanted: UIO[Boolean] = gone.isDone.map(!_)

  /** A run that finished: what it selected (none for a command without a test selection), when it started, its exit. */
  private final case class Finished(name: String, selection: Option[(String, Set[String])], started: Long, exit: Exit[Throwable, Json])

  /**
   * The validation service's book: a clock that ticks with every edit, each loop's last edit, the tests each loop has
   * asked for (by command without its selection), and the runs that finished.
   */
  private final case class Ledger(
    clock: Long = 0,
    lastEdit: Map[Option[String], Long] = Map.empty,
    tests: Map[String, Map[String, Option[String]]] = Map.empty,
    finished: List[Finished] = Nil,
    failed: Set[String] = Set.empty,
  ):
    def edited(loop: Option[String]): Ledger = copy(clock = clock + 1, lastEdit = lastEdit.updated(loop, clock + 1))
    def requested(name: String, selection: Option[Selection], loop: Option[String]): Ledger =
      selection.fold(this): s =>
        copy(tests = tests.updated(s.base, tests.getOrElse(s.base, Map.empty) ++ s.tests.map(_ -> loop)))
    /** The run recorded, and its tests' outcomes (from its output) in `failed`. */
    def ran(run: Finished): Ledger =
      val output = run.exit.foldExit(_ => "", json => textOf(json))
      val statuses = run.selection.toList.flatMap(_._2).map(t => t -> testStatus(output, t))
      val nowFailed = statuses.collect { case (t, Some("FAILED")) => t }.toSet
      val nowPassed = statuses.collect { case (t, Some("PASSED")) => t }.toSet
      copy(finished = run :: finished.take(20), failed = failed -- nowPassed ++ nowFailed)

    /** A finished run that answers this request: the same command, all its tests, started after the loop's last edit. */
    def covering(name: String, selection: Option[Selection], loop: Option[String]): Option[Finished] =
      selection.flatMap: s =>
        val since = lastEdit.getOrElse(loop, 0L)
        finished.find(f => f.name == name && f.started >= since && f.selection.exists((base, ts) => base == s.base && s.tests.toSet.subsetOf(ts)))

    /** The tests of `base` whose loop has edited since a run including them last started. */
    def pending(base: String): List[String] =
      tests.getOrElse(base, Map.empty).toList.collect:
        case (test, loop) if !finished.exists(f => f.selection.exists((b, ts) => b == base && ts(test)) && f.started >= lastEdit.getOrElse(loop, 0L)) => test
      .sorted

  private final case class Widened(arguments: Json.Obj, selection: Option[(String, Set[String])], extra: List[String])

  /** A test command with the pending tests of the other loops added. */
  private def widen(name: String, arguments: Json.Obj, book: Ledger, failing: Boolean, maxTests: Option[Int] = None): Widened =
    arguments.get("command").flatMap(_.asString).flatMap(Selection.of) match
      case None => Widened(arguments, None, Nil)
      case Some(s) =>
        val extra = book.pending(s.base).filterNot(s.tests.contains).filter(t => failing || !book.failed(t))
          .take(maxTests.fold(Int.MaxValue)(m => (m - s.tests.size).max(0)))
        val all = s.tests ++ extra
        Widened(if extra.isEmpty then arguments else arguments.add("command", Json.Str(s.render(all))), Some(s.base -> all.toSet), extra)

  /**
   * Waiting runs as fewer calls: identical calls run once; terminal commands of one build tool that differ only in the
   * tests they select run as one command selecting all of them (Gradle's `--tests` filters; sbt's `testOnly` specs).
   * Each merged call keeps the first call's other arguments, with the longest timeout of those merged.
   */
  def merge(batch: Chunk[Run], maxTests: Option[Int] = None): List[(String, Json.Obj, List[Run])] =
    def command(r: Run) = r.arguments.get("command").flatMap(_.asString)
    def key(r: Run): (String, Json.Obj) = command(r).flatMap(Selection.of) match
      case Some(selection) => (r.name, Json.Obj(r.arguments.fields.filterNot(f => f._1 == "command" || f._1 == "timeout")*).add("command", Json.Str(selection.base)))
      case None            => (r.name, r.arguments)
    // each group of at most `maxTests` selected tests (a call's own are never split)
    def capped(runs: List[Run]): List[List[Run]] = maxTests.fold(List(runs)): m =>
      runs.foldLeft(List.empty[List[Run]]):
        case (current :: done, r) if (current :+ r).flatMap(command).flatMap(Selection.of).flatMap(_.tests).distinct.size <= m =>
          (current :+ r) :: done
        case (acc, r) => List(r) :: acc
      .reverse
    batch.toList.groupBy(key).values.toList.sortBy(rs => batch.indexOf(rs.head)).flatMap(capped).map: runs =>
      val first = runs.head
      if runs.size == 1 then (first.name, first.arguments, runs)
      else
        val selections = runs.flatMap(command).flatMap(Selection.of)
        val merged = selections.headOption.fold(first.arguments): s =>
          val timeout = runs.flatMap(_.arguments.get("timeout").flatMap(_.asNumber).map(_.value.longValue)).maxOption
          val withCommand = first.arguments.add("command", Json.Str(s.render(selections.flatMap(_.tests).distinct)))
          timeout.fold(withCommand)(t => withCommand.add("timeout", Json.Num(t)))
        (first.name, merged, runs)

  /** A test command's selected tests: `base` is the command without them. */
  final case class Selection(base: String, tests: List[String], render: List[String] => String)

  object Selection:
    private val Gradle = """^(\S*gradlew?)\s+(.*)$""".r
    private val SbtTestOnly = """^(\S*sbt)\s+(-\S+\s+)*"?testOnly\s+([^"]+)"?\s*$""".r

    def of(command: String): Option[Selection] = command.trim match
      case Gradle(tool, rest) if rest.contains("--tests") =>
        val tokens = rest.split("\\s+").toList
        val tests = tokens.sliding(2).collect { case "--tests" :: t :: Nil => t }.toList
        val others = tokens.zipWithIndex.filterNot((t, i) => t == "--tests" || (i > 0 && tokens(i - 1) == "--tests")).map(_._1)
        val base = (tool :: others).mkString(" ")
        Some(Selection(base, tests, ts => (base :: ts.map(t => s"--tests $t")).mkString(" ")))
      case SbtTestOnly(tool, flags, specs) =>
        val base = s"$tool ${Option(flags).getOrElse("")}".trim
        Some(Selection(s"$base testOnly", specs.trim.split("\\s+").toList, ts => s"$base \"testOnly ${ts.mkString(" ")}\""))
      case _ => None

