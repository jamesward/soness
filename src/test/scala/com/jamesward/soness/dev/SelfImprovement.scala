package com.jamesward.soness.dev

import com.jamesward.soness.*
import com.jamesward.zio_decisions.orchestration.ProcessTree
import com.jamesward.zioacp.schema.ToolKind
import zio.*
import zio.json.ast.Json
import zio.stream.ZStream

import java.nio.file.Path

/**
 * Self-improvement, the development build's extension: on soness's own project (a clone of its repository), the
 * [[Observer]] watches each session's turns and records findings, and each finding starts self-improvement by itself
 * (no command, no request): rounds that work on the new findings in the project itself, editing its files directly
 * with the session's own tools, with this process's debug log as evidence, run in the background as a task of the
 * session (so messages steer them, and `/stop` ends them), each judged by its gates ([[Improve.Local]]). One run at
 * a time per project: findings made during a run start another once it ends. `self_improve` starts rounds on request.
 */
final class SelfImprovement(steps: Path => Improve.Steps = root => Improve.Local(root)) extends Extension:

  override val endsTurn: Set[String] = Set(SelfImprovement.Tool.name)

  /** Automatic runs, by project. */
  private val auto: Ref[Map[Path, SelfImprovement.Auto]] = Unsafe.unsafely(Ref.unsafe.make(Map.empty))

  override def started(session: Extension.Session): UIO[Unit] =
    for
      enabled <- System.env(Observer.Variable).map(_.forall(_.trim.toLowerCase != "false")).orElseSucceed(true)
      _       <- ZIO.foreachDiscard(session.cwd.filter(r => enabled && Observer.isSoness(r)))(observe(session, _))
    yield ()

  override def tools(task: Extension.Task): Option[Toolset] =
    // not in a watch's own tasks: they act on what they're shown
    task.session.cwd.filter(r => task.route != "watch" && Observer.isSoness(r)).map: root =>
      new Toolset:
        val tools: Chunk[ToolSpec] = Chunk(SelfImprovement.Tool)
        def call(name: String, arguments: Json.Obj): zio.Task[Json] =
          ZIO.fromEither(Improve.options(arguments, Some(root))).mapError(IllegalArgumentException(_))
            .flatMap(improve(task, root, _)).map(text => Json.Obj("text" -> Json.Str(text)))

  // ---------- the observer ----------

  /** The observer, a process of the session's tree, subscribed to its turns before this returns. */
  private def observe(session: Extension.Session, root: Path): UIO[Unit] =
    for
      ready <- Promise.make[Nothing, ProcessTree.Id]
      found <- Ref.make(0)
      // the findings already recorded were earlier processes' to work on: automatic runs take the ones from now on
      known <- Observer.read(root).map(_.size)
      _     <- auto.update(m => if m.contains(root) then m else m.updated(root, SelfImprovement.Auto(taken = known)))
      // its own requests are logged under a session of their own, never taken for the session's
      _     <- session.fork(ZIO.logAnnotate("session", Observer.ownSession(session.id))(ZIO.logAnnotate("loop", Observer.Name)(
                 session.tree.child[Any, Throwable, Unit](Some(session.node), ProcessTree.Kind.Task, Observer.Name, Some("watching this session's turns")): node =>
                   ZIO.scoped:
                     session.ended.flatMap: turns =>
                       ready.succeed(node) *>
                         ZStream.fromQueue(turns)
                         .mapZIO(turn => noticed(session, root, node, found, turn)
                           .catchAllCause(c => ZIO.logWarningCause(s"observer: turn ${turn.id} couldn't be looked at", c)))
                         .runDrain
                 .catchAllCause(c => ZIO.logInfoCause("observer ended", c)))))
      node  <- ready.await
      _     <- ZIO.logInfo(s"observer: watching session ${session.id}'s turns, as process [$node]")
    yield ()

  /**
   * A turn that ended: judged, and for one worth it, analyzed read-only and silently, recorded as a finding, and
   * worked on ([[kick]]).
   */
  private def noticed(session: Extension.Session, root: Path, node: ProcessTree.Id, found: Ref[Int], turn: Transcript.Turn): zio.Task[Unit] =
    val seen = Transcript.render(turn, 12000)
    for
      harness <- session.harness
      p       <- harness.judge(s"A turn of soness working on its own project just ended:\n$seen", Observer.Question, Observer.Yes, Observer.No)
      _       <- ZIO.logInfo(f"observer: turn ${turn.id} ended (${turn.ending}); worth looking into: p $p%.2f")
      _       <- (analyze(session, root, node, found, turn, p) *> kick(session, root)).when(p >= Observer.Threshold)
    yield ()

  /** A read-only, silent analysis of `turn`, recorded as a finding. */
  private def analyze(session: Extension.Session, root: Path, node: ProcessTree.Id, found: Ref[Int],
      turn: Transcript.Turn, p: Double): zio.Task[Observer.Finding] =
    for
      harness <- session.harness
      // its calls aren't the session's: unreported, so the client isn't shown them
      result  <- harness.run(Observer.request(turn), SelfImprovement.readOnly(session.toolset), Nil,
                   Some(s"What the turn shows:\n${Transcript.render(turn, 12000)}"))
      at      <- Clock.instant.map(_.toString)
      finding  = Observer.Finding(at, session.id, turn.id, turn.request, turn.ending, p, Observer.summary(result.answer), result.answer)
      _       <- Observer.record(root, finding)
      n       <- found.updateAndGet(_ + 1)
      _       <- session.tree.touch(node, Some(s"$n finding(s); the last: ${SelfImprovement.short(finding.summary)}"))
      _       <- ZIO.logInfo(s"observer: finding on ${finding.turn}: ${finding.summary}")
    yield finding

  // ---------- automatic self-improvement ----------

  /**
   * New findings in `root`: a run starts on them, unless one runs already (then another starts once it ends) or
   * automatic runs are off (`SONESS_SELF_IMPROVE=false`).
   */
  private def kick(session: Extension.Session, root: Path): UIO[Unit] =
    ZIO.unless(sys.env.get(SelfImprovement.Variable).exists(_.trim.equalsIgnoreCase("false"))):
      auto.modify: m =>
        val a = m.getOrElse(root, SelfImprovement.Auto())
        if a.running then (false, m.updated(root, a.copy(pending = true))) else (true, m.updated(root, a.copy(running = true, pending = false)))
      .flatMap(start => startAuto(session, root).when(start))
    .unit

  /** A run on the findings no run has taken yet, in the background; once it ends, the next if findings came meanwhile. */
  private def startAuto(session: Extension.Session, root: Path): UIO[Unit] =
    // a fiber of the session's, interruptible: run in a run's finalizer it would be uninterruptible, and so would the
    // next run it starts (no /stop, and the session, and the process, couldn't close)
    val finished = session.fork(ZIO.interruptible(
      auto.modify(m => m.get(root).fold((false, m))(a => (a.pending, m.updated(root, a.copy(running = false, pending = false)))))
        .flatMap(again => kick(session, root).when(again))))
    (for
      all   <- Observer.read(root)
      taken <- auto.get.map(_.get(root).fold(0)(_.taken))
      fresh  = all.drop(taken)
      _     <- auto.update(m => m.updatedWith(root)(_.map(_.copy(taken = all.size))))
      o      = SelfImprovement.autoOptions(Logging.current)
      _     <- if fresh.isEmpty then finished
               else session.background(s"${Improve.Name}: from ${fresh.size} new finding(s) of the observer", SelfImprovement.Label)(bg =>
                      SelfImprovement.exclusive(root)(rounds(session, root, o, bg, Some(fresh), chatty = false)).ensuring(finished))
                 .tap((id, node) => ZIO.logInfo(s"self-improvement: started by itself on ${fresh.size} finding(s), task $id, process [$node]"))
    yield ()).catchAllCause(c => ZIO.logWarningCause("self-improvement couldn't start", c) *> finished)

  // ---------- self-improvement rounds ----------

  private def improve(task: Extension.Task, root: Path, o: Improve.Options): zio.Task[String] =
    val request = s"${Improve.Name}: ${o.rounds} round${if o.rounds == 1 then "" else "s"}" + o.log.fold("")(l => s", with $l") +
      (if o.eval then "" else ", no eval")
    for
      running <- task.session.tree.subtree(task.session.node).map(_.find(n => n.state == ProcessTree.State.Running && n.kind == ProcessTree.Kind.Task && n.detail.exists(_.startsWith(Improve.Name))))
      out     <- running.fold(
                   task.background(request, SelfImprovement.Label)(bg => SelfImprovement.exclusive(root)(rounds(task.session, root, o, bg, None, chatty = true))).map: (id, node) =>
                     s"Started: $request, in the background, as task $id (process [$node]). Each round's progress and verdict " +
                       "come as messages in this session. While it runs, messages to this session steer it (every round reads " +
                       s"them), a question about it is answered from its progress, and asking to stop it (or /stop $node) ends " +
                       "it. This request is done: reply that it runs; nothing else is needed now.")(n =>
                   ZIO.succeed(s"Self-improvement already runs, as process [${n.id}]. This request is done: reply that it runs."))
    yield out

  /**
   * The rounds, one after another until they're done or one makes no change; a summary of what each did. They work
   * on `findings` (all the observer's, newest last, when none are given); `chatty`, each round's steps are said too.
   */
  private def rounds(session: Extension.Session, root: Path, o: Improve.Options, bg: Extension.Background,
      findings: Option[List[Observer.Finding]], chatty: Boolean): zio.Task[String] =
    // a round's steps are said only when asked for (`self_improve`); its plan and summary always, marked as its own
    val say = (text: String) => bg.say(text).when(chatty).unit
    val here = steps(root)
    def step[A](name: String)(work: zio.Task[A]): zio.Task[A] = session.tree.child(Some(bg.node), ProcessTree.Kind.Call, name)(_ => work)
    def go(n: Int, log: Option[Path], history: List[String]): zio.Task[List[String]] =
      if n > o.rounds then ZIO.succeed(history)
      else
        for
          _       <- say(s"Self-improvement round $n of ${o.rounds}: starting.")
          round   <- step(s"round $n: prepare")(here.prepare(o, n, log, history))
          found   <- findings.fold(Observer.read(root))(ZIO.succeed(_))
          worked  <- session.tree.child(Some(bg.node), ProcessTree.Kind.Loop, s"round-$n", Some(s"in ${round.project}"))(_ =>
                       work(session, round, bg, Observer.context(found))).either
          reply    = worked.toOption.map(_.answer)
          _       <- say(s"Round $n's work: ${worked.fold(e => s"it failed (${Option(e.getMessage).getOrElse(e.toString)})", _.answer)}")
          _       <- say(s"Round $n: checking the change (its gates).")
          // the round's change is its own edits: this background task's, in the project's event log
          verdict <- step(s"round $n: gates")(here.gate(o, round, Improve.By(session.id, bg.id)))
          _       <- say(s"Round $n: ${verdict.describe}. Its record: ${round.out}")
          _       <- ZIO.logInfo(s"self-improvement round $n: ${verdict.describe} (${round.out})")
          line     = Improve.history(verdict, reply)
          rest    <- if verdict.noChange then ZIO.succeed(history :+ line) else go(n + 1, verdict.next.orElse(log), history :+ line)
        yield rest
    val plan = findings.fold(Observer.read(root).map(_.takeRight(5)))(ZIO.succeed(_)).flatMap: found =>
      bg.say(SelfImprovement.plan(found, o, findings.isDefined))
    plan *> go(1, o.log, Nil).map: done =>
      done.zipWithIndex.map((line, i) => s"${i + 1}. $line").mkString(s"Self-improvement done (${done.size} round(s)):\n", "\n",
        s"\nThe changes are in the project's files (a rejected round's too: its verdict says why); they take effect once " +
          s"soness is staged and restarted. Each round's record: ${root.resolve("target/improve")}")

  /**
   * A round's work: its request worked on in the project, with the session's own tools (its IDE's, its build server's:
   * edits land in the project's files), skills and context (the observer's findings first), and the task's steering,
   * read as it goes; its calls reported as the task's.
   */
  private def work(session: Extension.Session, round: Improve.Round, bg: Extension.Background, findings: Option[String])
      : zio.Task[HarnessResult] =
    for
      (skills, known) <- session.contextOf(round.project, Nil)
      harness         <- session.harness
      context          = (findings.toList ++ known.toList).reduceOption(_ + "\n\n" + _)
      result          <- bg.loop(s"round-${round.round}")(harness.run(round.prompt, bg.reported(session.toolset), skills, context,
                           bg.steering))
    yield result

object SelfImprovement:

  /** What marks a run's messages and tool calls in the chat. */
  val Label = "self-improvement"

  /** What a run will do, said before it starts: the findings it works on and its steps. */
  def plan(findings: List[Observer.Finding], o: Improve.Options, automatic: Boolean): String =
    val from = if automatic then s"the observer's ${findings.size} new finding(s)" else s"the observer's findings (${findings.size} newest shown)"
    val listed = findings.map(f => s"- ${f.turn}: ${f.summary}").mkString("\n")
    s"Starting, from $from:\n$listed\n\nPlan: ${o.rounds} round(s). Each picks the finding that matters most, finds its cause in " +
      "soness's code (or a source dependency's), changes it with a test, and runs the relevant tests; then its gates judge " +
      s"it (a code change with a test, the offline suite${if o.eval then ", the quick eval" else ""}). Edits go to the project's " +
      "files directly. Messages to this session steer it; /stop ends it."

  /**
   * A project's automatic runs: whether one runs in this process, whether findings came during it (another runs after
   * it), and how many of the project's findings runs have taken (the rest are new).
   */
  final case class Auto(running: Boolean = false, pending: Boolean = false, taken: Int = 0)

  /** `SONESS_SELF_IMPROVE=false`: findings are recorded, but no run starts by itself. */
  val Variable = "SONESS_SELF_IMPROVE"

  /**
   * An automatic run's options: `SONESS_SELF_IMPROVE_ROUNDS` rounds (default 1), gated on the quick eval only with
   * `SONESS_SELF_IMPROVE_EVAL=true` (it's paid and takes about 15 minutes a round), with `log` (this process's debug log)
   * as evidence.
   */
  def autoOptions(log: Option[Path]): Improve.Options =
    def env(k: String) = sys.env.get(k).map(_.trim.toLowerCase).filter(_.nonEmpty)
    Improve.Options(log.filter(java.nio.file.Files.isRegularFile(_)),
      env("SONESS_SELF_IMPROVE_ROUNDS").flatMap(_.toIntOption).filter(n => n >= 1 && n <= 20).getOrElse(1),
      eval = env("SONESS_SELF_IMPROVE_EVAL").contains("true"), baseline = None)

  /**
   * `work` while this process holds the project's self-improvement lock (`target/improve/.lock`): rounds edit the
   * project, so a run of another soness process (another ACP client's) on it means none here.
   */
  def exclusive(root: Path)(work: zio.Task[String]): zio.Task[String] =
    val file = root.resolve("target").resolve("improve").resolve(".lock")
    ZIO.scoped:
      ZIO.acquireRelease(ZIO.attemptBlocking {
        java.nio.file.Files.createDirectories(file.getParent)
        val channel = java.nio.channels.FileChannel.open(file, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE)
        // held already in this process (a run another session started) is busy too, as another process's is
        (channel, try Option(channel.tryLock()) catch case _: java.nio.channels.OverlappingFileLockException => None)
      })((channel, lock) => ZIO.attemptBlocking { lock.foreach(_.release()); channel.close() }.ignore)
        .flatMap: (_, lock) =>
          if lock.isEmpty then ZIO.succeed(s"Self-improvement didn't run: another soness process is improving this project ($file).")
          else work

  val Tool = ToolSpec("self_improve", "Starts self-improvement of this project (soness, the agent harness) from what the " +
    "observer found while soness worked on itself (and a soness debug log, when one is given): rounds that run in the " +
    "background as a task of this session, each fixing a finding's cause in this project's files with a test, and " +
    "judged by the project's gates (a code change with a test, the offline suite passes, the quick eval doesn't regress). " +
    "It returns at once; messages while it runs steer it, and it runs until " +
    "its rounds end or it's stopped. For requests like \"improve yourself from what the observer found\" or \"run 3 " +
    "self-improvement rounds\".",
    Json.Obj(
      "type" -> Json.Str("object"),
      "properties" -> Json.Obj(
        "log" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("A soness debug log to improve from too (optional)")),
        "rounds" -> Json.Obj("type" -> Json.Str("integer"), "description" -> Json.Str("How many rounds (1 to 20; default 1)")),
        "eval" -> Json.Obj("type" -> Json.Str("boolean"),
          "description" -> Json.Str("Gate on the quick eval too, about 15 minutes a round (default true)")),
        "baseline" -> Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("The eval label to compare with")),
      ),
      "required" -> Json.Arr(),
    ), None)

  /** `toolset`'s reading and searching tools only. */
  def readOnly(toolset: Toolset): Toolset =
    new Toolset:
      val tools: Chunk[ToolSpec] =
        toolset.tools.filter(t => Set(ToolKind.Read, ToolKind.Search).contains(ToolCallView.kind(t.name, t.annotations)))
      def call(name: String, arguments: Json.Obj): zio.Task[Json] =
        if tools.exists(_.name == name) then toolset.call(name, arguments)
        else ZIO.fail(IllegalArgumentException(s"$name isn't offered here: the observer only reads"))

  def short(text: String): String = if text.length <= 80 then text else text.take(77) + "…"

/** The development build of soness (`Test / stage`): production soness with self-improvement. */
object Main extends ZIOAppDefault:
  override val bootstrap = Logging.fromArgs("soness-dev")
  override def gracefulShutdownTimeout: Duration = 10.seconds
  def run = Soness.serve(List(SelfImprovement())).catchSome { case _: String => exit(ExitCode.failure) }
