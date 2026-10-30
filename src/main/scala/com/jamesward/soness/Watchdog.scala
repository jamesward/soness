package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.{Overseer, ProcessTree}
import com.jamesward.zio_decisions.orchestration.Overseer.{Judgment, Stalled, Verdict}
import zio.*

/**
 * Watches a session's tool calls (in the agent's process tree, the calls below the session's node) for ones that hang, and asks Jev what to do about them: a call running `stall`
 * with no sign of life is judged (zio-decisions' `Overseer`: keep waiting, or stop it, from what the call is and how
 * long calls like it took). Stopped, it's a failed step: its loop goes on and can try otherwise; the user is told. Judged
 * worth waiting for, it's judged again once it has run twice as long. A call is stopped at `limit` whatever the
 * judgment, and when Jev can't be asked the watchdog only reports it. `SONESS_STALL_SECONDS` (default 120) and
 * `SONESS_CALL_LIMIT_SECONDS` (default 1800; 0 for none) set them.
 */
object Watchdog:
  final case class Limits(stall: Duration, limit: Option[Duration])

  /** Who decides about stalled calls: Jev, through the harness; by default, wait (and report). */
  type Judge = List[Stalled] => Task[List[Judgment]]

  val waitAlways: Judge = stalled => ZIO.succeed(stalled.map(s => Judgment(s.node.id, Verdict.Wait, 0.0)))

  val limits: UIO[Limits] =
    for
      stall <- System.env("SONESS_STALL_SECONDS").map(_.flatMap(_.toLongOption)).orElseSucceed(None)
      limit <- System.env("SONESS_CALL_LIMIT_SECONDS").map(_.flatMap(_.toLongOption)).orElseSucceed(None)
    yield Limits(stall.getOrElse(120L).seconds, limit.getOrElse(1800L) match
      case 0L => None
      case s  => Some(s.seconds))

  /**
   * Watches `tree` for the life of the fiber: told of each call as it starts, it sets that call's timer (the stall
   * time, then twice the time at each judgment to wait, and the limit), and the timer goes when the call ends: nothing
   * polls. A call still running when its timer fires is judged by Jev: stopped (a failed step), or waited for; `note`
   * tells the user.
   */
  def watch(tree: ProcessTree, limits: Limits, note: String => UIO[Unit], judge: Judge = waitAlways, under: Option[ProcessTree.Id] = None): UIO[Nothing] =
    ZIO.scoped:
      tree.started.flatMap: starts =>
        starts.take.flatMap: node =>
          ZIO.whenZIO(ZIO.succeed(node.kind == ProcessTree.Kind.Call) && below(tree, node, under))(
            watchCall(tree, node.id, limits, note, judge).forkDaemon)
        .forever

  /** Whether `node` is below `ancestor` (anything is, with none): a session's watchdog watches its own calls in a shared tree. */
  private def below(tree: ProcessTree, node: ProcessTree.Node, ancestor: Option[ProcessTree.Id]): UIO[Boolean] =
    ancestor.fold(ZIO.succeed(true)): a =>
      tree.snapshot.map: all =>
        val byId = all.map(n => n.id -> n).toMap
        Iterator.iterate(node.parent)(_.flatMap(byId.get).flatMap(_.parent)).takeWhile(_.isDefined).exists(_.contains(a))

  /** One call's watch: its timer, and what happens when it fires, until the call ends. */
  private def watchCall(tree: ProcessTree, id: ProcessTree.Id, limits: Limits, note: String => UIO[Unit], judge: Judge): UIO[Unit] =
    val limit = limits.limit.map(_.toSeconds)
    // `due`: the call's running time (seconds since it started) at which to look at it next
    def at(due: Long, elapsed: Long, waited: Int): UIO[Unit] =
      val next = limit.fold(due)(due.min)
      tree.awaitEnd(id).timeout(Duration.fromSeconds((next - elapsed).max(0))).flatMap:
        case Some(_) => ZIO.unit
        case None if limit.exists(_ <= next) =>
          tree.stop(id, s"ran past the ${limit.get}s limit with no answer") *>
            tree.snapshot.map(_.find(_.id == id)).flatMap(n =>
              note(s"⚠ Stopped [$id] ${n.fold("a call")(_.name)}: it ran past ${limit.get}s with no answer; its task goes on."))
        case None =>
          Overseer.stalled(tree, Duration.Zero, Map(id -> waited)).map(_.find(_.node.id == id)).flatMap:
            case None => ZIO.unit
            case Some(s) =>
              val what = s"[$id] ${s.node.name}${s.node.detail.fold("")(d => s" ($d)")}"
              judge(List(s)).catchAll(e => ZIO.logWarning(s"couldn't ask Jev about [$id]: ${e.getMessage}") *> waitAlways(List(s)).orDie)
                .map(_.headOption).flatMap:
                  case Some(j) if j.verdict == Verdict.Stop =>
                    tree.stop(id, s"no answer for ${s.runningSeconds}s; Jev judged it stuck") *>
                      ZIO.logInfo(f"watchdog: stopped $what after ${s.runningSeconds}s (stop ${j.stopProbability}%.2f)") *>
                      note(s"⚠ Stopped $what: no answer for ${s.runningSeconds}s" +
                        s.similar.maxOption.fold("")(m => s", where calls like it took at most ${m}s") + "; its task goes on.")
                  case j =>
                    ZIO.logInfo(f"watchdog: waiting for $what at ${s.runningSeconds}s (stop ${j.fold(0.0)(_.stopProbability)}%.2f)") *>
                      note(s"⏳ $what has had no answer for ${s.runningSeconds}s; it looks worth waiting for, so it goes on. " +
                        s"/stop $id stops it (its task goes on)." + limits.limit.fold("")(l => s" It will be stopped at ${l.toSeconds}s."))
                        .when(waited == 0) *>
                      // judged again once it has run twice as long
                      at(next * 2, next, waited + 1)
    at(limits.stall.toSeconds, 0, 0)
