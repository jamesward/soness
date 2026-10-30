package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ProcessTree
import com.jamesward.zio_decisions.orchestration.ProcessTree.Kind
import zio.*
import zio.test.*

object WatchdogSpec extends ZIOSpecDefault:
  def spec = suite("Watchdog")(
    test("a call with no answer is reported once at the stall time, and stopped at the limit; its task goes on") {
      for
        tree   <- ProcessTree.make
        notes  <- Ref.make(Vector.empty[String])
        _      <- Watchdog.watch(tree, Watchdog.Limits(60.seconds, Some(300.seconds)), n => notes.update(_ :+ n)).fork
        task   <- tree.child(None, Kind.Task, "task-1")(_ =>
                    ProcessTree.run(Kind.Call, "build_project")(ZIO.never).either.map(_.left.map(_.getMessage))).fork
        _      <- tree.running.repeatUntil(_.exists(_.kind == Kind.Call))
        _      <- TestClock.adjust(70.seconds)
        early  <- notes.get
        _      <- TestClock.adjust(70.seconds)
        again  <- notes.get
        _      <- TestClock.adjust(200.seconds)
        result <- task.join
        late   <- notes.get
      yield assertTrue(
        early.size == 1, early.head.contains("build_project has had no answer") && early.head.contains("/stop"),
        again.size == 1,
        result.left.exists(_.contains("ran past the 300s limit")),
        late.exists(_.startsWith("⚠ Stopped")),
      )
    },
    test("Jev's stop on a stalled call stops it (a failed step, with the reason); its wait is asked again at twice the time") {
      for
        tree   <- ProcessTree.make
        notes  <- Ref.make(Vector.empty[String])
        asked  <- Ref.make(Vector.empty[(String, Long)])
        // Jev waits for the build until it has run 4 minutes, and stops the read at once
        judge   = (stalled: List[com.jamesward.zio_decisions.orchestration.Overseer.Stalled]) =>
                    asked.update(_ ++ stalled.map(s => s.node.name -> s.runningSeconds)).as(stalled.map: s =>
                      val stop = s.node.name == "read_file" || s.runningSeconds >= 240
                      com.jamesward.zio_decisions.orchestration.Overseer.Judgment(s.node.id,
                        if stop then com.jamesward.zio_decisions.orchestration.Overseer.Verdict.Stop
                        else com.jamesward.zio_decisions.orchestration.Overseer.Verdict.Wait, if stop then 0.9 else 0.1))
        _      <- Watchdog.watch(tree, Watchdog.Limits(60.seconds, Some(3600.seconds)), n => notes.update(_ :+ n), judge).fork
        build  <- tree.child(None, Kind.Task, "task-1")(_ =>
                    ProcessTree.run(Kind.Call, "build_project")(ZIO.never).either.map(_.left.map(_.getMessage))).fork
        read   <- tree.child(None, Kind.Task, "task-2")(_ =>
                    ProcessTree.run(Kind.Call, "read_file")(ZIO.never).either.map(_.left.map(_.getMessage))).fork
        _      <- tree.running.repeatUntil(_.count(_.kind == Kind.Call) == 2)
        _      <- TestClock.adjust(10.seconds).repeatN(30)
        r      <- read.join
        b      <- build.join
        seen   <- asked.get
        all    <- notes.get
      yield assertTrue(
        r.left.exists(_.contains("Jev judged it stuck")), b.left.exists(_.contains("Jev judged it stuck")),
        // the build: asked at about 60 s, again at about 120 s and 240 s, then stopped
        seen.filter(_._1 == "build_project").map(_._2).size == 3,
        seen.filter(_._1 == "build_project").last._2 >= 240,
        all.exists(_.startsWith("⏳ [")), all.count(_.startsWith("⚠ Stopped")) == 2,
      )
    },
  ) @@ TestAspect.timeout(60.seconds)
