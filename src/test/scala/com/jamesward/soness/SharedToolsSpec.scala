package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ParallelLoop
import zio.*
import zio.json.ast.Json
import zio.test.*

/** The tools as loops working at the same time share them ([[SharedTools.coordinated]]). */
object SharedToolsSpec extends ZIOSpecDefault:

  private given CanEqual[Json, Json] = CanEqual.derived

  private def run(command: String, extra: (String, Json)*) =
    SharedTools.Run("execute_terminal_command", Json.Obj(("command" -> Json.Str(command)) +: extra*), null)

  private def inner(result: Json): Json = result.asObject.flatMap(_.get("result")).getOrElse(result)
  private def note(result: Json): Option[String] = result.asObject.flatMap(_.get("sharedRun")).flatMap(_.asString)

  private def commands(batch: SharedTools.Run*) =
    SharedTools.merge(Chunk.from(batch)).map((_, arguments, runs) => (arguments.get("command").flatMap(_.asString).getOrElse(""), runs.size))

  def spec = suite("coordinated tools")(
    test("a test run that ran no test fails, saying why: no match, none due, all ignored; one that ran tests is as it is") {
      def sbta(log: String, total: Int, skipped: Int = 0) = Json.Obj("command" -> Json.Str("testOnly *X*"), "success" -> Json.Bool(true),
        "log" -> Json.Str(log), "summary" -> Json.Obj("total" -> Json.Num(total), "passed" -> Json.Num(total - skipped),
          "failed" -> Json.Num(0), "skipped" -> Json.Num(skipped)))
      val unmatched = SharedTools.ranTests("test", sbta("[error] No tests match the patterns: *openai*, *Integration*\n", 0))
      val notDue = SharedTools.ranTests("test", sbta("[info] No tests to run for Test / testQuick\n", 0))
      val ignored = SharedTools.ranTests("sbta.test", sbta("+ live end to end\n  - every model - ignored: 1\n", 1, skipped = 1))
      val someRan = SharedTools.ranTests("test", sbta("+ a\n  + b\n  - c - ignored: 1\n", 3, skipped = 1))
      val notATest = SharedTools.ranTests("compile", sbta("", 0))
      assertTrue(
        unmatched.left.exists(m => m.contains("none matches *openai*, *Integration*") && m.contains("test-names")),
        notDue.left.exists(_.contains("none was due")),
        ignored.left.exists(_.contains("all 1 it found are ignored")),
        someRan.isRight, notATest.isRight, SharedTools.ranTests("test-names", sbta("", 0)).isRight,
      )
    },
    test("a test run another task's edit lands during is stopped and run again, its caller answered by the new run; its own task's edit doesn't") {
      def as[A](loop: String)(zio: Task[A]) = ParallelLoop.currentLoop.locally(Some(loop))(zio)
      for
        ran      <- Ref.make(Vector.empty[String])
        stopped  <- Ref.make(0)
        started  <- Queue.unbounded[Unit]
        release  <- Promise.make[Nothing, Unit]
        toolset   = new Toolset:
                      val tools = Chunk.empty
                      def call(name: String, arguments: Json.Obj): Task[Json] =
                        val command = arguments.get("command").flatMap(_.asString).getOrElse(name)
                        ran.updateAndGet(_ :+ command).flatMap: all =>
                          if !command.contains("test") then ZIO.succeed(Json.Str("edited"))
                          else (started.offer(()) *> release.await.onInterrupt(stopped.update(_ + 1))).as(Json.Str(s"run ${all.count(_.contains("test"))}"))
        out      <- ZIO.scoped:
                      for
                        calls  <- SharedTools.coordinated(toolset, Duration.Zero)
                        test    = as("s1/task-1/main")(calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test"))))
                        first  <- test.fork
                        _      <- started.take
                        // the same task's edit (one of its parts): the run goes on
                        _      <- as("s1/task-1/part b")(calls.call("apply_patch", Json.Obj()))
                        // another session's edit: the run is stale
                        _      <- as("s2/task-1/main")(calls.call("apply_patch", Json.Obj()))
                        _      <- started.take
                        _      <- release.succeed(())
                        result <- first.join
                      yield result
        count    <- stopped.get
        commands <- ran.get
      yield assertTrue(out == Json.Str("run 2"), count == 1, commands.count(_.contains("test")) == 2,
        SharedTools.supersedable("execute_terminal_command", Json.Obj("command" -> Json.Str("./sbt \"testOnly A\""))),
        !SharedTools.supersedable("execute_terminal_command", Json.Obj("command" -> Json.Str("git commit -m test"))),
        SharedTools.supersedable("compile", Json.Obj()), !SharedTools.supersedable("execute_terminal_command", Json.Obj("command" -> Json.Str("ls"))))
    } @@ TestAspect.withLiveClock,
    test("waiting test commands of one build tool merge into one selecting all their tests; others stay apart") {
      assertTrue(
        commands(run("./gradlew test --tests MainTest.scenario1"), run("./gradlew test --tests MainTest.scenario2")) ==
          List("./gradlew test --tests MainTest.scenario1 --tests MainTest.scenario2" -> 2),
        commands(run("./sbt -batch \"testOnly shop.A\""), run("./sbt -batch \"testOnly shop.B shop.A\"")) ==
          List("./sbt -batch \"testOnly shop.A shop.B\"" -> 2),
        commands(run("ls"), run("ls"), run("./gradlew build")) == List("ls" -> 2, "./gradlew build" -> 1),
        commands(run("./gradlew test --tests A"), run("./gradlew clean test --tests B")) ==
          List("./gradlew test --tests A" -> 1, "./gradlew clean test --tests B" -> 1),
        // the longest timeout of those merged
        SharedTools.merge(Chunk(run("./gradlew test --tests A", "timeout" -> Json.Num(1000)), run("./gradlew test --tests B", "timeout" -> Json.Num(5000))))
          .head._2.get("timeout").contains(Json.Num(5000)),
      )
    },
    test("a command waiting while another runs is merged with the others waiting, and each call gets the merged run's output") {
      for
        ran     <- Ref.make(Vector.empty[String])
        gate    <- Promise.make[Nothing, Unit]
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val command = arguments.get("command").flatMap(_.asString).getOrElse(name)
                       ran.update(_ :+ command) *> gate.await.when(command.contains("first")) .as(Json.Str(s"ran $command"))
        results <- ZIO.scoped:
                     for
                       calls  <- SharedTools.coordinated(toolset, Duration.Zero)
                       first  <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests first"))).fork
                       _      <- ran.get.repeatUntil(_.nonEmpty)
                       a      <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests A"))).fork
                       b      <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests B"))).fork
                       read   <- calls.call("read_file", Json.Obj())
                       _      <- ZIO.yieldNow.repeatN(100) *> gate.succeed(())
                       all    <- ZIO.foreach(List(first, a, b))(_.join)
                     yield (read, all)
        commands <- ran.get
        merged    = Set("./gradlew test --tests A --tests B", "./gradlew test --tests B --tests A")
      yield assertTrue(
        // the read didn't wait for the run
        results._1 == Json.Str("ran read_file"),
        // the two waiting runs merged into one (in the order they came, which forked callers don't fix)
        commands.take(2) == Vector("./gradlew test --tests first", "read_file"), commands.size == 3,
        merged.contains(commands(2)),
        // each caller gets the merged run's result, with a note that the run had the other's test too
        results._2.drop(1).forall(r => inner(r) == Json.Str(s"ran ${commands(2)}") && note(r).exists(_.contains("other agents' tests"))),
      )
    },
    test("a test run is widened with the tests of parts edited since; a part whose tests a later run covered gets its output") {
      def as[A](loop: String)(zio: Task[A]) = ParallelLoop.currentLoop.locally(Some(loop))(zio)
      def test(name: String) = Json.Obj("command" -> Json.Str(s"./gradlew test --tests $name"))
      for
        ran     <- Ref.make(Vector.empty[String])
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val command = arguments.get("command").flatMap(_.asString).getOrElse(name)
                       ran.update(_ :+ command).as(Json.Str(s"ran $command"))
        outputs <- ZIO.scoped:
                     for
                       calls <- SharedTools.coordinated(toolset, Duration.Zero, SharedTools.Policy(widen = true))
                       call   = (n: String, a: Json.Obj) => calls.call(n, a)
                       _     <- as("a")(call("apply_patch", Json.Obj()))
                       a1    <- as("a")(call("execute_terminal_command", test("A")))
                       _     <- as("b")(call("apply_patch", Json.Obj()))
                       b1    <- as("b")(call("execute_terminal_command", test("B")))
                       // a edits again, b asks for B again: a's A is pending, so b's run includes it
                       _     <- as("a")(call("apply_patch", Json.Obj()))
                       _     <- as("b")(call("apply_patch", Json.Obj()))
                       b2    <- as("b")(call("execute_terminal_command", test("B")))
                       // a's A ran after a's last edit: answered without a run
                       a2    <- as("a")(call("execute_terminal_command", test("A")))
                     yield List(a1, b1, b2, a2)
        commands <- ran.get.map(_.filter(_.contains("gradlew")))
      yield assertTrue(
        commands == Vector("./gradlew test --tests A", "./gradlew test --tests B", "./gradlew test --tests B --tests A"),
        inner(outputs.last) == Json.Str("ran ./gradlew test --tests B --tests A"),
        note(outputs.last).exists(_.contains("already run, after your last edit")),
      )
    },
    test("a waiting run whose caller was interrupted isn't made; the others' still are") {
      for
        ran     <- Ref.make(Vector.empty[String])
        gate    <- Promise.make[Nothing, Unit]
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val command = arguments.get("command").flatMap(_.asString).getOrElse(name)
                       ran.update(_ :+ command) *> gate.await.when(command == "first").as(Json.Str(command))
        _       <- ZIO.scoped:
                     for
                       calls <- SharedTools.coordinated(toolset, Duration.Zero)
                       first <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("first"))).fork
                       _     <- ran.get.repeatUntil(_.nonEmpty)
                       gone  <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("gone"))).fork
                       kept  <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("kept"))).fork
                       _     <- ZIO.yieldNow.repeatN(50)
                       _     <- gone.interrupt
                       _     <- gate.succeed(())
                       _     <- first.join *> kept.join
                     yield ()
        commands <- ran.get
      yield assertTrue(commands == Vector("first", "kept"))
    },
    test("a shared run's result as a caller sees it: its own tests' status, and a note that the others' are theirs") {
      val output = Json.Obj("command_exit_code" -> Json.Num(1), "command_output" -> Json.Str(
        "> Task :test\n\nMainTest > scenario1() STARTED\n\nMainTest > scenario1() FAILED\n    java.lang.Exception\n\n" +
          "MainTest > scenario5() STARTED\n\nMainTest > scenario5() PASSED\n\n2 tests completed, 1 failed\n"))
      val mine = SharedTools.forCaller(output, List("MainTest.scenario5"), List("MainTest.scenario5", "MainTest.scenario1"))
      val alone = SharedTools.forCaller(output, List("MainTest.scenario1"), List("MainTest.scenario1"))
      assertTrue(
        alone == output,
        mine.asObject.flatMap(_.get("sharedRun")).flatMap(_.asString).exists(n =>
          n.contains("Your tests: MainTest.scenario5 PASSED") && n.contains("other agents' tests (MainTest.scenario1)")),
        mine.asObject.flatMap(_.get("result")).contains(output),
        SharedTools.testStatus(output.get("command_output").flatMap(_.asString).getOrElse(""), "MainTest.scenario1").contains("FAILED"),
        SharedTools.testStatus("MainTest > scenario1() PASSED\nMainTest > scenario2() FAILED", "MainTest").contains("FAILED"),
        SharedTools.testStatus("MainTest > scenario1() PASSED", "MainTest.scenario9").isEmpty,
      )
    },
    test("a running run whose every caller has gone is stopped, and the runs behind it go on") {
      for
        ran     <- Ref.make(Vector.empty[String])
        stopped <- Promise.make[Nothing, Unit]
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       val command = arguments.get("command").flatMap(_.asString).getOrElse(name)
                       ran.update(_ :+ command) *>
                         (if command == "hang" then ZIO.never.onInterrupt(stopped.succeed(())) else ZIO.succeed(Json.Str(command)))
        result  <- ZIO.scoped:
                     for
                       calls <- SharedTools.coordinated(toolset, Duration.Zero)
                       hung  <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("hang"))).fork
                       _     <- ran.get.repeatUntil(_.nonEmpty)
                       next  <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("next"))).fork
                       _     <- hung.interrupt
                       _     <- stopped.await
                       out   <- next.join
                     yield out
      yield assertTrue(result == Json.Str("next"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
    test("a command that fails doesn't hold up the runs behind it: its caller gets the failure, the next run is made") {
      for
        result <- ZIO.scoped:
                    for
                      calls <- SharedTools.coordinated(new Toolset:
                                 val tools = Chunk.empty
                                 def call(name: String, arguments: Json.Obj): Task[Json] =
                                   val command = arguments.get("command").flatMap(_.asString).getOrElse("")
                                   if command == "refused" then ZIO.fail(RuntimeException("declined")) else ZIO.succeed(Json.Str(command))
                               , Duration.Zero)
                      failed <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("refused"))).either
                      next   <- calls.call("execute_terminal_command", Json.Obj("command" -> Json.Str("next")))
                    yield (failed, next)
      yield assertTrue(result._1.left.exists(_.getMessage == "declined"), result._2 == Json.Str("next"))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds),
    test("while other loops work, a loop can't overwrite a file it didn't create; it can its own, and alone it can any") {
      def as[A](loop: String)(zio: Task[A]) = ParallelLoop.currentLoop.locally(Some(loop))(zio)
      def create(path: String, overwrite: Boolean) =
        Json.Obj("pathInProject" -> Json.Str(path), "text" -> Json.Str("x"), "overwrite" -> Json.Bool(overwrite))
      for
        made    <- Ref.make(Vector.empty[String])
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       made.update(m => if name == "create_new_file" then m :+ arguments.get("pathInProject").flatMap(_.asString).getOrElse("") else m)
                         .as(Json.Str("[success]"))
        out     <- ZIO.scoped:
                     for
                       calls  <- SharedTools.coordinated(toolset, Duration.Zero)
                       call    = (n: String, a: Json.Obj) => calls.call(n, a)
                       _      <- as("a")(call("read_file", Json.Obj()))
                       _      <- as("b")(call("read_file", Json.Obj()))
                       shared <- as("a")(call("create_new_file", create("Main.java", true))).either
                       own    <- as("a")(call("create_new_file", create("A.java", false))) *> as("a")(call("create_new_file", create("A.java", true))).either
                       other  <- as("b")(call("create_new_file", create("A.java", true))).either
                     yield (shared, own, other)
        files   <- made.get
      yield assertTrue(
        out._1.left.exists(_.getMessage.contains("not overwritten")),
        out._2.isRight, out._3.isLeft, files == Vector("A.java", "A.java"),
      )
    },
    test("two sessions bound to one project's coordinator: their waiting test runs merge, through one session's tools") {
      def as[A](loop: String)(zio: Task[A]) = ParallelLoop.currentLoop.locally(Some(loop))(zio)
      def session(name: String, ran: Ref[Vector[String]], gate: Promise[Nothing, Unit]) = new Toolset:
        val tools = Chunk.empty
        def call(tool: String, arguments: Json.Obj): Task[Json] =
          val command = arguments.get("command").flatMap(_.asString).getOrElse(tool)
          ran.update(_ :+ s"$name: $command") *> gate.await.when(command.contains("first")).as(Json.Str(s"$name ran $command"))
      for
        ran    <- Ref.make(Vector.empty[String])
        gate   <- Promise.make[Nothing, Unit]
        out    <- ZIO.scoped:
                    for
                      project <- SharedTools.Coordinator.make(Duration.Zero)
                      a        = project.bind(session("a", ran, gate))
                      b        = project.bind(session("b", ran, gate))
                      first   <- as("a/task-1")(a.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests first")))).fork
                      _       <- ran.get.repeatUntil(_.nonEmpty)
                      x       <- as("a/task-2")(a.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests X")))).fork
                      y       <- as("b/task-1")(b.call("execute_terminal_command", Json.Obj("command" -> Json.Str("./gradlew test --tests Y")))).fork
                      _       <- ZIO.yieldNow.repeatN(100) *> gate.succeed(())
                      all     <- ZIO.foreach(List(first, x, y))(_.join)
                    yield all
        runs   <- ran.get
      yield
        // the merged run is made through whichever waiting session's tools went first: either order is one run
        val merged = Set("a: ./gradlew test --tests X --tests Y", "b: ./gradlew test --tests Y --tests X")
        assertTrue(
          runs.size == 2, runs.head == "a: ./gradlew test --tests first", merged(runs(1)),
          out.drop(1).forall(r => inner(r) == Json.Str(runs(1).replaceFirst(": ", " ran "))),
        )
    },
    test("two coordinators of one project (two soness processes) take turns at runs through the lock directory") {
      for
        dir     <- ZIO.attemptBlocking(java.nio.file.Files.createTempDirectory("soness-locks"))
        active  <- Ref.make(0)
        most    <- Ref.make(0)
        toolset  = new Toolset:
                     val tools = Chunk.empty
                     def call(name: String, arguments: Json.Obj): Task[Json] =
                       (active.updateAndGet(_ + 1).flatMap(n => most.update(_ max n)) *> ZIO.sleep(200.millis))
                         .ensuring(active.update(_ - 1)).as(Json.Str("ran"))
        _       <- ZIO.scoped:
                     for
                       one   <- SharedTools.Coordinator.make(Duration.Zero, locks = Some(dir))
                       two   <- SharedTools.Coordinator.make(Duration.Zero, locks = Some(dir))
                       runs   = List(one, two, one, two).zipWithIndex.map: (c, i) =>
                                  c.bind(toolset).call("execute_terminal_command", Json.Obj("command" -> Json.Str(s"./gradlew test --tests T$i")))
                       _     <- ZIO.collectAllPar(runs)
                     yield ()
        n       <- most.get
      yield assertTrue(n == 1)
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
  )
