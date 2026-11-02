package com.jamesward.soness.dev

import com.jamesward.soness.*
import com.jamesward.zio_decisions.Decisions
import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zio_decisions.orchestration.Interjection
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

import java.nio.file.{Files, Path}

/**
 * The development build's self-improvement on soness's own project (a clone of its repository): the observer records
 * findings from the session's own turns (published in the session's shared state, no log), and each finding starts
 * rounds by itself on the new findings; `self_improve` runs rounds on request. Rounds run in the background as a task
 * of the session, its steering reaching every round, each working on the project itself with the session's own tools,
 * between its steps' prepare and gate.
 */
object SelfImprovementSpec extends ZIOSpecDefault:

  final case class Seen(request: String, context: Option[String], tools: List[String], steering: List[String], read: String)

  /**
   * "improve …" starts self-improvement; "fix …" reads A.txt twice (a turn worth a finding); "hello" reads nothing (a
   * fine turn); the observer's analysis replies with a finding; a round waits for steering, then reads the project's
   * M.txt. Judges a turn by its request; steers to the newest running task once the launcher has handed over.
   */
  private final class Fake(seen: Ref[Vector[Seen]]) extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String],
        steering: UIO[List[String]]): Task[HarnessResult] =
      val tools = toolset.tools.map(_.name).toList
      def read(path: String) = toolset.call("read_file", Json.Obj("path" -> Json.Str(path))).map(_.toString).catchAll(e => ZIO.succeed(s"FAILED ${e.getMessage}"))
      if prompt.startsWith("improve") then
        toolset.call("self_improve", Json.Obj("rounds" -> Json.Num(2), "eval" -> Json.Bool(false)))
          .map(r => HarnessResult(r.toString, ModelUsage())) <* ZIO.sleep(3.seconds)
      else if prompt.startsWith("fix") then (read("A.txt") *> read("A.txt")).as(HarnessResult("Done.", ModelUsage()))
      else if prompt.startsWith("You are observing") then
        seen.update(_ :+ Seen(prompt, context, tools, Nil, "")).as(HarnessResult(
          "**The same file was read twice in a row.**\nEvidence: call-1, call-2.", ModelUsage()))
      else if prompt.startsWith("Improve soness") then
        for
          st   <- steering.repeatUntil(_.nonEmpty).timeout(5.seconds).someOrElse(Nil)
          text <- read("M.txt")
          _    <- seen.update(_ :+ Seen(prompt, context, tools, st, text))
        yield HarnessResult("fixed it", ModelUsage())
      else ZIO.succeed(HarnessResult("Hello.", ModelUsage()))

    override def judge(situation: String, question: String, yes: String, no: String): Task[Double] =
      ZIO.succeed(if situation.contains("fix the bug") then 0.9 else 0.1)

    override def route(message: String, running: List[Interjection.Running], context: Option[String],
        processes: List[Interjection.Process]): Task[Interjection.Decision] =
      val target = if running.exists(_.request.startsWith("improve")) then -1 else running.lastOption.map(_.id).getOrElse(0)
      // a new request is a task of its own, while self-improvement runs too
      val route = if message.startsWith("fix") || message.startsWith("hello") then Interjection.Route.NewTask else Interjection.Route.Steer(target)
      ZIO.succeed(Interjection.Decision(route, Map.empty, 1.0, Decisions.Usage(0, 0), 0))

  /** The steps, faked: prepare's and gate's calls recorded; every gate passes the tests, without the eval. */
  private final class Steps(project: Path, calls: Ref[Vector[String]]) extends Improve.Steps:
    def prepare(o: Improve.Options, n: Int, log: Option[Path], history: List[String]): Task[Improve.Round] =
      calls.update(_ :+ s"prepare $n history=${history.size}").as(Improve.Round(n, project.resolve(s"r$n"), project, Improve.prompt(n, project, log, history)))
    def gate(o: Improve.Options, round: Improve.Round, by: Improve.By): Task[Improve.Verdict] =
      calls.update(_ :+ s"gate ${round.round} eval=${o.eval}").as(Improve.Verdict("tests pass (no eval)", files = List("src/main/scala/A.scala")))

  private final class Updates(updates: Ref[Vector[SessionUpdate]]) extends com.jamesward.zioacp.Client:
    def sessionUpdate(n: SessionNotification): UIO[Unit] = updates.update(_ :+ n.update)
    def requestPermission(r: RequestPermissionRequest) = ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))

  private def text(us: Vector[SessionUpdate]) = us.collect { case SessionUpdate.AgentMessageChunk(ContentBlock.Text(t, _, _), _, _) => t }.mkString

  private def clone(dir: Path, remote: String): Task[Unit] =
    ZIO.attemptBlocking:
      Files.createDirectories(dir.resolve(".git"))
      Files.writeString(dir.resolve(".git/config"), s"[remote \"origin\"]\n\turl = $remote\n")
    .unit

  /** An agent with self-improvement on a project (a clone of soness when `self`) with M.txt. */
  private def setup(self: Boolean, earlier: List[Observer.Finding] = Nil) =
    for
      http    <- ZIO.environment[Client]
      dir     <- ZIO.attemptBlocking(Files.createTempDirectory("improving"))
      _       <- ZIO.attemptBlocking(Files.writeString(dir.resolve("M.txt"), "the project's file"))
      _       <- ZIO.attemptBlocking(Files.writeString(dir.resolve("A.txt"), "class A"))
      _       <- clone(dir, if self then "git@github.com:jamesward/soness.git" else "https://github.com/someone/shop.git")
      _       <- ZIO.foreachDiscard(earlier)(Observer.record(dir, _))
      seen    <- Ref.make(Vector.empty[Seen])
      calls   <- Ref.make(Vector.empty[String])
      updates <- Ref.make(Vector.empty[SessionUpdate])
      parent  <- ZIO.scope
      pipe    <- Transport.pipe
      catalog  = HarnessCatalog.make(List("m" -> Fake(seen)), "m").toOption.get
      _       <- AcpAgent.serve(pipe._1)(client =>
                   SonessAgent.make(client, http, catalog, parent, List(SelfImprovement(_ => Steps(dir, calls))))).forkScoped
      agent   <- AcpClient.connect(pipe._2, Updates(updates))
      _       <- agent.initialize(InitializeRequest(ProtocolVersion.V1, ClientCapabilities()))
      id      <- agent.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
      say      = (t: String) => agent.prompt(PromptRequest(id, List(ContentBlock.text(t))))
    yield (dir, id, say, seen, calls, updates)

  def spec = suite("self-improvement (the development build)")(
    test("the observer: a turn worth it is analyzed read-only and silently, recorded, and improved on by itself; a fine one isn't") {
      val old = Observer.Finding("t", "s", "s/task-9", "earlier", "answered", 0.9, "old", "An earlier process's finding.")
      for
        (dir, id, say, seen, calls, updates) <- setup(self = true, List(old))
        _        <- say("/status")
        status   <- updates.get.map(text)
        _        <- say("hello")
        _        <- say("fix the bug in A")
        done     <- updates.get.map(text).repeatUntil(_.contains("Self-improvement done")).timeout(60.seconds)
        all      <- Observer.read(dir)
        analyses <- seen.get.map(_.filter(_.request.startsWith("You are observing")))
        rounds   <- seen.get.map(_.filter(_.request.startsWith("Improve soness")))
        made     <- calls.get
        chat     <- updates.get.map(text)
      yield
        val tools = analyses.headOption.map(_.tools).getOrElse(Nil)
        assertTrue(
          status.contains(Observer.Name),
          all.size == 2, all.last.request == "fix the bug in A", all.last.session == id.value,
          all.last.summary == "The same file was read twice in a row.",
          // the turn as it went, from the session's shared state: its calls and their results
          analyses.size == 1, analyses.head.context.exists(c => c.contains("read_file") && c.contains("class A")),
          tools.contains("read_file"), !tools.contains("write_file"), !tools.contains("edit_file"), !tools.contains("run_command"),
          !tools.contains("self_improve"),
          // nothing asked of the user: one round started by itself on the new finding (not the earlier process's),
          // applied when it passes the gates, without the paid eval
          rounds.size == 1, rounds.head.context.exists(_.contains("The same file was read twice in a row.")),
          !rounds.head.context.exists(_.contains("An earlier process's finding.")),
          made == Vector("prepare 1 history=0", "gate 1 eval=false"),
          done.exists(_.contains("Self-improvement done (1 round(s))")),
          // the round's steps aren't said in the chat; its plan (the finding, marked as self-improvement's) and its
          // summary are, the observer itself says nothing
          !chat.contains("round 1 of 1: starting"),
          chat.contains("**[self-improvement]** Starting, from the observer's 1 new finding(s):\n- b2c8ccb8/task-2: The same file was read twice in a row."),
          chat.contains("Plan: 1 round(s)."), chat.contains("**[self-improvement]** Self-improvement done"),
          chat.indexOf("read twice in a row.") > chat.indexOf("**[self-improvement]** Starting"),
        )
    },
    test("findings made while a run is working start another once it ends, on those findings only") {
      for
        (_, _, say, seen, calls, updates) <- setup(self = true)
        _      <- say("fix the bug in A")
        _      <- calls.get.repeatUntil(_.nonEmpty).timeout(20.seconds)
        // the run's tree, while it works: its task, and its round below it
        status <- (updates.get.map(text).flatMap(before => say("/status") *> updates.get.map(text(_).drop(before.length))))
                    .repeatUntil(_.contains("round-1")).timeout(10.seconds).someOrElse("")
        _      <- say("fix the bug in A again")
        _      <- calls.get.repeatUntil(_.size >= 4).timeout(60.seconds)
        rounds <- seen.get.map(_.filter(_.request.startsWith("Improve soness")))
        made   <- calls.get
      yield assertTrue(
        status.contains("self-improvement: from 1 new finding(s)"), status.contains("round-1"),
        made.count(_.startsWith("prepare")) == 2, rounds.size == 2,
        rounds.forall(_.context.exists(_.split("- turn ").length == 2)),
        rounds(1).context.exists(_.contains("fix the bug in A again")),
      )
    },
    test("self_improve: rounds in the background; steering sent while the launcher still replies reaches every round") {
      for
        (dir, id, say, seen, calls, updates) <- setup(self = true)
        _       <- Observer.record(dir, Observer.Finding("t", "s", "s/task-1", "Fix it", "answered", 0.9, "x", "The tool was misdescribed."))
        launch  <- say("improve yourself").fork
        _       <- updates.get.map(text).repeatUntil(_.contains("round 1 of 2: starting")).timeout(20.seconds)
        steered <- say("focus on how the search tools are chosen")
        // the steering prompt stays open until the background run ends, so its summary reaches the chat in that turn
        atSteer <- updates.get.map(text)
        _       <- launch.join
        done    <- updates.get.map(text).repeatUntil(_.contains("Self-improvement done")).timeout(60.seconds)
        rounds  <- seen.get.map(_.filter(_.request.startsWith("Improve soness")))
        made    <- calls.get
      yield
        val all = done.getOrElse("")
        assertTrue(
          all.contains("Started: self-improvement: 2 rounds, no eval"),
          steered.meta.exists(_.toString.contains("steer:")),
          atSteer.contains("report here what it did when it's done"), atSteer.contains("**[self-improvement]** Self-improvement done (2 round(s))"),
          atSteer.contains("**[self-improvement]** Starting, from the observer's findings"),
          rounds.size == 2, rounds.forall(_.steering == List("focus on how the search tools are chosen")),
          rounds.forall(_.read.contains("the project's file")),
          rounds.forall(_.context.exists(_.contains("The tool was misdescribed."))),
          rounds.head.request.contains("round 1 of a self-improvement run"),
          all.contains("Round 1: tests pass (no eval) (src/main/scala/A.scala)"), all.contains("Self-improvement done (2 round(s))"),
          made == Vector("prepare 1 history=0", "gate 1 eval=false", "prepare 2 history=1", "gate 2 eval=false"),
        )
    },
    test("/stop of its process ends it, with no more rounds") {
      for
        (_, _, say, seen, _, updates) <- setup(self = true)
        started <- say("improve yourself").zipRight(updates.get.map(text))
        pid      = """process \[(\d+)\]""".r.findAllMatchIn(started).map(_.group(1)).toList.lastOption.getOrElse("0")
        _       <- updates.get.map(text).repeatUntil(_.contains("round 1 of 2: starting")).timeout(20.seconds)
        _       <- say(s"/stop $pid")
        stopped <- updates.get.map(text).repeatUntil(_.contains("stopped")).timeout(20.seconds)
        rounds  <- seen.get.map(_.filter(_.request.startsWith("Improve soness")))
      yield assertTrue(stopped.nonEmpty, rounds.isEmpty)
    },
    test("on another project: no observer, no self_improve") {
      for
        (_, _, say, seen, _, updates) <- setup(self = false)
        _      <- say("/status")
        status <- updates.get.map(text)
        _      <- say("fix the bug in A")
        _      <- ZIO.sleep(2.seconds)
        all    <- seen.get
        offered <- updates.get.map(_.collect { case u: SessionUpdate.AvailableCommandsUpdate => u.availableCommands.map(_.name) })
      yield assertTrue(!status.contains(Observer.Name), all.isEmpty, offered.forall(c => !c.contains("feedback") && !c.contains("findings")))
    },
    test("soness's own project is a clone of it, by any remote naming jamesward/soness or jamesward/jevness") {
      for
        dir      <- ZIO.attemptBlocking(Files.createTempDirectory("clones"))
        make      = (name: String, config: String) => ZIO.attemptBlocking:
                      val d = Files.createDirectories(dir.resolve(name).resolve(".git"))
                      Files.writeString(d.resolve("config"), config)
                      dir.resolve(name)
        https    <- make("https", "[remote \"origin\"]\n\turl = https://github.com/jamesward/jevness.git\n")
        ssh      <- make("ssh", "[remote \"upstream\"]\n\turl = git@github.com:JamesWard/soness\n[remote \"origin\"]\n\turl = https://github.com/me/fork.git\n")
        other    <- make("other", "[remote \"origin\"]\n\turl = https://github.com/jamesward/sonessx.git\n")
        worktree <- ZIO.attemptBlocking:
                      val w = Files.createDirectories(dir.resolve("worktree"))
                      val gitdir = Files.createDirectories(https.resolve(".git/worktrees/w"))
                      Files.writeString(gitdir.resolve("commondir"), "../..")
                      Files.writeString(w.resolve(".git"), s"gitdir: $gitdir\n")
                      w
        none     <- ZIO.attemptBlocking(Files.createDirectories(dir.resolve("none")))
      yield assertTrue(Observer.isSoness(https), Observer.isSoness(ssh), !Observer.isSoness(other), Observer.isSoness(worktree),
        !Observer.isSoness(none), Observer.gitHubRepository("https://github.com/jamesward/soness/").contains("jamesward/soness"))
    },
    test("the gates' pure parts: changed files, specs, the patch's labels, test counts, eval parts") {
      val diff =
        """diff -ruN soness/src/main/scala/A.scala /m/src/main/scala/A.scala
          |--- soness/src/main/scala/A.scala
          |+++ /m/src/main/scala/A.scala
          |@@ -1 +1 @@
          |-// old comment
          |+// new comment
          |--- soness/src/main/scala/B.scala
          |+++ /m/src/main/scala/B.scala
          |@@ -1 +1 @@
          |-val x = 1
          |+val x = 2
          |""".stripMargin
      // a task's changes, from the event log: the project's files relative, a source dependency's as <name>/…
      def changed(file: String, patch: String) = new EventLog.Event.FileChanged("s", 1, "t", "c", file, patch)
      val project = Path.of("/m/soness")
      val dep     = Path.of("/m/zio-decisions")
      val labelled = Improve.patchOf(List(
        changed("/m/soness/src/main/scala/A.scala", EventLog.patchOf("src/main/scala/A.scala", Some("// old comment\n"), Some("// new comment\n"))),
        changed("/m/soness/src/main/scala/B.scala", EventLog.patchOf("src/main/scala/B.scala", Some("val x = 1\n"), Some("val x = 2\n")))),
        project, List(dep))
      val ofDep = Improve.patchOf(List(changed("/m/zio-decisions/src/main/scala/L.scala",
        EventLog.patchOf("/m/zio-decisions/src/main/scala/L.scala", Some("val a = 1\n"), Some("val a = 2\n")))), project, List(dep))
      val records = List(
        """{"label":"q","extra":{"parts":{"a":true,"b":false}}}""", """{"label":"q","extra":{"parts":{"c":true}}}""",
        """{"label":"other","extra":{"parts":{"d":true}}}""")
      assertTrue(
        Improve.changedFiles(labelled) == Map("src/main/scala/A.scala" -> false, "src/main/scala/B.scala" -> true),
        labelled.contains("+++ b/src/main/scala/B.scala"),
        // a source dependency's change counts: its files are <name>/src/…, main code and tests apart from the project's
        Improve.changedFiles(ofDep) == Map("zio-decisions/src/main/scala/L.scala" -> true),
        // a file edited twice, its last edit only a comment: still a code change
        Improve.changedFiles(Improve.patchOf(List(
          changed("/m/soness/src/main/scala/B.scala", EventLog.patchOf("x", Some("val x = 1\n"), Some("val x = 2\n"))),
          changed("/m/soness/src/main/scala/B.scala", EventLog.patchOf("x", Some("// a\n"), Some("// b\n")))), project, List(dep)))
          == Map("src/main/scala/B.scala" -> true),
        // a new file's patch, and a deleted one's
        EventLog.patchOf("N.scala", None, Some("a\nb\n")) == "--- /dev/null\n+++ b/N.scala\n@@ -0,0 +1,2 @@\n+a\n+b\n",
        EventLog.patchOf("N.scala", Some("a\n"), None) == "--- a/N.scala\n+++ /dev/null\n@@ -1,1 +0,0 @@\n-a\n",
        Improve.isMain("zio-decisions/src/main/scala/L.scala"), Improve.isMain("src/main/scala/A.scala"),
        !Improve.isMain("zio-decisions/src/test/scala/LSpec.scala"), Improve.isTest("zio-decisions/src/test/scala/LSpec.scala"),
        Improve.specNames("package a.b\n\nobject XSpec extends ZIOSpecDefault:\nobject Y\n") == List("a.b.XSpec"),
        Improve.testCounts("3 tests passed. 1 tests failed.\n...\n9 tests passed. 0 tests failed.") == Some((9, 0)),
        Improve.evalParts(records, "q") == (2, 3),
        Improve.options(Json.Obj("rounds" -> Json.Num(30)), None).isLeft,
        Improve.options(Json.Obj(), None).exists(o => o.log.isEmpty && o.rounds == 1 && o.eval),
        Observer.summary("\n## **It repeated a call.**\nmore") == "It repeated a call.",
        Observer.context(Nil).isEmpty,
      )
    },
  ).provideSome[Scope](Client.default) @@ TestAspect.withLiveClock @@ TestAspect.timeout(120.seconds)

