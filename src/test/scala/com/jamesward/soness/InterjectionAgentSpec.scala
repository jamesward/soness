package com.jamesward.soness

import com.jamesward.zio_decisions.Decisions
import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zio_decisions.orchestration.{Interjection, ProcessTree}
import com.jamesward.zio_decisions.orchestration.Interjection.Route
import com.jamesward.zioacp.{AcpAgent, AcpClient, Transport}
import com.jamesward.zioacp.schema.*
import zio.*
import zio.http.Client
import zio.json.ast.Json
import zio.test.*

/** A prompt that arrives while a task runs: routed by the harness (Jev, in soness) as steering, a new task, or a follow-up. */
object InterjectionAgentSpec extends ZIOSpecDefault:

  /**
   * The first prompt ("work") waits for `gate`, then answers with the steering it got; any other prompt answers at
   * once with its context. Routes by the message's first word: "steer", "after", or anything else a new task.
   */
  private final class Stub(gate: Promise[Nothing, Unit], started: Promise[Nothing, Unit], asked: Ref[Vector[List[Interjection.Running]]])
      extends Harness:
    def run(prompt: String, toolset: Toolset, documents: Seq[Document], context: Option[String], mode: HarnessMode,
        steering: UIO[List[String]]): Task[HarnessResult] =
      if prompt == "hangs" then
        started.succeed(()) *>
          ProcessTree.run(ProcessTree.Kind.Call, "hang", Some("./gradlew test"))(ZIO.never).either.map: r =>
            HarnessResult(s"the call ended: ${r.fold(_.getMessage, _ => "ok")}", ModelUsage())
      else if prompt == "work" then started.succeed(()) *> gate.await *> steering.map(st => HarnessResult(s"worked; steered with ${st.mkString("|")}", ModelUsage()))
      else ZIO.succeed(HarnessResult(s"answered $prompt; context: ${context.getOrElse("")}", ModelUsage()))
    override def route(message: String, running: List[Interjection.Running], context: Option[String],
        processes: List[Interjection.Process]): Task[Interjection.Decision] =
      val route = message.takeWhile(_ != ' ') match
        case "steer"  => Route.Steer(running.head.id)
        case "after"  => Route.After(running.head.id)
        case "stop"   => Route.Stop(running.head.id)
        case "status" => Route.Status
        // a decision model slow to answer: the client can't wait for it
        case "slow"   => Route.Steer(running.head.id)
        case "kill"   => processes.find(_.name == "hang").fold(Route.NewTask)(p => Route.StopProcess(p.id))
        case _        => Route.NewTask
      (if message.startsWith("slow") then ZIO.sleep(30.seconds) else ZIO.unit) *>
        asked.update(_ :+ running).as(Interjection.Decision(route, Map.empty, 0.9, Decisions.Usage(40, 2), 5))

    // a summary that takes a while to write (an ACP agent's new session, a slow model)
    override def status(question: String, tree: String): Task[String] = ZIO.sleep(5.seconds).as(s"summary: $tree")

  private def setup =
    for
      http    <- ZIO.environment[Client]
      gate    <- Promise.make[Nothing, Unit]
      started <- Promise.make[Nothing, Unit]
      asked   <- Ref.make(Vector.empty[List[Interjection.Running]])
      updates <- Ref.make(Vector.empty[SessionUpdate])
      parent  <- ZIO.scope
      pipe    <- Transport.pipe
      catalog  = HarnessCatalog.make(List("m" -> Stub(gate, started, asked)), "m").toOption.get
      _       <- AcpAgent.serve(pipe._1)(client => SonessAgent.make(client, http, catalog, parent)).forkScoped
      agent   <- AcpClient.connect(pipe._2, new com.jamesward.zioacp.Client:
                   def sessionUpdate(notification: SessionNotification): UIO[Unit] = updates.update(_ :+ notification.update)
                   def requestPermission(request: RequestPermissionRequest) =
                     ZIO.succeed(RequestPermissionResponse(RequestPermissionOutcome.Cancelled))
                 )
      _       <- agent.initialize(InitializeRequest(ProtocolVersion.V1))
      dir     <- ZIO.attempt(java.nio.file.Files.createTempDirectory("interjection")).orDie
      session <- agent.newSession(NewSessionRequest(dir.toString, Nil))
    yield (agent, session.sessionId, gate, asked, updates, started)

  private def text(updates: Vector[SessionUpdate]): Vector[String] =
    updates.collect { case u: SessionUpdate.AgentMessageChunk => u.content }.collect { case ContentBlock.Text(t, _, _) => t }

  private def prompt(agent: com.jamesward.zioacp.AgentConnection, id: SessionId, message: String) =
    agent.prompt(PromptRequest(id, List(ContentBlock.text(message))))

  private def route(response: PromptResponse) = TurnMetrics.fromMeta(response.meta).flatMap(_.route)

  def spec = suite("a prompt while a task runs")(
    test("steering is answered at once and reaches the running task") {
      for
        (agent, id, gate, asked, updates, started) <- setup
        work   <- prompt(agent, id, "work").fork
        _      <- started.await
        steer  <- prompt(agent, id, "steer use tabs")
        _      <- gate.succeed(())
        done   <- work.join
        seen   <- asked.get
        all    <- updates.get
      yield assertTrue(
        route(steer).contains("steer:1"),
        TurnMetrics.fromMeta(steer.meta).map(_.jevInputTokens).contains(40L),
        seen.head.map(_.request) == List("work"),
        text(all).exists(_.startsWith("Got it")),
        text(all).contains("worked; steered with steer use tabs"),
        done.stopReason == StopReason.EndTurn,
      )
    } @@ TestAspect.withLiveClock,
    test("send now (_session/steering, advertised in initialize's _meta): into the running turn; nothing running, not found") {
      for
        (agent, id, gate, asked, updates, started) <- setup
        init    <- agent.initialize(InitializeRequest(ProtocolVersion.V1))
        idle    <- agent.extMethod("_session/steering", Json.Obj("sessionId" -> Json.Str(id.value),
                     "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("steer early"))))).either
        work    <- prompt(agent, id, "work").fork
        _       <- started.await
        steer   <- agent.extMethod("_session/steering", Json.Obj("sessionId" -> Json.Str(id.value),
                     "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("steer use tabs")))))
        more    <- agent.extMethod("_session/steering", Json.Obj("sessionId" -> Json.Str(id.value),
                     "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str("also add a test")))))
        _       <- gate.succeed(())
        done    <- work.join
        all     <- updates.get
      yield assertTrue(
        init.meta.flatMap(_.get("steering")).flatMap(_.asObject).flatMap(_.get("supported")).contains(Json.Bool(true)),
        idle.left.exists(_.code == ErrorCode.MethodNotFound),
        steer.toString == """{"outcome":"injected"}""", more.toString == """{"outcome":"injected"}""",
        text(all).contains("worked; steered with steer use tabs|The user added this request, to do as part of this work: also add a test"),
        done.stopReason == StopReason.EndTurn,
      )
    } @@ TestAspect.withLiveClock,
    test("send now is confirmed at once: a command is answered from the tree, a slow route steers the newest task, a status summary comes later") {
      def send(agent: com.jamesward.zioacp.AgentConnection, id: SessionId, message: String) =
        agent.extMethod("_session/steering", Json.Obj("sessionId" -> Json.Str(id.value),
          "prompt" -> Json.Arr(Json.Obj("type" -> Json.Str("text"), "text" -> Json.Str(message))))).timed
      for
        (agent, id, gate, asked, updates, started) <- setup
        work     <- prompt(agent, id, "work").fork
        _        <- started.await
        command  <- send(agent, id, "/status")
        before   <- asked.get.map(_.size)
        slow     <- send(agent, id, "slow, redo it with tabs")
        status   <- send(agent, id, "status of the work?")
        noSummary <- updates.get.map(us => !text(us).exists(_.startsWith("summary:")))
        summary  <- updates.get.map(text).repeatUntil(_.exists(_.startsWith("summary:"))).timeout(15.seconds)
        _        <- gate.succeed(())
        done     <- work.join
        all      <- updates.get
      yield assertTrue(
        command._2.toString == """{"outcome":"injected"}""", command._1 < 2.seconds, before == 0,
        text(all).exists(_.contains("task-1")),
        slow._2.toString == """{"outcome":"injected"}""", slow._1 < 6.seconds,
        status._2.toString == """{"outcome":"injected"}""", status._1 < 2.seconds, noSummary, summary.nonEmpty,
        text(all).exists(_.contains("worked; steered with slow, redo it with tabs")),
        done.stopReason == StopReason.EndTurn,
      )
    } @@ TestAspect.withLiveClock,
    test("a new task runs at once, alongside the running one") {
      for
        (agent, id, gate, _, updates, started) <- setup
        work  <- prompt(agent, id, "work").fork
        _     <- started.await
        other <- prompt(agent, id, "what time is it")
        _     <- gate.succeed(())
        _     <- work.join
        all   <- updates.get
      yield assertTrue(
        route(other).contains("new"),
        // answered while "work" still waited
        text(all).indexWhere(_.startsWith("answered what time")) < text(all).indexWhere(_.startsWith("worked")),
      )
    } @@ TestAspect.withLiveClock,
    test("a follow-up waits for the task it follows and gets its reply as context") {
      for
        (agent, id, gate, _, updates, started) <- setup
        work   <- prompt(agent, id, "work").fork
        _      <- started.await
        follow <- prompt(agent, id, "after commit it").fork
        _      <- ZIO.sleep(200.millis)
        before <- updates.get
        _      <- gate.succeed(())
        _      <- work.join
        done   <- follow.join
        all    <- updates.get
      yield assertTrue(
        text(before).isEmpty,
        route(done).contains("after:1"),
        text(all).exists(t => t.startsWith("answered after commit it") && t.contains("It replied:\nworked; steered with ")),
      )
    } @@ TestAspect.withLiveClock,
    test("a stop cancels the running task: its prompt answers cancelled") {
      for
        (agent, id, gate, _, updates, started) <- setup
        work  <- prompt(agent, id, "work").fork
        _     <- started.await
        stop  <- prompt(agent, id, "stop it")
        done  <- work.join
        all   <- updates.get
      yield assertTrue(
        route(stop).contains("stop:1"),
        done.stopReason == StopReason.Cancelled,
        text(all).exists(_.startsWith("Stopped")),
        !text(all).exists(_.startsWith("worked")),
      )
    } @@ TestAspect.withLiveClock,
    test("an idle session doesn't route: the prompt is a task") {
      for
        (agent, id, gate, asked, _, _) <- setup
        _    <- gate.succeed(())
        r    <- prompt(agent, id, "steer nothing")
        seen <- asked.get
      yield assertTrue(seen.isEmpty, route(r).isEmpty)
    },
    test("/status shows the process tree; a hung call, stopped by Jev's route or /stop, is a failed step and the task ends") {
      for
        (agent, id, _, _, updates, started) <- setup
        work   <- prompt(agent, id, "hangs").fork
        _      <- started.await *> ZIO.sleep(200.millis)
        status <- prompt(agent, id, "/status")
        shown  <- updates.get.map(text(_).last)
        kill   <- prompt(agent, id, "kill the gradle run")
        done   <- work.join
        all    <- updates.get
      yield assertTrue(
        route(status).contains("command"),
        shown.contains("task task-1: running") && shown.contains("call hang: running") && shown.contains("./gradlew test"),
        route(kill).exists(_.startsWith("stop-process:")),
        text(all).exists(_.startsWith("Stopped [")),
        text(all).exists(_.contains("the call ended: hang was stopped: stopped by the user")),
        done.stopReason == StopReason.EndTurn,
      )
    } @@ TestAspect.withLiveClock,
    test("/status in one session shows every session's work, each a node, this one marked; /stop reaches another session's call") {
      for
        (agent, id, _, _, updates, started) <- setup
        dir    <- ZIO.attempt(java.nio.file.Files.createTempDirectory("other")).orDie
        other  <- agent.newSession(NewSessionRequest(dir.toString, Nil)).map(_.sessionId)
        work   <- prompt(agent, id, "hangs").fork
        _      <- started.await *> ZIO.sleep(200.millis)
        _      <- prompt(agent, other, "/status")
        shown  <- updates.get.map(text(_).last)
        callId  = "\\[(\\d+)\\] call hang".r.findFirstMatchIn(shown).map(_.group(1)).getOrElse("0")
        own     = "This session is \\[(\\d+)\\]".r.findFirstMatchIn(shown).map(_.group(1)).getOrElse("0")
        _      <- prompt(agent, other, s"/stop $callId")
        done   <- work.join
        all    <- updates.get.map(text)
      yield assertTrue(
        shown.contains(s"session ${id.value.take(8)}"), shown.contains(s"session ${other.value.take(8)}"),
        shown.contains(s"[$own] session ${other.value.take(8)}"),
        shown.contains("task task-1: running") && shown.contains("call hang: running"),
        all.exists(_.contains("the call ended: hang was stopped")), done.stopReason == StopReason.EndTurn,
      )
    } @@ TestAspect.withLiveClock,
    test("/stop <id> stops a process by its id; unknown ids and commands are answered with the usage") {
      for
        (agent, id, _, _, updates, started) <- setup
        work   <- prompt(agent, id, "hangs").fork
        _      <- started.await *> ZIO.sleep(200.millis)
        _      <- prompt(agent, id, "/status")
        shown  <- updates.get.map(text(_).last)
        callId  = "\\[(\\d+)\\] call hang".r.findFirstMatchIn(shown).map(_.group(1)).getOrElse("0")
        _      <- prompt(agent, id, s"/stop $callId")
        _      <- work.join
        _      <- prompt(agent, id, "/stop 999")
        _      <- prompt(agent, id, "/status please")
        all    <- updates.get.map(text)
      yield assertTrue(
        all.exists(_.startsWith(s"Stopping [$callId] call hang")),
        all.exists(_.contains("the call ended: hang was stopped")),
        all.exists(_.startsWith("There's no process 999")),
        all.exists(_.startsWith("Unknown command /status please")),
      )
    } @@ TestAspect.withLiveClock,
    test("the session advertises its commands") {
      for
        (_, _, _, _, updates, _) <- setup
        _    <- updates.get.repeatUntil(_.exists(_.isInstanceOf[SessionUpdate.AvailableCommandsUpdate])).timeout(5.seconds)
        all  <- updates.get
      yield assertTrue(all.collect { case u: SessionUpdate.AvailableCommandsUpdate => u.availableCommands.map(_.name) }.headOption
        .contains(List("status", "stop")))
    } @@ TestAspect.withLiveClock,
  ).provide(Client.default, Scope.default) @@ TestAspect.timeout(60.seconds)
