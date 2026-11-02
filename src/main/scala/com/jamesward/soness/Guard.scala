package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.Safety
import com.jamesward.zioacp.schema.*
import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.file.Path

/**
 * Every tool call a task makes is allowed, put to the user, or refused, before it's made:
 *
 *  1. **Rules** ([[rules]]), with no model: a read, a search, a fetch, an edit inside the project (or a source
 *     dependency), a build, a test run or a command that only reads is allowed; an edit outside the project, a tool
 *     its server marks destructive, and a command that deletes, pushes, publishes, escalates or pipes into a shell is
 *     put to the user. Most calls end here.
 *  2. **Jev** (zio-decisions' `Safety`), for the rest (a command that's neither, an MCP tool that says nothing about
 *     itself): one yes-or-no question each, the calls waiting at the same moment (a batch's) in one request, judged
 *     with the task's request and its last calls, since whether a call follows from what the user asked is what
 *     catches instructions a page or a file slipped into the work. At least [[Policy.allowAbove]] is allowed, below
 *     [[Policy.denyBelow]] refused, between them put to the user; when Jev can't be asked, the user.
 *  3. **The user**, through ACP's `session/request_permission` on the call's own tool call: allow or reject, once or
 *     always. An "always" answer is a rule for the rest of the session ([[Remembered]]: the tool, and a command's first
 *     two words).
 *
 * A refused call is a failed step that says why, so the loop can do otherwise. `SONESS_GUARD` is `jev` (the default),
 * `rules` (the user decides what the rules don't) or `off`.
 */
object Guard:

  enum Verdict derives CanEqual:
    case Allow(why: String)
    case Ask(why: String)
    case Deny(why: String)

  enum Mode derives CanEqual:
    case Off, Rules, Jev

  final case class Policy(mode: Mode = Mode.Jev, allowAbove: Double = 0.8, denyBelow: Double = 0.2)

  object Policy:
    /** `SONESS_GUARD`, `SONESS_GUARD_ALLOW`, `SONESS_GUARD_DENY`. */
    val fromEnv: UIO[Policy] =
      for
        mode  <- System.env("SONESS_GUARD").orElseSucceed(None).map(_.map(_.trim.toLowerCase))
        allow <- System.env("SONESS_GUARD_ALLOW").orElseSucceed(None).map(_.flatMap(_.trim.toDoubleOption))
        deny  <- System.env("SONESS_GUARD_DENY").orElseSucceed(None).map(_.flatMap(_.trim.toDoubleOption))
      yield Policy(
        mode match
          case Some("off" | "false") => Mode.Off
          case Some("rules")         => Mode.Rules
          case _                     => Mode.Jev,
        allow.getOrElse(0.8), deny.getOrElse(0.2))

  /** The user's "always" answers in a session: by [[key]], allowed or not. */
  type Remembered = Ref[Map[String, Boolean]]

  /** What an "always" answer covers: the tool, and for a command its first two words (`git commit`, `npm test`). */
  def key(name: String, arguments: Json.Obj): String =
    command(arguments).fold(name)(c => s"$name ${c.trim.split("\\s+").take(2).mkString(" ")}")

  private def command(arguments: Json.Obj): Option[String] =
    arguments.get("command").orElse(arguments.get("cmd")).flatMap(_.asString)

  // a command that deletes, rewrites history, pushes, publishes, escalates, fetches code to run, kills processes, or
  // reads credentials: another process's environment, the user's key and credential files
  private val Dangerous = (
    """(?i)(^|[;&|`(\s])(rm\s+-[a-z]*[rf]|rm\s+.*\*|rmdir|sudo|su\s|chmod\s+-R|chown|mkfs|dd\s|shutdown|reboot|kill(all)?\s|pkill|""" +
      """git\s+(push|reset\s+--hard|clean\s+-[a-z]*f|checkout\s+--|branch\s+-D|rebase|filter-branch)|--force|""" +
      """(npm|yarn|pnpm)\s+publish|publish(Local|Signed)?\b|mvn\s+deploy|gradlew?\s+publish|docker\s+(push|rm|system)|""" +
      """kubectl|terraform\s+(apply|destroy)|aws\s|gcloud\s|az\s|curl[^|]*\|\s*(ba)?sh|wget[^|]*\|\s*(ba)?sh|ssh\s|scp\s)""").r

  /** Where credentials live: another process's environment, the user's key and credential files and stores. */
  private val Secrets =
    """(?i)/proc/[^/\s]+/environ|\.aws/credentials|\.ssh/|\.netrc|\.npmrc|\.pypirc|\.docker/config\.json|\.kube/config|\.gnupg/|keychain|secret-tool|credentials\.json""".r
  // a command that only reads: what it prints is all it does
  private val Reading = (
    """^\s*(ls|cat|head|tail|wc|grep|rg|find|pwd|echo|which|type|file|stat|tree|du|df|date|whoami|uname|""" +
      """git\s+(status|diff|log|show|branch|rev-parse|ls-files|blame|remote\s+-v))(\s|$)""").r

  /** What the rules say of a call, or none when they can't settle it. */
  def rules(spec: Option[ToolSpec], name: String, arguments: Json.Obj, root: Option[Path], dependencies: List[Path]): Option[Verdict] =
    val annotations = spec.flatMap(_.annotations)
    def hint(h: String) = annotations.exists(_.get(h).contains(Json.Bool(true)))
    val kind = ToolCallView.kind(ToolResolver.baseName(name), annotations)
    lazy val outside = ToolCallView.locations(arguments).map(_.path).filter: p =>
      root.exists: r =>
        val resolved = scala.util.Try(r.resolve(p.stripPrefix("file://")).normalize).toOption
        resolved.exists(f => !f.startsWith(r.toAbsolutePath.normalize) && !dependencies.exists(d => f.startsWith(d.toAbsolutePath.normalize)))
    // a JetBrains router's `execute_tool` takes a routed tool's command line: the routed tool is what's called
    val routed = Option.when(name == "execute_tool")(command(arguments).map(_.trim.takeWhile(!_.isWhitespace))).flatten
    if routed.exists(RoutedTools.reads) then Some(Verdict.Allow("a routed IDE tool that only reads"))
    // credentials: a command or a path that reaches where they're kept is the user's to allow
    else if Secrets.findFirstIn(arguments.toJson).isDefined then Some(Verdict.Ask("it reaches where credentials are kept"))
    else command(arguments).filterNot(_ => routed.isDefined) match
      case Some(c) if Dangerous.findFirstIn(c).isDefined => Some(Verdict.Ask(s"the command may do what can't be undone: $c"))
      // a build or a test run is allowed even through a tool its server marks destructive (sbta's `sbt-task`): the
      // commands that can't be undone (publishing, `--force`) were put to the user just above
      case Some(_) if SharedTools.supersedable(name, arguments) => Some(Verdict.Allow("a build or a test run"))
      case _ if hint("destructiveHint")                  => Some(Verdict.Ask(s"$name is marked destructive by its server"))
      case Some(c) if Reading.findFirstIn(c).isDefined && !c.contains(">") && !c.contains("`") && !c.contains("$(") =>
        Some(Verdict.Allow("a command that only reads"))
      case Some(c) if SharedTools.supersedable(name, arguments) => Some(Verdict.Allow("a build or a test run"))
      case Some(_) => None
      case None =>
        // a page whose address carries much more than a page needs (data sent out in a query) isn't a plain read
        val url = arguments.get("url").flatMap(_.asString).getOrElse("")
        if kind == ToolKind.Fetch && (url.length > 300 || url.dropWhile(_ != '?').length > 200) then None
        else if kind == ToolKind.Fetch then Some(Verdict.Allow("it reads a page"))
        else if hint("readOnlyHint") || SharedTools.reads(spec.toList)(name) ||
            Set(ToolKind.Read, ToolKind.Search, ToolKind.Think).contains(kind) then Some(Verdict.Allow("it only reads"))
        else if Set(ToolKind.Edit, ToolKind.Delete, ToolKind.Move).contains(kind) then
          if outside.nonEmpty then Some(Verdict.Ask(s"it changes files outside the project: ${outside.mkString(", ")}"))
          else if kind == ToolKind.Edit then Some(Verdict.Allow("an edit in the project")) else None
        else if SharedTools.supersedable(name, arguments) then Some(Verdict.Allow("a build or a test run"))
        else None

  /** A call put to Jev, waiting with the others that came at the same moment. */
  private final case class Pending(call: Safety.Call, answer: Promise[Nothing, Option[Double]])

  /**
   * The guard of a session's calls: `judge` asks Jev (through the session's harness, whichever model it has now),
   * `client` asks the user. Calls put to Jev within [[Window]] of each other go in one request.
   */
  final class Session private[Guard] (policy: Policy, pending: Queue[Pending], remembered: Remembered, permission: Guard.Permission,
      sessionId: SessionId, root: Option[Path], dependencies: List[Path]):

    /** `toolset`'s calls for a task working on `request`, each allowed, put to the user or refused first. */
    def bind(toolset: Toolset, request: String, recent: UIO[List[String]]): Toolset =
      if policy.mode == Mode.Off then toolset
      else new Toolset:
        val tools: Chunk[ToolSpec] = toolset.tools
        def call(name: String, arguments: Json.Obj): Task[Json] =
          verdict(toolset.tools.find(_.name == name), name, arguments, request, recent).flatMap:
            case Verdict.Allow(_)  => toolset.call(name, arguments)
            case Verdict.Deny(why) =>
              ZIO.logInfo(s"guard: refused $name: $why") *>
                ZIO.fail(RuntimeException(s"not called: $why. Do the work another way, or say in the reply what the user would need to allow."))
            case Verdict.Ask(_)    => ZIO.dieMessage("asked verdicts are settled before the call")

    private def verdict(spec: Option[ToolSpec], name: String, arguments: Json.Obj, request: String, recent: UIO[List[String]]): UIO[Verdict] =
      val k = key(name, arguments)
      remembered.get.map(_.get(k)).flatMap:
        case Some(true)  => ZIO.succeed(Verdict.Allow("the user allowed it for this session"))
        case Some(false) => ZIO.succeed(Verdict.Deny(s"the user refused $k for this session"))
        case None =>
          rules(spec, name, arguments, root, dependencies) match
            case Some(v: Verdict.Allow) => ZIO.succeed(v)
            case Some(Verdict.Ask(why)) => ask(name, arguments, k, why)
            case Some(v)                => ZIO.succeed(v)
            case None if policy.mode == Mode.Rules => ask(name, arguments, k, "the rules don't cover it")
            case None =>
              recent.flatMap: last =>
                val c = Safety.Call(name, spec.fold("")(_.description.take(600)), spec.flatMap(_.annotations).map(_.toJson),
                  arguments.toJson.take(3000), request.take(3000), last.takeRight(3))
                Promise.make[Nothing, Option[Double]].flatMap(p => pending.offer(Pending(c, p)) *> p.await).flatMap:
                  case Some(p) if p >= policy.allowAbove => ZIO.logDebug(f"guard: Jev allows $name (p $p%.2f)").as(Verdict.Allow(f"Jev judged it safe (p $p%.2f)"))
                  case Some(p) if p < policy.denyBelow   =>
                    ZIO.logInfo(f"guard: Jev refuses $name (p $p%.2f)").as(Verdict.Deny(f"judged unsafe for this request (p $p%.2f)"))
                  case Some(p) => ask(name, arguments, k, f"Jev wasn't sure it's safe (p $p%.2f)")
                  case None    => ask(name, arguments, k, "Jev couldn't be asked")

    /** The user's answer, through ACP's permission request on the call's own tool call. */
    private def ask(name: String, arguments: Json.Obj, k: String, why: String): UIO[Verdict] =
      ClientTools.currentCall.get.flatMap: call =>
        val id = call.getOrElse(ToolCallId(s"guard-${java.util.UUID.randomUUID}"))
        val options = List(
          PermissionOption(PermissionOptionId("allow_once"), "Allow", PermissionOptionKind.AllowOnce),
          PermissionOption(PermissionOptionId("allow_always"), s"Always allow $k", PermissionOptionKind.AllowAlways),
          PermissionOption(PermissionOptionId("reject_once"), "Reject", PermissionOptionKind.RejectOnce),
          PermissionOption(PermissionOptionId("reject_always"), s"Always reject $k", PermissionOptionKind.RejectAlways),
        )
        val update = ToolCallUpdate(id, title = Some(s"${ToolCallView.title(name, arguments)} — $why"), rawInput = Some(arguments))
        ZIO.logInfo(s"guard: asking the user about $name: $why") *>
          permission(RequestPermissionRequest(sessionId, update, options)).either.flatMap:
            case Right(RequestPermissionResponse(RequestPermissionOutcome.Selected(option, _), _)) => option.value match
              case "allow_once"    => ZIO.succeed(Verdict.Allow("the user allowed it"))
              case "allow_always"  => remembered.update(_.updated(k, true)).as(Verdict.Allow("the user allowed it"))
              case "reject_always" => remembered.update(_.updated(k, false)).as(Verdict.Deny(s"the user refused it ($why)"))
              case _               => ZIO.succeed(Verdict.Deny(s"the user refused it ($why)"))
            case Right(_) => ZIO.succeed(Verdict.Deny(s"the user didn't allow it ($why)"))
            case Left(e)  => ZIO.succeed(Verdict.Deny(s"the user couldn't be asked (${e.message}), and $why"))

  /** How long a call put to Jev waits for others to share its request (a batch's calls arrive together). */
  val Window: Duration = 30.millis

  /** How the user is asked: ACP's `session/request_permission` (the client connection's). */
  type Permission = RequestPermissionRequest => IO[RpcError, RequestPermissionResponse]

  /** A session's guard, its Jev requests made by a fiber of `scope`'s. */
  def make(policy: Policy, judge: List[Safety.Call] => Task[List[Double]], permission: Permission, sessionId: SessionId,
      root: Option[Path], dependencies: List[Path], window: Duration = Window): URIO[Scope, Session] =
    for
      pending    <- Queue.unbounded[Pending]
      remembered <- Ref.make(Map.empty[String, Boolean])
      _          <- (pending.take.flatMap(first => ZIO.sleep(window) *> pending.takeAll.map(first +: _)).flatMap: batch =>
                      judge(batch.map(_.call).toList).timeout(30.seconds).either.flatMap:
                        case Right(Some(ps)) if ps.size == batch.size => ZIO.foreachDiscard(batch.zip(ps))((b, p) => b.answer.succeed(Some(p)))
                        case other =>
                          ZIO.logWarning(s"guard: Jev couldn't judge ${batch.size} call(s): ${other.fold(_.getMessage, _ => "no answer")}") *>
                            ZIO.foreachDiscard(batch)(_.answer.succeed(None))
                    ).forever.forkScoped
    yield Session(policy, pending, remembered, permission, sessionId, root, dependencies)
