package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.AgentLoop.Document
import com.jamesward.zio_decisions.orchestration.ProcessTree
import com.jamesward.zioacp.schema.AvailableCommand
import zio.*

import java.nio.file.Path

/**
 * What a build of soness adds to the agent, beyond what every soness does: processes started with each session, tools
 * offered to each task, and session commands. Production soness has none; the development build (`Test / stage`) adds
 * self-improvement this way. An extension sees a session through [[Extension.Session]], never the agent's internals.
 */
trait Extension:
  /** Started when a session opens (forked in the session's scope with [[Extension.Session.fork]] when it goes on). */
  def started(session: Extension.Session): UIO[Unit] = ZIO.unit

  /** Tools for one task of the session; none by default. */
  def tools(task: Extension.Task): Option[Toolset] = None

  /** Tools whose successful call is the request's whole work (they start something that goes on by itself). */
  def endsTurn: Set[String] = Set.empty

  /** The commands it answers in `session`, advertised with the session's own (none where it doesn't apply). */
  def commands(session: Extension.Session): List[AvailableCommand] = Nil

  /** The answer to `/<name> <input>`, when it's one of the session's [[commands]]. */
  def command(session: Extension.Session, name: String, input: String): Option[UIO[String]] = None

object Extension:

  /** A directory's tools and its MCP servers' instructions (`server -> text`). */
  final case class Tools(toolset: Toolset, instructions: List[(String, String)])

  /** A session, as an extension sees it. */
  trait Session:
    def id: String
    def cwd: Option[Path]
    def tree: ProcessTree

    /** The session's node in the tree (the agent's, every session's): its processes go below it. */
    def node: ProcessTree.Id
    /** The session's own tools (its MCP servers', the client's, soness's local ones), unreported. */
    def toolset: Toolset
    /** The harness of the session's current model. */
    def harness: zio.Task[Harness]
    /** The session's turns as they end, from now on (shared state, not a log: every task publishes its turn). */
    def ended: ZIO[Scope, Nothing, Dequeue[Transcript.Turn]]
    /** Says `text` in the session's chat. */
    def say(text: String): UIO[Unit]
    /** Runs `zio` in the background for the session's life. */
    def fork(zio: UIO[Any]): UIO[Unit]
    /**
     * The tools of a directory (its `.mcp.json` servers, soness's local tools on it, web fetch, transcript reading),
     * sharing its coordinator with every session there, and its servers' instructions.
     */
    def toolsIn(dir: Path): ZIO[Scope, Throwable, Tools]
    /** A directory's required skills and its context (what's known about it, `instructions` of its servers). */
    def contextOf(dir: Path, instructions: List[(String, String)]): UIO[(List[Document], Option[String])]
    /**
     * Starts `work` as a task of the session in the background, of the extension's own accord (no request of the
     * user's launched it): routed to like any running task (steering, a stop, a question about it). `label` marks what
     * it says and its tool calls in the chat (`self-improvement`), so they're told from the user's own tasks. Its id
     * and process.
     */
    def background(request: String, label: String)(work: Background => zio.Task[String]): zio.Task[(Int, ProcessTree.Id)]

  /** A task of the session, as an extension sees it while offering it tools. */
  trait Task:
    def session: Session
    /** How it came to be: `""` for a prompt, `watch`, `new`, `after:N`, `steer:N`. */
    def route: String
    /**
     * Starts `work` as a task of the session in the background, taking this task's place: routed to like any running
     * task (steering, a stop, a question about it), its steering so far handed over; `label` marks what it says and its
     * tool calls in the chat. The task's id and process.
     */
    def background(request: String, label: String)(work: Background => zio.Task[String]): zio.Task[(Int, ProcessTree.Id)]

  /** A background task, as its work sees it. */
  trait Background:
    def id: Int
    def node: ProcessTree.Id
    /**
     * Says `text` in the chat, marked with the task's label: at once while a prompt turn of the session is open (a
     * client shows a message only then), else when the next one starts.
     */
    def say(text: String): UIO[Unit]
    /** The user's messages about it, read again as it goes. */
    def steering: UIO[List[String]]
    /** `toolset` with its calls reported to the client as this task's. */
    def reported(toolset: Toolset): Toolset
    /** `zio` as loop `name` of this task (`<session>/task-N/<name>` in the logs and the tools' coordination). */
    def loop[A](name: String)(work: _root_.zio.Task[A]): _root_.zio.Task[A]
