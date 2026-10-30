package com.jamesward.soness

import com.jamesward.zioacp.schema.{AvailableCommand, UnstructuredCommandInput}

/**
 * The session's commands, advertised to the client (ACP's `available_commands_update`) and answered at once from the
 * session's process tree, whether or not work is running: `/status` (what's running, with what looks stalled), `/status
 * all` (what ran too), `/stop <id>` (stop one process: a task, a loop, a tool call; a stopped call is a failed step and
 * its loop goes on). A build's extensions add their own ([[Extension.commands]]).
 */
object Commands:
  enum Command derives CanEqual:
    case Status, StatusAll
    case Stop(id: Long)
    case Unknown(text: String)

  val available: List[AvailableCommand] = List(
    AvailableCommand("status", "What the agent is working on: its tasks, loops and tool calls, and what looks stalled",
      Some(UnstructuredCommandInput("all, to show what has ended too"))),
    AvailableCommand("stop", "Stop one process of the work by its id (from /status): a task, a loop or a tool call",
      Some(UnstructuredCommandInput("the process id"))),
  )

  val usage: String = "/status shows the work; /stop <id> stops a process (a stopped tool call is a failed step; its task goes on)."

  /** A prompt's command name and the rest, when it starts with one: `/findings all` is `findings` and `all`. */
  def named(text: String): Option[(String, String)] =
    """(?s)\s*/([A-Za-z][\w-]*)\s*(.*)""".r.findPrefixMatchOf(text).map(m => m.group(1) -> m.group(2).trim)

  /** The command a prompt is, when it's one: `/status`, `/status all`, `/stop <id>`. */
  def parse(text: String): Option[Command] =
    val words = text.trim.split("\\s+").toList
    words match
      case "/status" :: Nil          => Some(Command.Status)
      case "/status" :: "all" :: Nil => Some(Command.StatusAll)
      case "/stop" :: id :: Nil      => Some(id.stripPrefix("[").stripSuffix("]").toLongOption.fold(Command.Unknown(text.trim))(Command.Stop(_)))
      case ("/status" | "/stop") :: _ => Some(Command.Unknown(text.trim))
      case _                         => None
