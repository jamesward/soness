package com.jamesward.soness.dev

import com.jamesward.soness.Transcript

import zio.*
import zio.json.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/**
 * The observer: in the development build, when soness works on its own project (a clone of its repository,
 * [[isSoness]]), a process of the session (a node of its process tree, so `/status` shows it and `/stop` ends it) that
 * watches the session's own turns as they end: every task publishes its turn, calls and results included, in the
 * session's shared state, so no log is needed. The decision model judges each; for one that shows something about
 * soness that could be better (a failure, wasted or repeated calls, a slow step, a misleading reply), a read-only
 * analysis looks into this project's code for the cause and records a finding. It changes nothing, and says nothing in
 * the chat while the user works; each finding starts self-improvement rounds on it by itself ([[SelfImprovement]]).
 */
object Observer:

  /** Its process's name in the tree. */
  val Name = "observer"

  /** `SONESS_OBSERVER=false` turns it off. */
  val Variable = "SONESS_OBSERVER"

  /** How likely, by the decision model, a turn must be to show something to improve before it's analyzed. */
  val Threshold = 0.6

  /** The session annotation the observer's own requests are logged under, so they're never taken for the session's. */
  def ownSession(session: String): String = s"$session/observer"

  /** soness's repositories, by any of their names: a clone of one is soness's own project. */
  val Repositories: Set[String] = Set("jamesward/soness", "jamesward/jevness")

  /** A remote URL's GitHub `owner/name` (https or ssh, with or without `.git`), lower-cased. */
  def gitHubRepository(url: String): Option[String] =
    """(?i)github\.com[:/]+([^/\s]+)/([^/\s]+?)(?:\.git)?/*$""".r.findFirstMatchIn(url.trim)
      .map(m => s"${m.group(1)}/${m.group(2)}".toLowerCase)

  /** The remote URLs in a git config's text. */
  def remoteUrls(config: String): List[String] =
    """(?m)^\s*url\s*=\s*(\S+)\s*$""".r.findAllMatchIn(config).map(_.group(1)).toList

  /**
   * The git config of the repository at `root`: `.git/config`, or for a worktree or submodule (a `.git` file naming its
   * git directory) that directory's common one's.
   */
  def gitConfig(root: Path): Option[Path] =
    val git = root.resolve(".git")
    val dir =
      if Files.isDirectory(git) then Some(git)
      else if Files.isRegularFile(git) then
        scala.util.Try(Files.readString(git, UTF_8)).toOption.map(_.trim.stripPrefix("gitdir:").trim).map(d => root.resolve(d).normalize)
          .map: d =>
            val common = d.resolve("commondir")
            if Files.isRegularFile(common) then scala.util.Try(d.resolve(Files.readString(common, UTF_8).trim).normalize).getOrElse(d) else d
      else None
    dir.map(_.resolve("config")).filter(Files.isRegularFile(_))

  /** Whether `root` is a clone of soness: one of its remotes is one of [[Repositories]]. */
  def isSoness(root: Path): Boolean =
    gitConfig(root).flatMap(c => scala.util.Try(Files.readString(c, UTF_8)).toOption).toList
      .flatMap(remoteUrls).flatMap(gitHubRepository).exists(Repositories)

  /** A finding: from a turn the observer judged worth it (`feedback`: the user's word on it, in older findings). */
  final case class Finding(at: String, session: String, turn: String, request: String, ending: String, p: Double,
      summary: String, analysis: String, feedback: Option[String] = None) derives JsonCodec

  /** The project's findings file (next to its event log). */
  def file(project: Path): Path = project.resolve(".soness").resolve("findings.jsonl")

  def record(project: Path, finding: Finding): Task[Unit] =
    ZIO.attemptBlocking:
      val f = file(project)
      Files.createDirectories(f.getParent)
      Files.writeString(f, finding.toJson + "\n", UTF_8, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    .unit

  /** Every finding so far, oldest first; lines that don't parse are skipped. */
  def read(project: Path): UIO[List[Finding]] =
    ZIO.attemptBlocking(if Files.isRegularFile(file(project)) then Files.readAllLines(file(project), UTF_8).toArray.toList.map(_.toString) else Nil)
      .map(_.flatMap(_.fromJson[Finding].toOption)).orElseSucceed(Nil)

  /** A finding's first line: what the analysis's reply leads with. */
  def summary(analysis: String): String =
    // its markdown (a heading's #s, bold) left out
    val first = analysis.linesIterator.map(_.trim.dropWhile(_ == '#').trim.stripPrefix("**").stripSuffix("**").trim).find(_.nonEmpty).getOrElse("")
    if first.length > 200 then first.take(197) + "…" else first

  /** The findings as a context section for self-improvement rounds. */
  def context(findings: List[Finding], limit: Int = 8): Option[String] =
    Option.when(findings.nonEmpty):
      findings.takeRight(limit).map(f => s"- turn ${f.turn} (\"${Transcript.cut(f.request, 160)}\", ${f.ending})" +
        f.feedback.fold("")(fb => s", the user said: \"${Transcript.cut(fb, 600)}\"") + s":\n${f.analysis.take(2500)}")
        .mkString("What the observer found while soness worked on itself (newest last):\n", "\n\n", "")

  val Question = "Does this turn show something about the agent's own working (soness, the harness) that could be better?"
  val Yes = "it shows a problem or an improvement: a failure, a wrong, missing or misleading answer, failed or repeated " +
    "calls, wasted or slow steps, work going round in circles, the user having to correct or repeat themselves"
  val No = "it went as it should: answered well, without failures, repeats or wasted time worth improving"

  /** The analysis's request, for a turn the decision model judged worth it. */
  def request(turn: Transcript.Turn): String =
    s"""You are observing soness (this project, an agent harness) as it works on this same project for its user. A turn
       |just ended: ${turn.id} ("${Transcript.cut(turn.request, 200)}"), ${turn.ending}. What it shows (its calls, their
       |arguments and results, its answer) is in the context.
       |
       |Find what could be better in how soness did it, and why: what in this project's code (the harness: how it
       |chooses tools, describes them, writes their arguments, handles results, ends turns, replies) made it behave that
       |way. Read the code to point at the cause. Change nothing: this is only an observation. Don't redo the user's
       |request, and don't judge the user's own code.
       |
       |Reply with the finding in its first line (one sentence: what went wrong or could be better), then the evidence
       |(calls, times), the likely cause (files, functions), and a suggested change. When there's nothing in soness to
       |improve (the model's slip, or the turn went well), say so in the first line.""".stripMargin
