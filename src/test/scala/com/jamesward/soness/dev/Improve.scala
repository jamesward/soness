package com.jamesward.soness.dev

import com.jamesward.soness.{EventLog, LocalTools, SourceDependencies}

import zio.*
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * Self-improvement: soness improving its own project (a clone of soness, [[Observer.isSoness]]) in rounds that a
 * session runs as a task of its own, in the background, so the user can prompt while they run: a message steers it
 * (every round's loop reads it), asks how it's going, or stops it, as for any running task.
 *
 * What a round improves comes from the [[Observer]]'s findings (and a debug log, when one is given). A round's
 * [[Steps]] get it ready and, once the session has worked on it, check what the round changed ([[Local]]'s gates) and
 * give the verdict. The round edits the project itself (and, where the fix is theirs, its source dependencies), live,
 * alongside whatever else is working on them: there's no copy, nothing set aside, put back or undone. What it changed
 * is its task's own edits, read from the project's event log.
 */
object Improve:

  /** What the session's task for self-improvement is called (its request, as `/status` and the routing see it). */
  val Name = "self-improvement"

  final case class Options(log: Option[Path], rounds: Int, eval: Boolean, baseline: Option[String]) derives CanEqual

  /** A round, ready: its record's directory, the project it works on, and its request. */
  final case class Round(round: Int, out: Path, project: Path, prompt: String) derives CanEqual

  /** A round's verdict: `next` is the next round's evidence (this round's eval log), when there is one. */
  final case class Verdict(result: String, next: Option[Path] = None, files: List[String] = Nil) derives JsonCodec:
    def noChange: Boolean = result == NoChange
    def accepted: Boolean = result.startsWith("accepted") || result.startsWith("tests pass")

    def describe: String = result + (if files.isEmpty then "" else s" (${files.mkString(", ")})")

  given JsonCodec[Path] = JsonCodec.string.transform(Path.of(_), _.toString)

  /** The task that worked on a round: its session and its number there, whose edits are the round's change. */
  final case class By(session: String, task: Int) derives CanEqual

  val NoChange = "no change"

  /** A round's two ends, around the session's work on the project. */
  trait Steps:
    /** Round `n` ready to work on, with what earlier rounds did in its request. */
    def prepare(o: Options, n: Int, log: Option[Path], history: List[String]): Task[Round]

    /** What the round changed (the edits of the task `by` that worked on it), checked: the verdict. */
    def gate(o: Options, round: Round, by: By): Task[Verdict]

  /** The options of a `self_improve` call: the log (relative to `root`), and the rounds and gates. */
  def options(arguments: Json.Obj, root: Option[Path]): Either[String, Options] =
    def str(k: String) = arguments.get(k).flatMap(_.asString).map(_.trim).filter(_.nonEmpty)
    def bool(k: String, default: Boolean) = arguments.get(k).flatMap(_.asBoolean).getOrElse(default)
    for
      rounds <- arguments.get("rounds").fold(Right(1))(_.asNumber.map(_.value.intValue).filter(n => n >= 1 && n <= 20)
                  .toRight("rounds is a number from 1 to 20"))
    yield Options(str("log").map(l => root.fold(Path.of(l))(_.resolve(l)).toAbsolutePath.normalize), rounds,
      bool("eval", true), str("baseline"))

  /** One line for the next rounds' requests: what this round did, and how it was judged. */
  def history(verdict: Verdict, reply: Option[String]): String =
    val said = reply.map(_.replaceAll("\\s+", " ").trim).filter(_.nonEmpty).fold("")(r => ": " + (if r.length > 300 then r.take(297) + "…" else r))
    verdict.describe + said

  /** A round's request: what to improve, from the findings (in the context) and the log, if any. */
  def prompt(n: Int, project: Path, log: Option[Path], history: List[String]): String =
    val evidence = log.fold("")(l =>
      s"\n\nA soness debug log of real work is at $l too: transcript_turns, transcript_turn and transcript_call show its turns.")
    s"""Improve soness (this project, the agent harness). What the observer found while soness worked on itself is in
       |the context: pick the finding that matters most and that the code can fix.$evidence
       |
       |The turns it found were soness working on someone's request: don't redo that request's work. The tools those
       |turns called are mostly the client's (an IDE's tools) or the project's, not defined here: this project's code is
       |how the harness chose them, described them, wrote their arguments and handled their results. Find what in this
       |project's code made the harness behave that way, change it, add or update a test that fails without the change,
       |and run the relevant tests until they pass. Keep the change small and focused on that one cause.
       |${if history.isEmpty then "" else "Earlier rounds: " + history.mkString("; ") + "."}
       |
       |This is round $n of a self-improvement run, on $project itself: edit its files (and, where the cause is in
       |one, its source dependencies' sibling checkouts) directly. What this project's files then are is judged after
       |this reply by its gates (a code change with a test, the offline suite, the quick eval when asked). In your reply
       |say: the finding you picked, the cause in the code, the change (files), and the tests run.""".stripMargin

  // ---------- the gates' pure parts ----------

  /**
   * The files a patch changes, relative to the project, with whether any changed line is more than a comment: a file's
   * blocks taken together (a task's patch from the event log has a block for each of its edits of a file).
   */
  def changedFiles(diff: String): Map[String, Boolean] =
    val (files, _) = diff.linesIterator.foldLeft((Vector.empty[(String, Boolean)], Option.empty[String])):
      case ((acc, current), line) =>
        """\+\+\+ [^/]+/(\S+)""".r.findPrefixMatchOf(line) match
          case Some(m) => (acc :+ (m.group(1) -> false), Some(m.group(1)))
          case None =>
            val code = current.isDefined && (line.startsWith("+") || line.startsWith("-")) && !line.startsWith("+++") &&
              !line.startsWith("---") && {
                val body = line.drop(1).trim
                body.nonEmpty && !List("//", "*", "/*", "#").exists(body.startsWith)
              }
            if code then (acc.map((f, real) => if current.contains(f) then f -> true else f -> real), current) else (acc, current)
    files.groupMapReduce(_._1)(_._2)(_ || _)

  /** The ZIO specs (objects extending ZIOSpecDefault) in a test file's text, fully named. */
  def specNames(text: String): List[String] =
    val pkg = """(?m)^package (\S+)""".r.findFirstMatchIn(text).map(_.group(1))
    """(?m)^object (\w+) extends ZIOSpecDefault""".r.findAllMatchIn(text).map(m => pkg.fold(m.group(1))(p => s"$p.${m.group(1)}")).toList

  /**
   * A changed file as the gates name it: the project's relative path (`src/…`), a source dependency's `<name>/…`, or
   * the absolute path of a file outside both.
   */
  def relative(file: Path, project: Path, dependencies: List[Path]): String =
    if file.startsWith(project) then project.relativize(file).toString
    else dependencies.find(file.startsWith(_)).fold(file.toString)(d => s"${d.getFileName}/${d.relativize(file)}")

  /** A task's changes (from the event log) as one patch: each file's changes in turn, its files named [[relative]]ly. */
  def patchOf(changes: List[EventLog.Event.FileChanged], project: Path, dependencies: List[Path]): String =
    changes.map: c =>
      val name = relative(Path.of(c.file), project, dependencies)
      c.patch.linesIterator.map:
        case l if l.startsWith("--- a/") => s"--- a/$name"
        case l if l.startsWith("+++ b/") => s"+++ b/$name"
        case l => l
      .mkString("", "\n", "\n")
    .mkString

  /** Whether a changed file (the project's `src/main/…`, or a dependency's `<name>/src/main/…`) is main code. */
  def isMain(file: String): Boolean = file.startsWith("src/main/") || """[^/]+/src/main/.*""".r.matches(file)

  /** Whether a changed file (the project's, or a dependency's) is a test source. */
  def isTest(file: String): Boolean =
    (file.startsWith("src/test/") || """[^/]+/src/test/.*""".r.matches(file)) && file.endsWith(".scala")

  /** The offline suite's counts, `(passed, failed)`, from sbt's output: its last summary. */
  def testCounts(output: String): Option[(Int, Int)] =
    """(\d+) tests passed\. (\d+) tests failed""".r.findAllMatchIn(output).toList.lastOption.map(m => m.group(1).toInt -> m.group(2).toInt)

  /** An eval label's parts passed and run, from the evals' records. */
  def evalParts(records: List[String], label: String): (Int, Int) =
    records.flatMap(_.fromJson[Json.Obj].toOption).filter(_.get("label").flatMap(_.asString).contains(label))
      .flatMap(_.get("extra").flatMap(_.asObject).flatMap(_.get("parts")).flatMap(_.asObject).toList)
      .foldLeft((0, 0))((acc, parts) => (acc._1 + parts.fields.count(_._2.asBoolean.contains(true)), acc._2 + parts.fields.size))

  /** `text` single-quoted for bash. */
  def quote(text: String): String = "'" + text.replace("'", "'\\''") + "'"

  // ---------- the rounds on this machine ----------

  /**
   * The rounds' steps on this machine, for the project at `root`, which the rounds edit directly: a round's record (its
   * request, its change as its edits made it, outputs and verdict) is in `target/improve/<stamp>-r<n>/`.
   */
  final class Local(root: Path) extends Steps:
    private val Offline = "env -u TYPESAFE_API_KEY -u AWS_BEARER_TOKEN_BEDROCK -u BEDROCK_MODEL_ID"

    /** `command` by bash in `dir`: its exit code and output. */
    private def sh(dir: Path, command: String, seconds: Int = 3600): Task[(Int, String)] =
      LocalTools.run(dir, command, seconds).map: json =>
        val o = json.asObject
        (o.flatMap(_.get("exit_code")).flatMap(_.asNumber).map(_.value.intValue).getOrElse(-1),
          o.flatMap(_.get("output")).flatMap(_.asString).getOrElse(""))

    private def write(path: Path, text: String): Task[Unit] =
      ZIO.attemptBlocking { Files.createDirectories(path.getParent); Files.writeString(path, text, UTF_8) }.unit

    private lazy val dependencies: List[Path] = SourceDependencies.of(root)

    /** A changed file's path: the project's, or a source dependency's (`<name>/src/…`). */
    private def resolve(file: String): Path =
      if file.startsWith("src/") then root.resolve(file)
      else dependencies.find(_.getFileName.toString == file.takeWhile(_ != '/')).fold(root.resolve(file))(d =>
        d.resolve(file.dropWhile(_ != '/').drop(1)))

    /** A dependency's `specs`, in its build through the project's sbt server: its project by its directory's name, else `root`. */
    private def dependencyTests(dep: String, specs: List[String]): Task[(Int, String)] =
      val build = dependencies.find(_.getFileName.toString == dep).fold(root.resolve("..").resolve(dep))(identity)
      def run(id: String) = sh(root, s"$Offline ./sbta ${quote(s"{${build.toUri}}$id / Test / testOnly ${specs.mkString(" ")}")}", 3600)
      run(dep).flatMap((code, out) => if code != 0 && out.contains("Not a valid") then run("root") else ZIO.succeed((code, out)))

    def prepare(o: Options, n: Int, log: Option[Path], history: List[String]): Task[Round] =
      for
        stamp <- Clock.localDateTime.map(_.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")))
        out    = root.resolve("target").resolve("improve").resolve(s"$stamp-r$n")
        _     <- ZIO.attemptBlocking(Files.createDirectories(out))
        _     <- ZIO.foreachDiscard(log)(l => ZIO.attemptBlocking(Files.copy(l, out.resolve("evidence.log"))))
        round  = Round(n, out, root, prompt(n, root, log, history))
        _     <- write(out.resolve("round.json"), Json.Obj("round" -> Json.Num(n), "project" -> Json.Str(root.toString),
                   "prompt" -> Json.Str(round.prompt)).toJsonPretty)
      yield round

    def gate(o: Options, round: Round, by: By): Task[Verdict] =
      val out = round.out
      def done(v: Verdict) = write(out.resolve("verdict.json"), v.toJsonPretty).as(v)
      for
        // the round's change is its task's own edits, as the event log has them: not a diff against a copy, so what
        // others change in the same files meanwhile isn't the round's
        events <- EventLog.at(root).read
        diff    = patchOf(EventLog.changes(events, by.session, by.task), root, dependencies)
        _      <- write(out.resolve("change.patch"), diff)
        files   = changedFiles(diff)
        code    = files.collect { case (f, true) if isMain(f) => f }.toList
        tests   = files.keys.filter(isTest).toList
        texts  <- ZIO.foreach(tests)(t => ZIO.attemptBlocking(Files.readString(resolve(t), UTF_8)).orElseSucceed("").map(t -> _))
        specs   = texts.flatMap(t => specNames(t._2))
        // a dependency's specs the round changed: run in its build, through the project's sbt server
        depSpecs = texts.collect { case (t, text) if !t.startsWith("src/") => t.takeWhile(_ != '/') -> specNames(text) }
                     .groupMapReduce(_._1)(_._2)(_ ++ _).filter(_._2.nonEmpty)
        listed  = files.keys.toList.sorted
        verdict <-
          if diff.trim.isEmpty then done(Verdict(NoChange))
          else if code.isEmpty then done(Verdict("rejected: no code change beyond comments", files = listed))
          else if specs.isEmpty then done(Verdict("rejected: no test of the change", files = listed))
          else
            for
              // through the project's own sbt server (the session's): a second server on the project would clash
              deps  <- ZIO.foreach(depSpecs.toList)((dep, ss) => dependencyTests(dep, ss))
              main  <- sh(root, s"$Offline ./sbta testFull", 3600)
              suite  = deps.collectFirst { case (code, out) if code != 0 => (code, out) }.getOrElse(main) match
                         case (code, out) => (code, (deps.map(_._2) :+ main._2).mkString("\n"))
              _     <- write(out.resolve("tests.out"), suite._2)
              counts = testCounts(suite._2).fold("")((p, f) => s", $p passed, $f failed")
              v     <-
                if suite._1 != 0 then done(Verdict(s"rejected: the offline tests fail$counts", files = listed))
                else if !o.eval then done(Verdict(s"tests pass (no eval)$counts", files = listed))
                else evaluated(o, round, listed, counts).flatMap(done)
            yield v
      yield verdict

    /** The quick eval of the project as the round left it, against the baseline label's: within 5% of its parts rate. */
    private def evaluated(o: Options, round: Round, files: List[String], counts: String): Task[Verdict] =
      val label = s"self-${round.out.getFileName}"
      val records = root.resolve("target/evals/parallel-tasks.jsonl")
      for
        ran     <- sh(root, s"bash scripts/quick.sh ${quote(label)} boundCalls,noRepeats,correct,batch 2", 3600)
        _       <- write(round.out.resolve("eval.out"), ran._2)
        lines   <- ZIO.attemptBlocking(if Files.exists(records) then Files.readAllLines(records, UTF_8).toArray.toList.map(_.toString) else Nil)
        now      = evalParts(lines, label)
        before   = evalParts(lines, o.baseline.getOrElse("q-numfix"))
        rate     = (p: (Int, Int)) => if p._2 == 0 then 0.0 else p._1.toDouble / p._2
        log      = root.resolve(s"target/evals/logs/$label.log")
        next    <- ZIO.attemptBlocking(Option.when(Files.exists(log))(log))
        result   = if now._2 > 0 && rate(now) >= rate(before) - 0.05 then s"accepted: eval ${now._1}/${now._2} (baseline ${before._1}/${before._2})$counts"
                   else s"rejected: the eval regressed, ${now._1}/${now._2} (baseline ${before._1}/${before._2})"
      yield Verdict(result, next, files = files)
