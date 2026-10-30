package com.jamesward.soness

import com.jamesward.zio_decisions.orchestration.ProcessTree
import zio.*
import zio.json.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.jdk.CollectionConverters.*

/**
 * The process trees of the user's other soness processes (other ACP clients: an IDE and a CLI, two IDEs), so `/status`
 * in any session shows all of the user's work, not only its own process's. Each process publishes its tree to
 * `<dir>/<pid>.json` as it changes (told by the tree, not polled) and removes it when it exits; a reader takes the
 * files of the processes still alive (and removes the others'). Their ids are their own process's: `/stop` reaches
 * only this process's.
 */
object Peers:

  /** The directory the user's soness processes publish to: under the user's cache, as the project locks are. */
  def defaultDir: Path =
    Path.of(java.lang.System.getProperty("user.home"), ".cache", "soness", "processes")

  final case class Wire(id: Long, parent: Option[Long], kind: String, name: String, detail: Option[String], state: String,
      started: String, lastActive: String, ended: Option[String], outcome: Option[String]) derives JsonCodec

  final case class Published(pid: Long, at: String, nodes: List[Wire]) derives JsonCodec

  /** Another process's tree: its pid, and its nodes. */
  final case class Peer(pid: Long, nodes: Vector[ProcessTree.Node])

  private def wire(n: ProcessTree.Node): Wire =
    Wire(n.id.value, n.parent.map(_.value), n.kind.toString, n.name, n.detail, n.state.toString, n.started.toString,
      n.lastActive.toString, n.ended.map(_.toString), n.outcome)

  private def node(w: Wire): Option[ProcessTree.Node] =
    scala.util.Try(ProcessTree.Node(ProcessTree.Id(w.id), w.parent.map(ProcessTree.Id(_)), ProcessTree.Kind.valueOf(w.kind), w.name,
      w.detail, ProcessTree.State.valueOf(w.state), Instant.parse(w.started), Instant.parse(w.lastActive),
      w.ended.map(Instant.parse), w.outcome)).toOption

  private val self: Long = ProcessHandle.current().pid()

  /** Publishes `tree` to `dir` for the life of the scope: on every change, at most every [[Settle]], and once removed. */
  def publish(tree: ProcessTree, dir: Path): URIO[Scope, Unit] =
    val file = dir.resolve(s"$self.json")
    def write: UIO[Unit] =
      (tree.snapshot <*> Clock.instant).flatMap: (nodes, now) =>
        ZIO.attemptBlocking:
          Files.createDirectories(dir)
          val tmp = Files.createTempFile(dir, s"$self", ".tmp")
          Files.writeString(tmp, Published(self, now.toString, nodes.map(wire).toList).toJson, UTF_8)
          Files.move(tmp, file, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
          ()
        .catchAll(e => ZIO.logDebug(s"peers: couldn't publish to $file: ${e.getMessage}"))
    for
      _       <- ZIO.addFinalizer(ZIO.attemptBlocking(Files.deleteIfExists(file)).ignore)
      changes <- tree.changes
      // a change, then whatever else changed while it settles, written once
      _       <- (changes.take *> ZIO.sleep(Settle) *> changes.takeAll *> write).forever.forkScoped
      _       <- write
    yield ()

  /** How long a change waits for the ones right after it before the tree is written. */
  val Settle: Duration = 250.millis

  /** The other live processes' trees, in `dir`. */
  def others(dir: Path): UIO[List[Peer]] =
    ZIO.attemptBlocking:
      if !Files.isDirectory(dir) then Nil
      else
        val files = Files.list(dir)
        try files.iterator.asScala.toList.filter(_.toString.endsWith(".json")) finally files.close()
    .orElseSucceed(Nil).flatMap: files =>
      ZIO.foreach(files): f =>
        ZIO.attemptBlocking:
          val pid = f.getFileName.toString.stripSuffix(".json").toLongOption
          pid.filter(_ != self).flatMap: p =>
            if ProcessHandle.of(p).map(_.isAlive).orElse(false) then
              Files.readString(f, UTF_8).fromJson[Published].toOption.map(pub => Peer(p, pub.nodes.flatMap(node).toVector))
            else
              Files.deleteIfExists(f)
              None
        .orElseSucceed(None)
      .map(_.flatten.sortBy(_.pid))
