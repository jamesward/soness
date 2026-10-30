package com.jamesward.soness

import zio.*
import zio.stream.ZStream

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardWatchEventKinds}

/**
 * A log being written, followed: its new lines as they're appended (the file system says when it changes, through a
 * `WatchService`: nothing polls), and the turns of a soness debug log as they end ([[Transcript]]). A file that's
 * truncated or replaced is read again from its start.
 */
object Watch:

  /** How long an entry with nothing after it waits before it's taken as complete (the log's last entry). */
  val Settle: Duration = 1.second

  /** The lines appended to `file` from now on (or, `fromStart`, from its start), for as long as the stream is read. */
  def lines(file: Path, fromStart: Boolean = false, from: Option[Long] = None): ZStream[Any, Throwable, String] =
    ZStream.unwrapScoped:
      for
        abs     <- ZIO.succeed(file.toAbsolutePath.normalize)
        service <- ZIO.fromAutoCloseable(ZIO.attemptBlocking(abs.getFileSystem.newWatchService()))
        _       <- ZIO.attemptBlocking(abs.getParent.register(service, StandardWatchEventKinds.ENTRY_MODIFY,
                     StandardWatchEventKinds.ENTRY_CREATE))
        // from where the caller says (measured when the watch was asked for: what's written while this starts is kept)
        offset  <- ZIO.attemptBlocking(from.getOrElse(if fromStart || !Files.exists(abs) then 0L else Files.size(abs))).flatMap(Ref.make)
        partial <- Ref.make("")
      yield
        // what's there now, then again each time the directory says the file changed
        val changes = ZStream.unit ++ ZStream.repeatZIO(ZIO.attemptBlockingInterrupt {
          val key = service.take()
          val touched = key.pollEvents().toArray.exists(e => Option(e.asInstanceOf[java.nio.file.WatchEvent[Path]].context()).exists(_.toString == abs.getFileName.toString))
          key.reset()
          touched
        }).filter(identity).as(())
        changes.mapZIO(_ => read(abs, offset, partial)).flattenChunks

  /** The complete lines added since `offset`; a line not yet ended waits in `partial`. */
  private def read(file: Path, offset: Ref[Long], partial: Ref[String]): Task[Chunk[String]] =
    for
      from  <- offset.get
      size  <- ZIO.attemptBlocking(if Files.exists(file) then Files.size(file) else 0L)
      start  = if size < from then 0L else from
      _     <- partial.set("").when(size < from)
      bytes <- ZIO.attemptBlocking:
                 val channel = java.nio.channels.FileChannel.open(file)
                 try
                   val buffer = java.nio.ByteBuffer.allocate((size - start).toInt.max(0))
                   channel.read(buffer, start)
                   buffer.array()
                 finally channel.close()
      _     <- offset.set(start + bytes.length)
      lines <- partial.modify: before =>
                 val text = before + String(bytes, UTF_8)
                 val cut = text.lastIndexOf('\n')
                 if cut < 0 then (Chunk.empty, text) else (Chunk.fromArray(text.substring(0, cut).split("\n", -1)), text.substring(cut + 1))
    yield lines

  /**
   * The turns of the soness debug log `file` that end from now on, each when its metrics (or its failure) is logged.
   * An entry is complete when the next one starts, or when nothing follows it for [[Settle]].
   */
  def endedTurns(file: Path, fromStart: Boolean = false, from: Option[Long] = None): ZStream[Any, Throwable, Transcript.Turn] =
    ZStream.unwrapScoped:
      for
        queue <- Queue.unbounded[Option[String]]
        _     <- lines(file, fromStart, from).map(Some(_)).runForeach(queue.offer).forkScoped
        out   <- Queue.unbounded[Transcript.Turn]
        state <- Ref.make((Transcript.State(), Vector.empty[String]))
        // one entry's lines at a time: an entry ends when the next starts, or when the log falls quiet
        flush  = state.modify { case (s, pending) =>
                   pending.headOption.flatMap(_ => Transcript.record(pending.mkString("\n"))) match
                     case Some(r) => val (next, ended) = Transcript.step(s, r); (ended, (next, Vector.empty))
                     case None    => (Nil, (s, Vector.empty))
                 }.flatMap(ended => out.offerAll(ended))
        _     <- queue.take.timeout(Settle).flatMap {
                   case Some(Some(line)) if line.startsWith("timestamp=") => flush *> state.update((s, _) => (s, Vector(line)))
                   case Some(Some(line))                                  => state.update((s, p) => (s, if p.isEmpty then p else p :+ line))
                   case _                                                 => flush
                 }.forever.forkScoped
      yield ZStream.fromQueue(out)
