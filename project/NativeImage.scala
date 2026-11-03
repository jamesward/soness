import com.typesafe.sbt.packager.Keys.{scriptClasspath, stage}
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport.Universal
import sbt.*
import sbt.Keys.*

import scala.sys.process.{Process, ProcessLogger}

/**
 * `nativeImage`: the application as a native executable (GraalVM native-image), built in a container
 * (`project/native/Dockerfile`: GraalVM's native-image image, with UPX) for the host's architecture, so building it
 * needs Docker, not a local GraalVM. It compiles the staged application (`Universal / stage`) and compresses the
 * result with UPX unless `-Dnative.upx=false`. The executable is `target/native/<name>`.
 */
object NativeImage extends AutoPlugin {
  override def requires: Plugins = JavaAppPackaging

  object autoImport {
    val nativeImageOptions = settingKey[Seq[String]]("native-image's options")
    val nativeImage = taskKey[File]("Builds the native executable in a container: target/native/<name>")
  }
  import autoImport.*

  override def projectSettings: Seq[Setting[?]] = Seq(
    nativeImageOptions := Seq(
      "--no-fallback",
      "-Os", // optimize for size: 61 MiB instead of 75 MiB, and the same startup
      "-H:+ReportExceptionStackTraces",
      "--enable-url-protocols=http,https",
      // Netty frees its buffers with Java 25's shared arenas; without this its event loop threads die on shutdown
      // and SIGTERM hangs
      "-H:+SharedArenaSupport",
      // the classpath resources the application reads (native-image embeds only the ones it is told to)
      "-H:IncludeResources=^(tool-catalog|intellij-mcp-tools)\\.json$",
    ),
    nativeImage := Def.uncached {
      val log = streams.value.log
      val processLog = ProcessLogger(line => log.info(line), line => log.warn(line))
      def run(command: Seq[String]): Unit = {
        log.info(command.mkString(" "))
        val exit = Process(command).!(processLog)
        if (exit != 0) sys.error(s"${command.take(2).mkString(" ")} … failed (exit $exit)")
      }
      val staged = (Universal / stage).value
      val classpath = scriptClasspath.value.map(jar => s"/stage/lib/$jar").mkString(":")
      val main = (Compile / mainClass).value.getOrElse(sys.error("no main class"))
      val binary = name.value
      val out = target.value / "native"
      IO.delete(out)
      IO.createDirectory(out)
      val image = s"$binary-native-image"
      run(Seq("docker", "build", "-q", "-t", image, (baseDirectory.value / "project" / "native").getAbsolutePath))
      // as the build's user, so what it writes isn't root's; rootless Docker's root is the build's user already (and
      // the build's own user id there is another, which can't write the build's files)
      val rootless = Process(Seq("docker", "info", "--format", "{{.SecurityOptions}}")).!!.contains("rootless")
      val user = if (rootless) Nil else Seq("--user", s"${Process("id -u").!!.trim}:${Process("id -g").!!.trim}")
      val container = Seq("docker", "run", "--rm") ++ user ++ Seq("-e", "HOME=/tmp",
        "-v", s"${staged.getAbsolutePath}:/stage:ro", "-v", s"${out.getAbsolutePath}:/out", "-w", "/out", image)
      run(container ++ Seq("native-image", "-cp", classpath, "-o", binary) ++ nativeImageOptions.value :+ main)
      if (sys.props.get("native.upx").forall(_ != "false")) run(container ++ Seq("upx", "--best", "-q", s"/out/$binary"))
      out / binary
    },
  )
}
