name := "soness"

scalaVersion := "3.10.0"

scalacOptions ++= Seq(
  "-language:strictEquality",
  "-deprecation",
  "-Werror",
)

// SkillsJars: extract agent Skills with `./sbta extractSkillsJars`, into the cross-client location (`.agents/skills`,
// which soness reads too)
skillsJarsOutputDir := Some(file(".agents/skills"))

val zioVersion = "2.1.27"

// Development: zio-acp, zio-bedrock, zio-http-mcp, zio-decisions and (for tests) zio-evals from sibling checkouts
// until their fixes are released (see AGENTS.md). zio-evals takes its own zio-decisions from the sibling checkout
// too only with `-Dlocal`, which `.sbtopts` sets.
lazy val zioAcp = RootProject(file("../zio-acp"))
lazy val zioBedrock = RootProject(file("../zio-bedrock"))
lazy val zioHttpMcp = RootProject(file("../zio-http-mcp"))
lazy val zioDecisions = RootProject(file("../zio-decisions"))
lazy val zioEvals = RootProject(file("../zio-evals"))

lazy val root = (project in file(".")).dependsOn(zioAcp, zioBedrock, zioHttpMcp, zioDecisions, zioEvals % "test->compile")

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"             % zioVersion,
  "dev.zio"       %% "zio-http"        % "3.11.6",
  "dev.zio"       %% "zio-logging"     % "2.5.3",

  "dev.zio"       %% "zio-test"     % zioVersion % Test,
  "dev.zio"       %% "zio-test-sbt" % zioVersion % Test,

  "com.jamesward" % "skills" % "0.0.17" % Skills,

  // the log explorer's (test code, `LogExplorer`): Tailwind's browser build, served from the test classpath
  "org.webjars.npm" % "tailwindcss__browser" % "4.3.3" % Set(WebJar, Test),
)

enablePlugins(JavaAppPackaging)

Compile / mainClass := Some("com.jamesward.soness.Main")

fork := true

// Load .env variables for tests using sbt-dotenv
Test / envFileName := ".env"
Test / envVars := (Test / envFromFile).value

javaOptions ++= Seq(
  "--enable-native-access=ALL-UNNAMED",
  "--sun-misc-unsafe-memory-access=allow",
)

// Specs have main methods too; only Compile has an application entry point.
Test / mainClass := None

// The log explorer (test code) is Test's app: `~Test/runReload` (sbt-reload) runs it and restarts it on every change,
// its page's JS and CSS included; `Test / runReloadArgs` takes `--port` and the logs' paths
Test / run / mainClass := Some("com.jamesward.soness.inspector.LogExplorer")
Test / runReloadArgs := Seq("--static", "src/test/resources/log-explorer")
addCommandAlias("inspector", "~Test/runReload")

// The subprocess specs launch soness's Main on the runtime classpath, and test fixtures (the stdio MCP server
// standing in for IntelliJ IDEA's) on the test classpath.
Test / javaOptions ++= Def.uncached {
  val converter = fileConverter.value
  def classpath(entries: Classpath) = entries.map(entry => converter.toPath(entry.data).toFile).mkString(java.io.File.pathSeparator)
  Seq(
    s"-Dsoness.classpath=${classpath((Runtime / fullClasspath).value)}",
    s"-Dsoness.testClasspath=${classpath((Test / fullClasspath).value)}",
  )
}

// The development build: soness with self-improvement (`com.jamesward.soness.dev`: its extension and entry point are
// test code, which production soness doesn't ship). `Test / stage` stages it like `stage`, into `test-stage/` under
// the target: `bin/soness-dev`, with its own copies of the jars in `lib/` (under their own names; the launcher lists
// them in classpath order), so a later build doesn't change a running one.
Test / stage := Def.uncached {
  val converter = fileConverter.value
  val out = target.value / "test-stage"
  val lib = out / "lib"
  IO.delete(out)
  IO.createDirectory(lib)
  val entries = (Test / fullClasspath).value.map(entry => converter.toPath(entry.data).toFile).filter(_.exists)
  // each entry under its own name (a directory of classes as a jar of it); a name taken already gets a number
  val names = entries.foldLeft(Vector.empty[String]) { (taken, file) =>
    val base = if (file.isDirectory) s"${file.getParentFile.getName}-${file.getName}.jar" else file.getName
    val name = Iterator.from(1).map(n => if (n == 1) base else base.stripSuffix(".jar") + s"-$n.jar").find(n => !taken.contains(n)).get
    if (file.isDirectory) IO.zip(Path.allSubpaths(file), lib / name, None) else IO.copyFile(file, lib / name)
    taken :+ name
  }
  val classpath = names.map(n => "$here/lib/" + n).mkString(":")
  val options = (Compile / javaOptions).value.mkString(" ")
  val bin = out / "bin" / "soness-dev"
  IO.write(bin,
    s"""#!/usr/bin/env bash
       |# soness's development build (with self-improvement), staged by `Test / stage`
       |here="$$(cd "$$(dirname "$${BASH_SOURCE[0]}")/.." && pwd)"
       |exec java $options $${JAVA_OPTS:-} -cp "$classpath" com.jamesward.soness.dev.Main "$$@"
       |""".stripMargin)
  bin.setExecutable(true)
  // a goal, then soness improving itself from how it went (`com.jamesward.soness.dev.GoalRun`)
  val goal = out / "bin" / "soness-goal"
  IO.write(goal,
    s"""#!/usr/bin/env bash
       |# a goal for soness, then soness improving itself from how it went, staged by `Test / stage`
       |here="$$(cd "$$(dirname "$${BASH_SOURCE[0]}")/.." && pwd)"
       |exec java $options $${JAVA_OPTS:-} -cp "$classpath" com.jamesward.soness.dev.GoalRun "$$@"
       |""".stripMargin)
  goal.setExecutable(true)
  out
}
