name := "jevness"

scalaVersion := "3.10.0"

scalacOptions ++= Seq(
  "-language:strictEquality",
  "-deprecation",
  "-Werror",
)

// sbt-mcp settings (loopback-only: its tools can execute build tasks)
mcpEnabled := true
mcpHost := "127.0.0.1"
mcpPort := 5171

// SkillsJars: extract agent Skills with `./sbt extractSkillsJars`
skillsJarsOutputDir := Some(file(".kiro/skills"))

val zioVersion = "2.1.26"

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"             % zioVersion,
  "dev.zio"       %% "zio-http"        % "3.11.6",
  "com.jamesward" %% "zio-acp"         % "0.0.1",
  "com.jamesward" %% "zio-typesafe-ai" % "0.1.0",
  "com.jamesward" %% "zio-bedrock"     % "0.1.0",
  "com.jamesward" %% "zio-http-mcp"    % "0.9.0",

  "dev.zio"       %% "zio-test"     % zioVersion % Test,
  "dev.zio"       %% "zio-test-sbt" % zioVersion % Test,
  "com.jamesward" %% "zio-evals"    % "0.2.0"    % Test,

  "com.jamesward" % "skills" % "0.0.12" % Skills,
)

enablePlugins(JavaAppPackaging)

Compile / mainClass := Some("com.jamesward.jevness.Main")

fork := true

javaOptions ++= Seq(
  "--enable-native-access=ALL-UNNAMED",
  "--sun-misc-unsafe-memory-access=allow",
)

// Specs have main methods too; only Compile has an application entry point.
Test / mainClass := None
