package com.jamesward.soness

import zio.test.*

object OptionsSpec extends ZIOSpecDefault:
  def spec = suite("Options")(
    test("no arguments is info logging to stderr only; --debug logs at debug level") {
      assertTrue(Options.parse(Nil) == Right(Options()), Options.parse(List("--debug")) == Right(Options(debug = true)))
    },
    test("an unknown argument (the old --log-file too) is an error with the usage") {
      assertTrue(
        Options.parse(List("--verbose")) == Left(s"unknown argument: --verbose\n${Options.usage}"),
        Options.parse(List("--debug", "--log-file", "/tmp/x.log")) == Left(s"unknown argument: --log-file\n${Options.usage}"),
      )
    },
    test("a --debug log is named for the process and its start, in the build's target/logs") {
      val file = Logging.debugFile("soness")
      assertTrue(
        sys.env.contains("SONESS_LOG_FILE") || file.getParent.equals(Logging.logsDir),
        Logging.logsDir.endsWith(java.nio.file.Paths.get("target", "logs")),
        sys.env.contains("SONESS_LOG_FILE") || file.getFileName.toString.matches("""soness-\d{4}-\d\d-\d\dT\d\d-\d\d-\d\d\.log"""),
      )
    },
  )
