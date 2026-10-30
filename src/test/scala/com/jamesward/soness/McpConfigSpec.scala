package com.jamesward.soness

import zio.*
import zio.http.Client
import zio.test.*

import java.nio.file.{Files, Path}

/** The project's `.mcp.json`: read as coding agents read it, combined with the client's servers, and launched. */
object McpConfigSpec extends ZIOSpecDefault:
  private val dir = Path.of("/work/project")
  private val env = Map("TOKEN" -> "t0k", "EMPTY" -> "").get

  def spec = suite("project MCP servers (.mcp.json)")(
    test("stdio and http servers, project-relative commands, ${VAR} and ${VAR:-default}, the rest skipped with reasons") {
      val text =
        """{"mcpServers": {
          |  "sbta":  {"type": "stdio", "command": "./sbta", "args": ["--mcp"], "timeout": 1800000},
          |  "tool":  {"command": "npx", "args": ["-y", "server"], "env": {"A": "${TOKEN}", "B": "${MISSING:-dflt}", "C": "${EMPTY:-e}"}},
          |  "docs":  {"type": "http", "url": "https://example.com/mcp", "headers": {"Authorization": "Bearer ${TOKEN}"}},
          |  "bare":  {"url": "https://example.com/other"},
          |  "old":   {"type": "sse", "url": "https://example.com/sse"},
          |  "off":   {"command": "x", "disabled": true},
          |  "bad":   {"type": "stdio"}
          |}}""".stripMargin
      val Right((servers, skipped)) = McpConfig.parse(text, dir, env): @unchecked
      assertTrue(
        servers == List(
          McpEndpoint.Stdio("sbta", "/work/project/sbta", List("--mcp"), Map.empty, Some(dir)),
          McpEndpoint.Stdio("tool", "npx", List("-y", "server"), Map("A" -> "t0k", "B" -> "dflt", "C" -> "e"), Some(dir)),
          McpEndpoint.Http("docs", "https://example.com/mcp", Map("Authorization" -> "Bearer t0k")),
          McpEndpoint.Http("bare", "https://example.com/other"),
        ),
        skipped.size == 2, skipped.exists(_.startsWith("old: SSE")), skipped.exists(_.startsWith("bad: a stdio server needs")),
      )
    },
    test("no file is no servers; a malformed one is a reason, not a failed session") {
      for
        empty <- ZIO.attemptBlocking(Files.createTempDirectory("mcp"))
        none  <- McpConfig.load(empty, env)
        _     <- ZIO.attemptBlocking(Files.writeString(empty.resolve(".mcp.json"), "{not json"))
        bad   <- McpConfig.load(empty, env)
      yield assertTrue(none == (Nil, Nil), bad._1.isEmpty, bad._2.size == 1)
    },
    test("the client's server wins over the project's of the same name") {
      val client = List(McpEndpoint.Http("sbta", "http://client"))
      val project = List(McpEndpoint.Stdio("sbta", "./sbta"), McpEndpoint.Stdio("other", "x"))
      assertTrue(McpConfig.combine(client, project).map(_.name) == List("sbta", "other"),
        McpConfig.combine(client, project).head == client.head)
    },
    // soness building soness: this project's own .mcp.json launches sbta, whose tools a session gets
    test("this project's .mcp.json: sbta is launched and its build tools are listed") {
      val root = Path.of(sys.props("user.dir"))
      for
        loaded    <- McpConfig.load(root, sys.env.get)
        connected <- McpToolset.connect(loaded._1.filter(_.name == "sbta"))
        names      = connected.toolset.tools.map(_.name).toSet
      yield assertTrue(loaded._1.exists(_.name == "sbta"), connected.failed.isEmpty, Set("compile", "test", "check").subsetOf(names))
    } @@ TestAspect.withLiveClock @@ TestAspect.timeout(3.minutes) @@ TestAspect.ifEnvSet("SONESS_SBTA_SPEC"),
  ).provideSome[Scope](Client.default)
