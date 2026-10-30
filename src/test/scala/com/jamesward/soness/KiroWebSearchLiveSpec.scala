package com.jamesward.soness

import com.jamesward.zioacp.AgentCommand
import zio.*
import zio.test.*

/**
 * Live: Kiro's web search through a real `kiro-cli acp`, as soness offers it when Kiro is the writer: one query through
 * [[AcpWriter.search]] and [[WebSearch.kiro]], answered from the web with at least one cited source. Skipped unless
 * `KIRO_API_KEY` is set (sbt reads it from `.env`).
 */
object KiroWebSearchLiveSpec extends ZIOSpecDefault:

  private val Query = "What is the latest released version of the Scala 3 compiler? Cite the page you found it on."

  def spec = suite("Kiro web search, live")(
    test("a query is answered from the web, citing at least one source") {
      ZIO.scoped:
        for
          scope  <- ZIO.scope
          // a search only prompts the agent with the query: none of the writer's prompts are used
          writer <- AcpWriter.make(Soness.KiroAgent, AgentCommand("kiro-cli", args = List("acp")), scope, null.asInstanceOf[WritingPrompts])
          result <- WebSearch.kiro(writer.search).search(Query, ContextSize.Medium)
          _      <- ZIO.logInfo(s"answer: ${result.answer}\ncitations: ${result.citations.map(_.url).mkString(", ")}")
        yield assertTrue(
          result.answer.trim.nonEmpty,
          result.citations.forall(c => c.url.startsWith("http://") || c.url.startsWith("https://")),
        )
    } @@ TestAspect.timeout(3.minutes)
  ) @@ TestAspect.ifEnvSet("KIRO_API_KEY") @@ TestAspect.withLiveClock @@ TestAspect.sequential
