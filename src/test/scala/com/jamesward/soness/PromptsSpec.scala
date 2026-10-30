package com.jamesward.soness

import zio.test.*

/** What Jev is told about failing checks: a failure in what the steps changed is the next thing to fix; one already
  * there before the change, in files the steps didn't touch, doesn't make the request unable. */
object PromptsSpec extends ZIOSpecDefault:

  def spec = suite("prompts")(
    test("Jev fixes failures in what the steps changed, not every failing build or test run") {
      assertTrue(
        Prompts.Jev.contains("A build or test run that fails in what the steps changed is the next thing to fix"),
        !Prompts.Jev.contains("A build or test run that fails is the next thing to fix"),
      )
    },
    test("Jev doesn't end as unable for a check that fails only in files the steps didn't change") {
      assertTrue(
        Prompts.Jev.contains("A check that fails only in files the steps didn't change"),
        Prompts.Jev.contains("don't end as unable for them."),
      )
    },
    test("Jev judges a failing check by the errors in the files the steps changed, not by the total count") {
      assertTrue(
        Prompts.Jev.contains("A failing build or check reports every error it finds, not only the change's"),
        Prompts.Jev.contains("each error's file with the files the steps changed"),
        Prompts.Jev.contains("never by the total count"),
      )
    },
  )
