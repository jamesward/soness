package com.jamesward.soness

import zio.*
import zio.test.*

import java.nio.file.{Files, Path}

/** Skill discovery in a session's roots, and progressive loading of a skill's linked references. */
object SkillsSpec extends ZIOSpecDefault:

  /** Writes `files` (relative path to text) under a fresh temporary directory, deleted with the scope. */
  def tree(files: (String, String)*): ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("skills"))): root =>
      ZIO.attemptBlocking(Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete(_))).ignore
    .tap: root =>
      ZIO.foreachDiscard(files): (path, text) =>
        ZIO.attemptBlocking:
          val file = root.resolve(path)
          Files.createDirectories(file.getParent)
          Files.writeString(file, text)

  /** A router skill like IntelliJ IDEA's, with a reference index and one page per routed tool. */
  val routerSkill: Seq[(String, String)] = Seq(
    ".agents/skills/project-info/SKILL.md" ->
      """---
        |name: project-info
        |description: Describe the project open in the IDE. Use when asked about the project.
        |allowed-tools: execute_tool
        |---
        |
        |# Project info
        |
        |Call tools through the router: `execute_tool(command="<tool> --param value")`.
        |See [reference/tools.md](reference/tools.md) for the tools. Ignore [the web](https://example.com) and
        |[a secret](../../secret.md).
        |""".stripMargin,
    ".agents/skills/project-info/reference/tools.md" ->
      """# Tools
        |
        |- [get_project_info](tools/get_project_info.md) — Describes the project: its name and directory.
        |""".stripMargin,
    ".agents/skills/project-info/reference/tools/get_project_info.md" ->
      """# get_project_info
        |Takes no parameters: `execute_tool(command="get_project_info")`.
        |""".stripMargin,
    "secret.md" -> "not part of any skill",
  )

  def spec = suite("Skills")(
    test("a directory without a SKILL.md (Claude's synced bucket) isn't a skill, and isn't an error") {
      for
        root   <- tree(routerSkill*)
        _      <- ZIO.attemptBlocking(Files.createDirectories(root.resolve(".claude/skills/synced/bucket")))
        found  <- Skills.discover(List(root))
        failed <- ZTestLogger.logOutput.map(_.exists(_.cause.prettyPrint.contains("NoSuchFileException")))
      yield assertTrue(found.map(_.title) == List("project-info"), !failed)
    },
    test("finds the skills under each root, described by their front matter") {
      for
        one    <- tree(routerSkill*)
        two    <- tree(".claude/skills/other/SKILL.md" -> "No front matter here.", "skills/not-a-skill/SKILL.md" -> "ignored")
        skills <- Skills.discover(List(one, two, one))
      yield assertTrue(
        skills.map(_.title) == List("project-info", "other"),
        skills.head.description == "Describe the project open in the IDE. Use when asked about the project.",
        skills.head.metadata == Map("allowed-tools" -> "execute_tool"),
        skills(1).description == "The other skill",
      )
    },
    test("reading a skill gives its text and the files it links to inside the skill, which load in turn") {
      for
        root    <- tree(routerSkill*)
        skill   <- Skills.discover(List(root)).map(_.head)
        top     <- skill.read
        index   <- top.linked.head.read
        page    <- index.linked.head.read
      yield assertTrue(
        top.text.contains("execute_tool(command="),
        top.linked.map(_.id) == List("skill:project-info/reference/tools.md"),
        index.linked.map(d => (d.id, d.title, d.description)) == List((
          "skill:project-info/reference/tools/get_project_info.md",
          "project-info: get_project_info",
          "[get_project_info](tools/get_project_info.md) — Describes the project: its name and directory.",
        )),
        page.text.contains("Takes no parameters"),
        page.linked.isEmpty,
      )
    },
    test("the skills the project's instructions name are required, by whole name; the others aren't") {
      for
        root   <- tree(
                    ".agents/skills/zen-of-scala/SKILL.md" -> "---\nname: zen-of-scala\ndescription: Scala idioms\n---\nUse ZIO.",
                    ".agents/skills/zen/SKILL.md" -> "---\nname: zen\ndescription: Zen\n---\nCalm.",
                    ".claude/skills/other/SKILL.md" -> "---\nname: other\ndescription: Other\n---\nx")
        skills <- Skills.discover(List(root))
        marked  = Skills.required(skills, "## Skills\n\n- `zen-of-scala` — Scala 3 / ZIO idioms for all code.")
      yield assertTrue(
        skills.map(_.title).toSet == Set("zen-of-scala", "zen", "other"),
        marked.filter(Skills.isRequired).map(_.title) == List("zen-of-scala"),
      )
    },
    test("a project's skill wins over the user's of the same name; .kiro/skills isn't a standard location") {
      for
        project <- tree(".agents/skills/a/SKILL.md" -> "---\nname: a\ndescription: project's\n---\n",
                     ".kiro/skills/k/SKILL.md" -> "---\nname: k\ndescription: kiro's\n---\n")
        home    <- tree(".agents/skills/a/SKILL.md" -> "---\nname: a\ndescription: user's\n---\n")
        skills  <- Skills.discover(List(project, home))
      yield assertTrue(skills.map(s => s.title -> s.description) == List("a" -> "project's"))
    },
    test("no roots, or roots without skills, give no skills") {
      for
        empty  <- tree("README.md" -> "hi")
        skills <- Skills.discover(List(empty, empty.resolve("missing")))
      yield assertTrue(skills.isEmpty)
    },
  )
