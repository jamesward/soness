# AGENTS.md — jevness

`jevness` is an agent harness served only as an ACP agent over stdio (`com.jamesward.jevness.Main`), staged with
sbt-native-packager. Jev (zio-typesafe-ai's `GenericOrchestrator`) orchestrates the session's MCP tools and calls a
Bedrock LLM (zio-bedrock) as its generative tools; a pure-LLM Bedrock tool loop is the `llm` baseline mode. See
`README.md` for configuration and the protocol surface.

Follow the `zen-of-projects` Skill (extract with `./sbt extractSkillsJars`); this file records only project-specific
facts and exceptions.

## Skills

- `zen-of-scala` — Scala 3 / ZIO idioms for all code and review.
- `zen-of-james` — design, domain modeling, effects, and testing principles.
- `zen-of-projects` — project/build conventions.

## Exceptions to zen-of-projects

- No sbt-reload: the server speaks ACP over stdio, not HTTP, so there is nothing to reload in place.
- `com.jamesward:zio-acp_3:0.0.1` was tagged but not yet on Maven Central when this project was created. Until it is,
  build zio-acp `v0.0.1` with `./sbt 'set version := "0.0.1"; publishLocal'` in a zio-acp checkout; CI fails to resolve
  it until then.
- `src/test/scala/com/jamesward/zio_typesafe_ai/FakeJev.scala` is in zio-typesafe-ai's package because a
  `TypeSafeAI.Client` is implemented against that library's package-private wire types.

## Build, Test & Dev Workflow

- Use the MCP server named `sbt-mcp-jevness` (`http://127.0.0.1:5171/`) for sbt interactions: `sbt-task`, `check`,
  and the symbol and javadocs.dev tools. If it is unavailable, say so and fall back to the `./sbt` launcher.
  - Kiro: HTTP entry in `.kiro/settings/mcp.json`; start sbt first.
  - Claude Code: `.mcp.json` runs `.claude/sbt-mcp-stdio.sh` (approved in `.claude/settings.json`). Its tools are
    deferred: load them with ToolSearch (search `sbt-mcp-jevness`). Diagnostics: `/tmp/sbt-mcp-stdio.log`,
    `/tmp/sbt-mcp-server.log`.
- Commands (CLI fallback form):
  - `./sbt "Test / compile; testFull; stage"` — full validation (what CI runs).
  - `./sbt "testOnly *HarnessSpec"` — a single spec.
- Smoke test after `stage`: pipe an ACP `initialize` request into
  `target/out/jvm/scala-3.10.0/jevness/universal/stage/bin/jevness` with `AWS_BEARER_TOKEN_BEDROCK=x BEDROCK_MODEL_ID=x
  JEVNESS_MODE=llm` (configuration is only read, not checked, at startup).
- The default suite needs no network beyond dependency resolution: the MCP fixture, Bedrock (its OpenAI-compatible
  Mantle API) and Jev are local fakes.

## Paid evals

- `JevnessEvalSpec`'s live suite calls Jev and Bedrock. It runs only when `TYPESAFE_API_KEY`,
  `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` are set; `JEVNESS_EVAL_SAMPLES` (default 3) sets samples per mode.
  Never run it in CI or the maintenance routine. Run `./sbt shutdown` after changing these variables.
