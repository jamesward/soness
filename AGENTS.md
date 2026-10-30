# AGENTS.md — soness

`soness` (System One Harness) is an agent harness served only as an ACP agent over stdio (`com.jamesward.soness.Main`), staged with
sbt-native-packager. Jev (zio-decisions' `AgentLoop`) orchestrates the session's MCP tools, choosing one tool or an
ending at each step, and a Bedrock LLM (zio-bedrock) writes the chosen tool's arguments and the reply; Bedrock is never given the session's tools to call (structured output or text only; on Mantle structured output is a single forced function call, see README). The decision
model is pluggable (zio-decisions' `DecisionModel`; soness's `DecisionModels` picks the provider from
`SONESS_DECISION_MODEL`: `jev`, zio-decisions' `typesafe.TypeSafe`, or `openai`, its `openai.OpenAI`): orchestration
code asks `DecisionModel`, never a provider directly. A new provider is a `DecisionModels.Provider` that builds a `DecisionModel`. See
`README.md` for configuration and the protocol surface.

Follow the `zen-of-projects` Skill (extract with `./sbta extractSkillsJars` into `.agents/skills/`); this file records only project-specific
facts and exceptions.

## Skills

- `zen-of-scala` — Scala 3 / ZIO idioms for all code and review.
- `zen-of-james` — design, domain modeling, effects, and testing principles.
- `zen-of-projects` — project/build conventions.

## Exceptions to zen-of-projects

- sbt-reload only for the log explorer (test code, `~Test/runReload`): soness itself speaks ACP over stdio, not HTTP,
  so there is nothing to reload in place.
- zio-evals (tests only) is a source dependency on `../zio-evals` too, for its ACP `AgentLoop`; `.sbtopts` sets
  `-Dlocal` so zio-evals uses the sibling zio-decisions rather than a published one.
- zio-acp, zio-bedrock, zio-http-mcp and zio-decisions are source dependencies on sibling checkouts
  (`RootProject(file("../…"))` in `build.sbt`) while their changes are unreleased: in zio-acp, `"type":"stdio"` MCP
  servers, the subprocess transport's shutdown and debug logging of every message; in zio-bedrock, no tool
  configuration for a request without tools, and structured output against a runtime JSON Schema (on Mantle as a forced function call)
  (`Request.asJsonObject`); in zio-http-mcp, the stdio client transport (`McpClient.connectStdio`); in
  zio-decisions, the whole library (unpublished), with `orchestration.AgentLoop`. Replace them with the released artifacts once published; CI can't
  build this project until then.
- After a `build.sbt` change, the zio-acp classes under `target/out/jvm/scala-3.10.0/zio-acp` have been left
  incomplete, which shows as `Not found: AcpAgent`; delete that directory and compile again.
- `src/test/scala/com/jamesward/zio_decisions/typesafe/FakeJev.scala` (and `JevDecisionEvalSpec`) are in
  zio-decisions' `typesafe` package because `FakeJev` is a `TypeSafe.Transport`, which with its wire types is
  package-private there.

## Build, Test & Dev Workflow

- Use the MCP server `sbta` (this build's `./sbta --mcp`) for all sbt work: build, test and run
  with `compile`, `test` (`tests` for specific classes, `full` for all), `run`, `clean` and
  `publish-local`; inspect the build with `projects`, `test-names`, `main-classes`, `dependencies` and
  `show`. Use `sbt-task` for any other command (e.g. `reload`, `stage`) and `list-tasks` to find tasks. Don't run
  `sbt` or `./sbta` in a shell while the MCP tools are available; if the server is unavailable, say so and
  fall back to `./sbta <command>`.
- After editing Scala 3 sources, check them with `check` (pass the edited files: a fast type check
  without compiling; `"scope":"module"` after changing an API), then run `compile` and `test` before
  declaring the work done.
- For Scala symbols use `glob-search` (find or list), `inspect` (members and signatures) and
  `symbol-location` (source location) rather than text search or guessing APIs. JavaDoc/ScalaDoc
  lookups are the proxied javadocs.dev tools of the same server.
- The `sbta` server is registered for Kiro in `.kiro/settings/mcp.json` and in `.mcp.json` (Claude Code, and soness
  itself, which loads a project's `.mcp.json`: soness builds soness through it); `./sbta --mcp --setup <client>` adds
  it for others; it needs macOS or Linux. The older `sbt-mcp-soness` server stays registered alongside it:
  every client reaches that one through the stdio launcher `.sbt-mcp/SbtMcpStdio.java` (Kiro:
  `.kiro/settings/mcp.json`; Claude Code: `.mcp.json`, approved in `.claude/settings.json`, tools deferred: load them
  with ToolSearch, search `sbt-mcp-soness`). It starts sbt (`sbt --client`) when it isn't running. Re-run
  `mcpInstallStdio` after upgrading sbt-mcp and commit `.sbt-mcp/`. Diagnostics: `project/target/sbt-mcp/stdio.log`
  and `project/target/sbt-mcp/boot.log`.
- Commands (CLI fallback form):
  - `./sbta "Test / compile; testFull; stage"` — full validation (what CI runs).
  - `./sbta "testOnly *HarnessSpec"` — a single spec.
- Smoke test after `stage`: `IntelliJReplaySpec` is the end-to-end check (it launches `Main` against fakes). By hand,
  startup checks each Bedrock model with a chat, so the staged script
  (`target/out/jvm/scala-3.10.0/soness/universal/stage/bin/soness`) needs real Bedrock credentials, or
  `SONESS_BEDROCK_API=mantle SONESS_BEDROCK_ENDPOINT=<an OpenAI-compatible fake>`, and a `TYPESAFE_API_KEY` (Jev
  isn't called at startup). Without a reachable model it exits with "no Bedrock model is available".
- To see what a session does, run soness with `--debug` (in IntelliJ, `args` in `~/.jetbrains/acp.json`): every ACP
  message, tool call and Bedrock/Jev exchange, in full, in `target/logs/soness-<date-time>.log` (`soness-dev-…` for
  the development build; `SONESS_LOG_FILE` names the file instead, as the evals' `SONESS_EVAL_LOG` does).
- To explore a debug log in a browser (each turn's time, tokens and calls; every record with what was sent where, its
  request and response, how long it took and its tokens): `./sbta inspector` (`~Test/runReload`, sbt-reload: restarted on
  every change to its Scala code; its JS and CSS are served from `src/test/resources/log-explorer/`, so an edit shows on
  a refresh; `set Test / runReloadArgs += "<file-or-directory>"` for other logs than
  `target/logs`' and `target/evals/logs`'), then http://127.0.0.1:8077. From an agent while it runs:
  `Test/reloadStatus`, `Test/reloadOutput`, `Test/reloadPause` / `Test/reloadResume` around several edits. It's test
  code (`src/test/scala/com/jamesward/soness/inspector/`, its page's JS and CSS in `src/test/resources/log-explorer/`),
  zio-http with template2 and Tailwind's browser build from a WebJar (sbt-webjars). It listens on loopback only, with
  no authentication, and serves only the `.log` files under the paths it's given. `LogExplorerSpec` checks it.
- The default suite needs no network beyond dependency resolution: the MCP fixture, Bedrock (its OpenAI-compatible
  Mantle API) and Jev are local fakes.

## IntelliJ IDEA's MCP server, headless

The real IDE MCP server runs without the GUI, on its own config and system directories so it doesn't clash with a
running IDE (`idea.properties` with `idea.config.path`, `idea.system.path`, `idea.log.path` under a scratch directory,
and `idea.plugins.path` pointing at the installed plugins):

```bash
IDEA_PROPERTIES=/tmp/ijh/idea.properties idea mcpServer --project=<dir> --port=64562 --invocation-mode=router --allowed-tools='*'
```

It takes 30–60 s to start (project import, indexing) and prints "MCP Server started on port …". Start it on a project
without `.idea`, `.bsp`, `project/` or `target/` left from an earlier run, and from a fresh config directory: otherwise
it stops with "Missing value for the environment key 'project.open.type'". With `--invocation-mode=router` it serves
as IntelliJ serves ACP agents (only `execute_tool`; `*` routes the GUI's 58 tools and 9 plugin-development ones); with
`--invocation-mode=direct` its `tools/list` has every tool's description and schemas. Clients reach it the way ACP
agents do, through `idea stdioMcpServer` with `IJ_MCP_SERVER_PORT` and `IJ_MCP_SERVER_PROJECT_PATH`.

- `src/main/resources/intellij-mcp-tools.json` is the direct-mode `tools/list` (without `execute_tool`) wrapped as
  `{"source": …, "tools": [{name, description, inputSchema, outputSchema}]}`. Regenerate it for a new IntelliJ
  version from a direct-mode server. It holds JetBrains' tool descriptions: check their terms before publishing.
- `IntellijTaskEvalSpec` needs its project open in the IDE: write it with
  `./sbta "Test/runMain com.jamesward.soness.WriteProject <dir>"` and start the server on a copy of it.
- `IntellijEvalSpec` and `IntellijTaskEvalSpec` use the real IDE when `SONESS_IJ_MCP_PORT`, `SONESS_IJ_PROJECT` (the project the server
  opened: a copy of `IntellijEvalSpec.project`) and `SONESS_IJ_IDEA` (the `idea` launcher) are set;
  `SONESS_IJ_SKILL_ROOT` replaces the skill root. With the same variables `IntellijRouterSpec` checks the catalog
  against the router's tools, the rendered commands against the real router, and the mock's output against the real
  one's. Run `./sbta shutdown` after setting them.

## Paid tests

- `LiveE2ESpec` is the live end-to-end check: soness as a subprocess, IntelliJ's captured messages, real Bedrock and
  Jev, every model the agent offers (add models with `SONESS_BEDROCK_MODELS`). It checks the protocol and the answers,
  not speed. It runs when `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` are set, costs a few
  calls per model, and falls under the same rules as the evals below.
- `SonessEvalSpec`'s live suite calls Jev and Bedrock. It runs only when `TYPESAFE_API_KEY`,
  `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` are set; `SONESS_EVAL_SAMPLES` (default 3) sets samples per model.
  Agents may run it (including as part of `testFull`) whenever those variables are set in sbt's environment; no extra
  confirmation is needed. Keep it out of CI. Run `./sbta shutdown` after changing these variables.
- `IntellijTaskEvalSpec` is the measure for orchestration changes: run it before and after with
  `SONESS_EVAL_LABEL` set and `SONESS_EVAL_SAMPLES` of 3 or more, and compare the records in
  `target/evals/intellij-tasks.jsonl` (pass rate, Jev and LLM calls and tokens, latency and its split).
- Strange LLM behavior met in an eval (prose instead of a call, malformed or repeated arguments, runaway output) gets a
  reproducer: capture the exchange from the run's debug log with `scripts/capture-llm-behavior.py <log> <name>
  <description> <checks> [--tool T] [--match TEXT] [--no-call] [--observed TEXT]` into
  `src/test/resources/llm-behaviors/`, add a check to `LlmBehaviorSpec` if none fits, and iterate on the prompt or
  protocol against `testOnly *LlmBehaviorSpec` (seconds, not an eval run; `SONESS_LLM_BEHAVIORS`,
  `SONESS_LLM_BEHAVIOR_SAMPLES`, `SONESS_LLM_BEHAVIOR_MODEL`). It's paid and fails while a behavior persists.
- Fast iteration, cheapest first: offline specs (seconds; `AcpProtocolSpec`, `SessionsSpec`, `LocalToolsSpec`,
  zio-decisions' `AgentLoopSpec` with a mocked Jev); `JevDecisionEvalSpec` and `LlmBehaviorSpec` (real models on
  captured requests, seconds); `IntellijParallelEvalSpec` with `SONESS_PARALLEL_TASKS=katas,easyracer`,
  `SONESS_EASYRACER_SCENARIOS=1,2,4`, `SONESS_EVAL_TURN_MINUTES=5` and a debug log (`SONESS_EVAL_LOG`), about 15
  minutes for 3 samples; the 11-scenario run only to confirm. After each run, `scripts/diagnose-run.py <log>` (tool
  failures, repeated calls, course corrections, rewritten replies) says where it went wrong, and
  `scripts/replay-arguments.py <log> <efforts>` replays its argument writing (reasoning effort, latency, tokens).
- Self-improvement is the development build's, not production's: `Test / stage` stages
  `target/out/jvm/scala-3.10.0/soness/test-stage/bin/soness-dev`, which is soness with the `SelfImprovement` extension
  (`src/test/scala/com/jamesward/soness/dev/`). Production soness (`stage`) ships none of it; the agent offers
  extension points (`Extension`: per-session processes, per-task tools, commands, background tasks) that it uses.
  On a clone of this repository (a remote naming jamesward/soness or jamesward/jevness), the development build:
  - runs the observer, a process of each session's tree that reads the session's own turns as every task publishes
    them (the session's shared state; no log needed), and records findings of what soness could do better in
    `.soness/findings.jsonl`. It only reads and says nothing in the chat. `SONESS_OBSERVER=false` turns it off;
  - improves itself from them, unasked: each finding starts a background task of the session (one at a time per
    project, `target/improve/.lock`) whose rounds edit this checkout's files directly (and its source dependencies'
    when the cause is theirs), with the session's own tools and the process's debug log as evidence; `/status` shows
    its tree, `/stop` ends it, messages steer it. It skips the eval gate unless `SONESS_SELF_IMPROVE_EVAL=true`, runs
    `SONESS_SELF_IMPROVE_ROUNDS` rounds (default 1); `SONESS_SELF_IMPROVE=false` turns it off. `self_improve` starts a
    run on request (all the findings). A round's change is judged by three gates (`Improve.Local`); a rejected round's
    edits stay in the files, its verdict saying why:
    1. it changes code, not only comments, and adds a test;
    2. the offline `testFull` passes (through this checkout's own sbt server);
    3. the quick eval doesn't regress against the baseline (unless `eval` is false).
    Each round's starting files, patch, outputs and verdict are in `target/improve/`.
- A goal, then soness improving itself from how it went (`com.jamesward.soness.dev.GoalRun`, no editor):
  `./sbta Test/stage`, then `target/out/jvm/scala-3.10.0/soness/test-stage/bin/soness-goal [--project <dir>]
  [--rounds <n>] [--no-improve] [--eval] [--allow] <goal…>`. soness runs in that process (its ACP agent driven in
  memory: soness's own file tools and the project's `.mcp.json` servers; permission requests refused unless
  `--allow`) and works on the goal in the project (default: the current directory). The goal's turn is read back
  from the run's debug log (`target/logs/soness-goal-…`) with its telemetry (wall, decision-model and LLM requests,
  time and tokens, tool time, how much ran at once, failures, repeats), and the decision model judges whether the
  goal was done. Then each round prompts soness on this project (and its source dependencies) to make the one change
  that best serves, in order: the goal accomplished, alignment with DESIGN.md, token efficiency, time efficiency;
  `Improve.Local`'s gates judge it as they do self-improvement's. Run it outside sbt (`Test/runMain` would hold the
  sbt server the gates and the round's build use). Its record (goal, telemetry, transcript, each round's verdict) is
  in `target/goal-runs/<stamp>/`; the changes take effect once soness is staged again. `GoalRunSpec` checks it.
- A live failure is a real result: report the verdicts and each model's latency and token numbers rather than treating
  it as noise or a reason to skip validation.
