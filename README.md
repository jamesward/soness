# soness

*soness*: System One Harness. An agent harness orchestrated by a "System 1" decision model (see [Decision model](#decision-model)).

An agent harness whose only interface is the [Agent Client Protocol](https://agentclientprotocol.com) (ACP). An editor
or any other ACP client launches it over stdio, gives each session its MCP servers, and sends prompts.

Inside, [Jev](https://docs.typesafe.ai/introduction) is the orchestrator of an agent loop (zio-decisions'
`AgentLoop`). At every step Jev answers one choice: call one of the session's tools, each offered with its name,
description, input and output schemas, annotations and how its earlier calls went; read one of the session's skills
or a reference it links to; or end the loop, with an answer or as unable to complete the request. Skills load
progressively: reading one offers the files it links to, so Jev can learn how to use a tool (an MCP server that routes
its tools through one command, say) before calling it. Jev sees the request and every step taken (results cut to 4000 characters). A
generative LLM on Amazon Bedrock does the writing: a chosen tool's arguments (structured output against the tool's
input schema) and the reply. Bedrock is never given the session's tools to call: on Mantle, its structured output
is one forced call of a function named for the chosen tool, taking that tool's input schema, whose arguments are
the output and which nothing runs (Mantle's `response_format` is only a hint to models such as gpt-oss). After 12 tool calls only the endings are offered, so a turn
always ends with a reply.

Built with:

- [zio-acp](https://github.com/jamesward/zio-acp): the ACP agent side and stdio transport;
- [zio-decisions](https://github.com/jamesward/zio-decisions): the decision models (Jev, OpenAI's Decisions API) and the agent loop (`AgentLoop`);
- [zio-bedrock](https://github.com/jamesward/zio-bedrock): the Bedrock LLM (Converse or Mantle), for Jev's generative tools;
- [zio-http-mcp](https://github.com/jamesward/zio-http-mcp): the MCP client (stdio and Streamable HTTP) for the session's tools;
- [zio-evals](https://github.com/jamesward/zio-evals): the eval runner and Jev judge.

```
ACP client ──stdio──▶ SonessAgent ──session model──▶ DecisionHarness (AgentLoop)
                         │                              │ Jev chooses: call tool A | B | … | end (answer) | end (unable)
                         │                              │ Bedrock (no tools): writes the arguments, and the reply
                         └── MCP servers from session/new (stdio, HTTP) ◀──┘
```

## Run

```bash
./sbta stage
target/out/jvm/scala-3.10.0/soness/universal/stage/bin/soness
```

Configure an ACP client to launch that script. For example, in [Zed](https://zed.dev/docs/ai/external-agents):

```json
{
  "agent_servers": {
    "soness": {
      "command": "/path/to/soness/target/out/jvm/scala-3.10.0/soness/universal/stage/bin/soness",
      "env": { "TYPESAFE_API_KEY": "…", "AWS_BEARER_TOKEN_BEDROCK": "…", "BEDROCK_MODEL_ID": "…" }
    }
  }
}
```

Argument: `--debug` logs at debug level, and to a file of its own (see [Logging](#logging)).

| Variable | Required | Default | |
| --- | --- | --- | --- |
| `SONESS_DECISION_MODEL` | no | the first provider whose key is set (`TYPESAFE_API_KEY`, then `OPENAI_API_KEY`), else `jev` | the decision model that orchestrates (see [Decision model](#decision-model)): `<provider>` or `<provider>:<model>`, e.g. `jev:jev-latest`: `jev` (Jev; `TYPESAFE_API_KEY`) or `openai` (OpenAI's Decisions API; `OPENAI_API_KEY`, default model `gpt-6-luna`, `SONESS_OPENAI_ENDPOINT` replaces its origin) |
| `TYPESAFE_API_KEY` | with `jev` | | Jev API key |
| `TYPESAFE_DEFAULT_MODEL` | no | `jev-latest` | Jev's model, when `SONESS_DECISION_MODEL` names none |
| `AWS_BEARER_TOKEN_BEDROCK` | unless an ACP agent writes | | [Bedrock API key](https://docs.aws.amazon.com/bedrock/latest/userguide/api-keys.html) |
| `BEDROCK_MODEL_ID` | one of these two | | the default Bedrock model, for example `us.anthropic.claude-sonnet-4-5-20250929-v1:0` (Converse) or `openai.gpt-oss-120b` (Mantle) |
| `SONESS_BEDROCK_MODELS` | one of these two | | more Bedrock models to offer in the session's model picker, comma-separated |
| `AWS_REGION` | no | `us-east-1` | Bedrock region |
| `SONESS_BEDROCK_API` | no | `converse` | `converse` or `mantle` |
| `SONESS_BEDROCK_ENDPOINT` | no | | an origin that replaces Bedrock's regional one (a proxy, or the tests' fake) |
| `SONESS_TYPESAFE_ENDPOINT` | no | | an origin that replaces the TypeSafe AI API's (a proxy, or the tests' fake) |
| `SONESS_EXPLORE_LIMIT` | no | `6` | reads in a row (files, searches, pages) after which the decision model is asked each step whether it's read enough to start the change; once it says so, at twice the limit, or (for a request that asks for a change) with 3 steps left, reading tools are left out until a step does more than read; `0` turns it off |
| `SONESS_REVIEW_EVERY` | no | `12` | calls after which the decision model is asked whether they moved the request forward (found something, changed something, a check's result changed); judged not, the loop ends with what it found and what's in the way, rather than running on; `0` turns it off |
| `SONESS_BRIEF_ARGUMENTS` | no | `true` | before a call's arguments are written, the decision model picks, one yes-or-no question each in one request, which earlier steps, documents read and the project context the writer needs; the writer gets the request, the tool and only those (and the tool's own failed calls), and may set `need_more_information` instead of guessing, when it's written again with everything; `false` gives it everything |
| `SONESS_LOOP_STRATEGY` | no | `boundCalls,noRepeats,correct,batch` | the agent loop's strategy (zio-decisions' `AgentLoop.Strategy`): any of `boundCalls`, `noRepeats`, `correct`, `batch` (independent calls of any tools, reads, fetches, edits of different files, written at once and run at the same time), `giveUp`, `relevanceFilter`, `catalogInState`, `parallelCalls`, `keepContext` or `rateContext`, or `all` / `none`; for measuring orchestration with the evals |
| `SONESS_MAX_PARALLEL` | no | `32` | how many things run at once: a batch's calls (and how many it may have) and a plan's parts (zio-decisions' `Work`, which both share) |
| `SONESS_ESCALATION_MODEL` | no | | a Bedrock model stronger than the session's (one of the session models, or any other, checked at startup like them but not offered in the picker): when a loop's work keeps failing, Jev can hand its writing to it |
| `SONESS_ARGUMENTS_EFFORT` | no | `low` | the reasoning effort gpt-oss writes a call's arguments at: `low`, `medium`, `high`, or `default` (the model's own); edits, and rewrites after a misfit, are always written at the model's own |
| `SONESS_PROJECT_MCP` | no | `true` | `false` leaves out the project's `.mcp.json` servers |
| `SONESS_EVENT_LOG` | no | `true` | `false` keeps no event log (`.soness/events.jsonl`): no listing, loading or resuming sessions, and no recall of earlier work |
| `SONESS_GUARD` | no | `jev` | tool call safety (see [Safety](#safety)): `jev`, `rules` (the user decides what the rules don't), or `off`; `SONESS_GUARD_ALLOW` (default `0.8`) and `SONESS_GUARD_DENY` (`0.2`) are Jev's thresholds |
| `SONESS_SHARED_TTL_MINUTES` | no | `60` | how long a result in the project's shared log (`.soness/results/`: web pages, versioned artifacts' docs) answers the same call from any session |
| `SONESS_PEERS` | no | `true` | `false` neither publishes this process's process tree for the user's other soness processes' `/status` nor shows theirs |
| `SONESS_ACP_AGENTS` | no | Kiro, when `kiro-cli` is installed | the ACP agents that can write for Jev (the `acp` mode's models): `name=command args…` separated by `;`, for example `kiro=kiro-cli acp;claude=npx -y @agentclientprotocol/claude-agent-acp`; empty for none |

At startup soness checks every configured model with a one-line chat (a few tokens each, in parallel) and offers
only those that answer; each one that doesn't is logged on stderr with Bedrock's error. Sessions start with
`BEDROCK_MODEL_ID`, or the first available model when it is unset or unavailable. The agent exits when no decision model is
configured or no model is available. Bedrock is optional when an ACP agent can write (Kiro, when `kiro-cli` is on the
`PATH`) and no Bedrock setting is given: with only `OPENAI_API_KEY` and Kiro (signed in, or `KIRO_API_KEY`), OpenAI's
Decisions API decides and Kiro writes and searches.

## Decision model

Every decision is the decision model's: a "System 1" model that answers typed questions (yes or no, a choice among
described options, a score) with probabilities, quickly and cheaply. The agent loop's next step, whether a request has
independent parts, how a message sent mid-task is routed, whether a stalled call is stopped, which earlier work bears on
a request, whether a reply reports unfinished work. The orchestration (zio-decisions' `orchestration` package) asks
them through one interface, `DecisionModel`, never through a model's own client; a provider implements it.
`SONESS_DECISION_MODEL` picks the provider and, optionally, its model: Jev (TypeSafe AI, zio-decisions'
`typesafe.TypeSafe`) or OpenAI's Decisions API (`openai.OpenAI`). A provider reads its settings and key through a credential source, the environment for now, so another source
(a sign-in) can replace it without the providers changing. Throughout this README, "Jev" means the decision model.

## Providers

The decision model decides; a generative model writes (a chosen tool's arguments, the reply, the parallel plan). Who writes is the
session's ACP mode, and the model picker lists that provider's models:

- `bedrock`: a Bedrock model (`BEDROCK_MODEL_ID`, `SONESS_BEDROCK_MODELS`);
- `acp`: an ACP agent (`SONESS_ACP_AGENTS`; Kiro by default when it's installed, writing with `claude-sonnet-5.5`
  unless its command names a `--model`), each writing a prompt in a clean session of its own (made ahead, closed once
  answered), in a scratch directory with no MCP servers, asked for only the JSON or text needed; its permission
  requests are refused, so it uses no tools of its own. The agent's process starts on first use.

## Work

There are no modes: whatever can run at the same time does (a loop's independent calls as a batch, a request's
independent parts, the tasks of a session and of every session in the project). For each request Jev first
answers one yes-or-no question (one cheap request): is the request made of several independent parts of real work
(scenarios, endpoints, functions, separate changes, each with its own code and test)? When not (a question, a bug, one
change), one agent loop works through it, as it always has. When so (zio-decisions' `ParallelLoop.auto`):

1. one loop reads what the request needs (the spec, the files), changing nothing;
2. the Bedrock model plans the parts from that, and Jev confirms they can be done at the same time;
3. one loop drafts every part in one pass and runs the tests once (small parts are often all right first time, and
   one pass costs one test run); Jev judges, part by part, from that run's results, which parts are done; when most
   failed, and Jev sees one shared cause (the same error in code the parts share), one loop fixes that first and the
   parts are judged again;
4. only the parts that aren't get a loop each, all at the same time (each seeing what the others have done so far, and
   the paragraphs of the gathered spec Jev judges it needs, verbatim; a round has at most 5 minutes; one that doesn't
   finish gets a second round at once, when Jev judges from its calls that another is likely to get there);
5. a last loop checks them together and replies.

There's no setting for this: Jev decides. Each tool call update says which loop made it
(`_meta.soness.loop`). `AutoModeEvalSpec` measures Jev's first decision on labelled requests.

The session's tools are shared as a validation service: reads run freely; edits run one at a time; builds, tests and
commands run one at a time, with those waiting merged (identical ones once, Gradle `--tests` and sbt `testOnly`
selections as one run). A caller of a run that also had others' tests gets its result with a note of its own tests'
status (from Gradle's test events) and that the other failures aren't its own. A part whose tests a later run covered
is answered from that run. A run whose callers have all stopped isn't made. A command's timeout is capped at 5
minutes, since the others' runs wait for it. While other loops work, a loop may overwrite only a file it created
itself. `ValidationSimSpec` compares these policies in simulated time.

The service is the project's, not the session's: every session in one project directory (several chats of one ACP
client) shares it, so their edits don't interleave and their builds and tests take turns and merge; and soness
processes of several clients in one project take turns at runs and edits through lock files under
`~/.cache/soness/locks/`. `MultiSessionEvalSpec` runs two chats' changes in one project at once.

The sessions in a project also share its state (`Workspace`):

- a check (a build, a test run, a type check) that another task's edit lands during is stale: it's stopped (an MCP
  call's `notifications/cancelled`, a command's process) and run again, its callers answered by the new run, at most 3
  times; an edit by one of the run's own tasks (a parallel part's) doesn't restart it, and a command that may change
  things (git, a publish) is never stopped;
- a call whose result is the world's, not the project's (a web page; a versioned artifact's docs), is kept in
  `.soness/results/`: the same call from any session, in any soness process, is answered from it (marked so), and one
  made while the same call is in flight waits for it;
- every call is in the project's activity log, by task, with the task's request: each decision and writing sees what
  the other sessions are doing (their requests, their edits, their runs and how they ended, the pages they fetched),
  fitted into the context by priority with the rest; a session's own calls are its steps, so they aren't repeated.
- what other tasks do is pushed to the loops it bears on (zio-decisions' `Notices`), not only found at their next
  look: the project's event log is followed as it grows, by every soness process in the project (the file system
  says when; nothing polls), and another task's edit of a file a loop read or edited, or its check that failed while
  the loop edits or checks, reaches that loop. The decision model judges each one (one yes-or-no question, with what
  the loop is doing and what its work has touched): one that makes the work in flight out of date is marked so at the
  top of the loop's next decision, the rest are noted. This is always on: soness assumes work is concurrent.
  `NoticeJudgeEvalSpec` measures that judgment on labelled cases; `CoordinationEvalSpec` runs two sessions changing
  the same file at once;
- an edit of a file that changed since its loop last read or wrote it (another task's edit) isn't made: it fails at
  once with what changed, so the loop writes it again on top of the other change rather than over it. The same call
  made again then isn't taken for a repeat of a failed one;
- the files earlier tasks read (this session's, a follow-up's "go ahead", or another session's) whose file hasn't
  changed since (same size and last change, no call naming it since) are in the context with their text, so they
  aren't read again; a file that changed is left out and read anew.

## Safety

Every call a task makes is allowed, put to the user, or refused before it's made (`Guard`):

1. Rules, with no model: reads, searches, fetches, edits inside the project or a source dependency, builds, test runs
   and commands that only read (`ls`, `git status`, …) run. Edits outside the project, tools their server marks
   `destructiveHint`, and commands that delete, push, publish, escalate, kill or pipe a download into a shell are put to
   the user. Most calls end here.
2. Jev, for the rest (another command, an MCP tool that says nothing about itself, a fetch whose URL carries a lot of
   data): one yes-or-no question each, with the task's request and its last calls, since whether a call follows from
   what was asked is what catches instructions a page slipped into the work; the calls of one moment (a batch) in one
   request. At least 0.8 runs, below 0.2 is refused, between is put to the user; if Jev can't be asked, the user.
3. The user, through ACP's `session/request_permission` on the call's own tool call: allow or reject, once or always.
   "Always" holds for the rest of the session, for the tool (and a command's first two words, `git commit`).

A refused call is a failed step that says why, so the loop can do otherwise. `GuardEvalSpec` measures Jev's verdicts
on labelled calls.

## Logging

ACP has no logging channel, and stdout carries the protocol, so soness logs to stderr. ACP clients capture it:
IntelliJ IDEA writes it to its `idea.log` (Help | Show Log in Files), one `[air-acp] stderr:` entry per chunk. For a
log of its own, with everything the session sees, pass `--debug`: the log also goes to a file named for the process
and when it started (`soness-2026-10-19T16-30-36.log`, the development build's `soness-dev-…`) in the build's
`target/logs/` (`~/.cache/soness/logs/` for a soness not run from its build), its path logged at startup;
`SONESS_LOG_FILE` names the file instead. In IntelliJ IDEA's `~/.jetbrains/acp.json`:

```json
"soness": {
  "command": "/path/to/soness/target/out/jvm/scala-3.10.0/soness/universal/stage/bin/soness",
  "args": ["--debug"],
  "env": { … }
}
```

Info level (the default) logs startup (the available models, and each unavailable one with the reason), each
session's MCP servers and tools (and each server skipped, with the reason), model changes, failed turns, and Jev's
orchestration steps. Debug level adds,
in full: every ACP message sent and received, each prompt, each tool call with its arguments and its result, each
Bedrock and Jev exchange with its request and response, the answer, the turn's metrics, and the stderr of stdio MCP
servers. A prompt turn's lines
carry `session=<id>`. Debug logs contain prompts, tool data and model output, so keep them where those may be stored.

## Protocol surface

- `session/new` connects to the session's MCP servers: stdio servers (such as the IDE MCP server IntelliJ IDEA passes)
  are launched in the session's `cwd` with their `env`, and Streamable HTTP servers are reached with their headers.
  SSE servers, and servers that fail to start or connect, are skipped with a warning on stderr. Tool names must be
  unique across the servers.
- The project's own MCP servers: `.mcp.json` in `session/new`'s `cwd`, the file coding agents share (Claude Code's
  project scope; `{"mcpServers": {name: {"command", "args", "env"} | {"type": "http", "url", "headers"}}}`), with
  `${VAR}` and `${VAR:-default}` expanded from soness's environment and a relative command (`./sbta`) resolved against
  the project. They're added after the client's: a client's server of the same name wins. SSE and `disabled` servers
  are skipped with a warning. Opening a project runs the commands its `.mcp.json` names, as other agents do once
  approved; `SONESS_PROJECT_MCP=false` leaves them out (for projects you don't trust). This project's own `.mcp.json`
  has [sbta](https://github.com/jamesward/sbta), so soness working on soness builds and tests it through sbta's tools.
- Skills: `session/new`'s `cwd`, its `additionalDirectories` (advertised in `sessionCapabilities`), any
  `_meta.additionalRoots` and then the user's home are searched for [Agent Skills](https://agentskills.io) in the
  standard locations (`.agents/skills/*/SKILL.md`, and `.claude/skills/*/SKILL.md` for compatibility); a project's skill
  wins over the user's of the same name. No skill's text is in a request's context until it's chosen: before the work
  starts, Jev is asked, from each skill's name and description only (one yes-or-no question each, in one request;
  zio-decisions' `SkillChoice`), which the request needs; those (at most 3) are read once and are in the context of
  every decision and every writing. A skill the project's `AGENTS.md` (or an MCP server's instructions) names, such as
  "Follow the `zen-of-scala` skill", is marked so in that question, which makes it likelier for work it bears on but
  not loaded for a request it doesn't (a greeting, a status question). The rest stay offered by name and description:
  reading one during the work loads its file and offers the files it links to inside its directory, each described by
  the line that links it. What Jev reads is given to Bedrock when it writes arguments and the reply. At most 8
  documents are read per turn.
- MCP server instructions: what each server says about using its tools (`instructions` in its `initialize` result) is
  base knowledge, as `AGENTS.md` is: in the project context Jev and the Bedrock model see.
- Session config options, each a drill-down by provider (ACP's grouped select options), switched with
  `session/set_config_option` and taking effect from the session's next turn:
  - `decision_model` ("Decision model"; category `model`, so it's the editors' model picker): Jev or OpenAI's Decisions API;
  - `model` ("LLM provider"; category `_llm_model`): Bedrock's models, or an ACP agent (Kiro);
  - `web_search` ("Web search"; category `_web_search`): Kiro's, Bedrock's, or None.

  Every provider is listed, those without their credentials or tool too: ACP has no disabled option, so one that's
  unavailable is named "… (unavailable)", its description says what it needs (`set OPENAI_API_KEY`, `install Kiro`),
  `_meta.soness.available` is `false`, and choosing it fails saying so. There's no mode selector (`modes`). A session
  without web search is told so in its first turn (an `agent_thought_chunk`: what would give one), and again after its
  search changes to none.
- `session/prompt` answers the prompt's text. The response carries ACP's turn `usage` (Jev's and the LLM's tokens
  together). Each tool call is reported as a `tool_call` update (input) and a
  `tool_call_update` (output or failure), the answer as an `agent_message_chunk`, and the turn's cost in the response's
  `_meta.soness`: `model`, `ending` (`answered`, `unable`, or `budget_exhausted`), `latencyMs`, `toolCalls`, and the Bedrock and Jev usage kept apart (`llmCalls`,
  `llmInputTokens`, `llmOutputTokens`, `jevCalls`, `jevInputTokens`, `jevOutputTokens`), and where the time went
  (`jevLatencyMs`, `llmLatencyMs`, `toolLatencyMs`).

- Tool calls are reported as ACP describes them: each `tool_call` has its kind (`read`, `edit`, `search`, `execute`,
  `fetch`, …), a title saying what it does ("Edit src/A.java", "Run ./gradlew test", "Search for SAVE10"), the files
  it touches (`locations`, for the client's follow-along), and an edit's `diff`; a cancelled or stopped call ends
  `failed`, so none is left in progress.
- When the client advertises them (`clientCapabilities.fs`, `terminal`), its files and terminals are tools too:
  `read_text_file` and `write_text_file` (`fs/read_text_file`, `fs/write_text_file`, so the editor's unsaved buffers are
  what's read and written), `edit_text_file` with both (an exact replacement, read and written back through the editor), and `run_terminal_command` (`terminal/create`, shown in the tool call as a `terminal`
  content, awaited with `terminal/wait_for_exit`, never polled, killed on timeout or cancel, always released). A session
  tool of the same name wins; a client that advertises neither (IntelliJ IDEA's) is offered neither.
- Work in parallel (see [Work](#work)) is reported as the turn's `plan`: reading, setup, the draft, each part and
  the integration, each `pending`, `in_progress` or `completed`, every entry sent each time one changes.
- Watching a transcript: a request like "watch /tmp/soness.log and improve soness from what you see" starts a managed
  process (`watch_log`) on a soness debug log (another soness's `--debug`): the file system says when the
  log grows (nothing polls); each turn that ends there is diagnosed (failed calls, patches that applied nothing,
  repeats, slow calls, course corrections, rewritten replies, where the time went), Jev judges whether it shows
  something to act on, and if so the session works on it as a task of its own, one at a time, with the transcript
  tools (`transcript_turns`, `transcript_turn`, `transcript_call`, `transcript_search`) to see it whole. Such a task
  can't edit the watched turn's files (or copies of them). `/status` shows the watch; `/stop <id>` ends it.
- Self-improvement, in the development build only (`Test / stage`, `bin/soness-dev`; see AGENTS.md), on soness's own
  project (a clone whose git remote is github.com/jamesward/soness or /jevness):
  - the observer, a process of each session's tree (`/status` shows it, `/stop` ends it), reads the session's own turns
    as they end (every task publishes its turn, calls and results included, in the session's shared state). The
    decision model judges each; for one that shows something soness could do better, a read-only analysis looks into
    the code for the cause and records a finding in `.soness/findings.jsonl`. `SONESS_OBSERVER=false` turns it off;
  - self-improvement, by itself: each finding starts a task of the session that runs in the background on the
    findings no run has taken yet, with this process's debug log as evidence. Each round edits the project's own files
    (and, where the cause is theirs, its source dependencies') with the session's tools, and is judged by gates (a
    code change with a test, the offline suite); a rejected round's edits stay, its verdict saying why. Nothing is
    asked of you: `/status` shows the run's tree (its task, its rounds, their steps), `/stop <id>` ends it, a
    message while it runs steers it (that prompt stays open until the run ends, so its summary is in that turn), and its plan (the findings it works on, its steps) comes in the chat before it starts and its summary when it ends. What it says and its tool calls are marked `[self-improvement]`; what it says while no prompt turn is open (a client shows nothing then) is said again at the start of the next turn. One run at a time per project
    (across soness processes too); findings made during a run start another once it ends.
    `SONESS_SELF_IMPROVE_ROUNDS` (default 1) rounds a run, `SONESS_SELF_IMPROVE_EVAL=true` gates on the quick eval too (paid, about 15 minutes a round), and
    `SONESS_SELF_IMPROVE=false` turns the runs off (findings are still recorded);
  - `self_improve` ("run 3 self-improvement rounds, with the eval") starts such a run on request, on all the findings.
- `session/cancel` stops the turn; `session/close` disconnects the session's MCP servers and stops its stdio servers
  (as does the agent's exit, for sessions left open).
- A JetBrains IDE's MCP server in router mode (IntelliJ IDEA gives ACP agents one with a single tool, `execute_tool`,
  taking a command line such as `read_file --file_path build.sbt`) is recognized by its `serverInfo` ("… MCP
  Server") and its tool list, and asked once, at `session/new`, which tools it routes (the router's answer to an
  unknown tool lists them). Each routed tool is offered to Jev as a tool of its own, by name, description and parameters (name, type, whether
  required); the
  Bedrock model writes its arguments against its input schema, and soness calls it through `execute_tool` with the
  command line rendered from them (single-quoted values, JSON for non-strings). The descriptions and schemas come from
  `src/main/resources/intellij-mcp-tools.json`, the IDE's own `tools/list` in direct mode (see AGENTS.md to regenerate
  it); a routed tool it doesn't list is offered by name, its arguments written as the command line's rest. Tool call
  updates name the routed tool.
- Each prompt starts from the project's context (`ProjectContext`), read from the session's `cwd` as coding harnesses
  do: its files (outside version-control, IDE, dependency and build-output directories, at most 300 listed), its
  build and languages, and its agent instructions (`AGENTS.md`, the standard file). Jev sees it with the request, and the
  Bedrock model gets it when writing arguments and the reply. Both models are directed as a coding agent
  (`Prompts`): how to work through a question, a bug or a change, how to search (case-sensitively, so with other
  spellings and concepts when a search finds nothing), when to end, and that the reply reports, never plans.
- An IDE `apply_patch` that applied fewer operations than its patch had ("0 out of 1 operations applied…", which the
  IDE answers as a success) is reported as a failed call.
- The session's tools are resolved by capability (`ToolResolver`, `TOOLS_SPEC.md`): every MCP server's tool is
  offered; soness's own (the client's `fs` and `terminal` tools, its local file tools, `web_fetch`) only for a
  capability (read, write, patch, find, search, build, fetch) no MCP tool covers, so an IDE's `read_file` isn't
  shadowed by soness's. A shell of soness's own is offered only when a capability is left uncovered and no MCP server
  has a shell, its description naming what it's for. MCP tools whose names clash across servers are offered as
  `<server>.<tool>`. The coverage is logged at startup and is in the project context.
- Tools are tiered when a session has many (more than 24): the ones most requests need (reading, writing, patching,
  searching, building, running commands) are always offered, and the rest are offered as groups (zio-decisions'
  `ToolGroup`): one option each, its name, description and its tools' names, which Jev opens, at no step's cost, to
  have its tools offered from the next decision. A tool once called stays offered. The groups are the catalog's
  (`tool-catalog.json`: IntelliJ's debugger, databases, code intelligence, project structure, run configurations,
  editor and inspection scripts; sbta's build details, Scala symbols and library docs); an unknown MCP server's tools
  that cover no required capability are one group of its own when it has more than 8. With IntelliJ's 66 routed
  tools, the options go from about 52,000 characters to 9,000.
- When neither the session's MCP servers nor the client give file or command tools (a client without an IDE server
  or ACP `fs`/`terminal`, with only a build server like sbta), soness offers its own, on the project only (a path
  outside it is refused): `read_file` (a range, or a log's last lines), `list_directory` (any directory, build output
  and logs too, each entry's size and time, newest or largest first), `list_files` (by glob; build output when the glob
  names it or asked), `search_text` (matching lines, or counts per file), `edit_file` (an exact replacement of a unique
  piece of text; one whose lines match but for indentation is re-indented) and `write_file` when no session tool edits,
  and `run_command` (bash in the project, its exit awaited, killed on timeout or stop) when no session tool runs
  commands. `SONESS_LOCAL_TOOLS=false` leaves them out.
- A project's source dependencies (other checkouts its build compiles from source: sbt's `RootProject(file(…))` and
  `ProjectRef(file(…), …)`, Gradle's `includeBuild`) are in its context. An IDE's own tools do their files where they
  can (IntelliJ's `read_file` reads every source root); where they can't (`create_new_file`, `list_directory_tree` and
  `apply_patch` refuse a path outside the IDE's project), soness runs those calls itself, as the IDE would. Without an
  IDE, soness's own file tools reach the dependencies too. A `file://` URL given to an IDE tool as a path is taken as
  the path.
- Jev checks each reply before it's sent: one that reads as the model's own planning ("We need to call open_file")
  instead of a report to the user is written once more, told so (`SONESS_CHECK_REPLY=false` skips the check).
- What the loop does between its tool calls (writing a call's arguments, writing the reply, going on because the work
  isn't done, a course correction, moving from reading to the change) comes as ACP `agent_thought_chunk` updates.
- Besides the session's MCP tools, soness has its own `web_fetch` (an HTTP(S) GET, asking for Markdown first; HTML
  is reduced to its text, long pages come in parts), so a request can point at documentation on the web. It fetches any
  URL the model writes, local ones included; `SONESS_WEB_FETCH=false` leaves it out. A session tool of the same name
  wins.
- `web_search`, an answer with its sources, comes with the writer: Bedrock's Web Search for a Bedrock model that has it
  (`SONESS_BEDROCK_API=mantle`, an OpenAI GPT-5.4 or later model, us-east-1, us-east-2, us-west-2 or us-gov-west-1;
  the key needs the `bedrock-websearch` permissions, and pages come from Bedrock's index and cache only), Kiro's own
  for the Kiro writer, none otherwise.
- Sessions are kept, in the project's event log (`.soness/events.jsonl`, append-only, one event a line: each
  session's start, each task's request, tool calls and reply, and each file an edit changed, as a patch;
  `.soness/` ignores itself for git). The log is the
  record and everything else is a view of it: `session/list` lists the project's sessions (titled by their first
  request), `session/load` sets one up again and replays its history (requests, tool calls, replies), and
  `session/resume` sets one up again without the replay; a resumed session's tasks go on from its last. There's no
  conversation to clear: before each request Jev recalls, from the session's earlier tasks (a loaded or resumed
  session's included; the newest 20), the ones that bear on it (zio-decisions' `Recall`: one yes-or-no question
  each), and those (request, reply, files worked on) are the request's context. Other sessions' tasks aren't recalled:
  what they're doing is in the project's activity log (see [Work](#work)). A new session starts with nothing recalled.
  `SONESS_EVENT_LOG=false` keeps no log.
- A prompt that arrives while the session is working on another (a client that sends `session/prompt` before the
  turn before it has ended) is routed by Jev (zio-decisions' `Interjection`), which sees the message and each running
  task's request, time and calls so far:
  - a new task: started at once, alongside the running ones;
  - steering of a running task (a constraint, a correction, "use X instead", "stop"): added to that task's context,
    which its loop reads at every decision (Jev's state and the Bedrock model's prompts), and the prompt answered at
    once with a note;
  - a follow-up of a running task ("then commit it"): started when that task ends, with its request and reply as context;
  - a stop of a running task ("stop", "never mind"; not "stop doing X, do Y", which is steering): that task's prompt
    answers `cancelled`, and this one says so;
  - a question about the work ("what are you doing?", "is anything stuck?"): answered at once from the process tree
    (see below);
  - a stop of one process ("kill that gradle run", "the scenario 4 agent is stuck, stop it"): that tool call or loop
    is stopped, and its task goes on.

  A client can also send a message into the running turn instead of queueing it (IntelliJ IDEA's "send now"):
  soness advertises `{"steering": {"supported": true}}` in `initialize`'s `_meta` and answers `_session/steering`
  (`{sessionId, prompt}`) with `{"outcome": "injected"}`, routing the message as above (a new request or a follow-up
  becomes part of the running task's work); with nothing running the method isn't found, and the client sends it as a
  prompt.

  The response's `_meta.soness.route` says which (`new`, `steer:<task>`, `after:<task>`, `stop:<task>`). Steering that
  arrives as its task ends is taken as a follow-up of it. A session's tasks share its
  tools as a request's parallel parts do (see [Work](#work)). ACP v1 doesn't define steering or queueing (the v2 prompt
  lifecycle leaves them open), so this works with clients that send a prompt while one runs; a client that queues
  prompts until the turn ends, or cancels the running turn first, gets one task at a time.
- Each session's work is a process tree (zio-decisions' `ProcessTree`): its tasks, the loops working on them
  (a parallel request's gathering, parts and integration) and their tool calls, each with its state, times and id. The
  session advertises two commands (`available_commands_update`), answered at once whether or not work is running:
  `/status` (what's running, with what has had no answer for a while; `/status all` shows what ended too) and
  `/stop <id>` (stop a task, a loop or a tool call; a session's id stops its work). The tree is the agent's, not the
  session's: every session is a node of it, with its tasks below, so `/status` in any chat shows every chat's work
  (its own session named), and `/stop` reaches any of it. The user's other soness processes (other ACP clients)
  publish their trees under `~/.cache/soness/processes/` as they change, and `/status` shows them too, read-only. A stopped tool call is a failed step, so its loop goes on: no loop
  is ever stuck behind a call. A watchdog asks Jev about every call that has had no answer for `SONESS_STALL_SECONDS` (default
  120): zio-decisions' `Overseer` shows it the call (tool, arguments, the task it's for), how long it has run, and
  how long calls like it took before, and Jev chooses to keep waiting (asked again once the call has run twice as long)
  or to stop it (a failed step: its loop goes on and can try otherwise). The user is told either way, with the call's
  id for `/stop`. A call is stopped at `SONESS_CALL_LIMIT_SECONDS` (default 1800; 0 for no limit) whatever Jev
  judged. `OverseerEvalSpec` measures Jev's verdicts on labelled stalled calls. A shared test
  run whose callers have all been stopped is stopped too, so it doesn't hold up the runs behind it.
- In a session without tools Jev is offered only the endings.

## Evals

`SonessEvalSpec` runs each task through the ACP agent with each Bedrock model it offers, against a deterministic
order-management MCP server (`OrdersMcpServer`: 300 wide customer records, their orders, order details), grades the
answers with Jev (zio-evals' `JevJudge`) and deterministic checks, prints each model's latency and token use (Jev and
Bedrock apart), and asserts that every model answers correctly:

- **filter a wide customer list**: which Platinum-tier customers are in EMEA;
- **filter, then fan out**: which of those customers' orders are still placed.

`IntellijEvalSpec` runs typical IDE chat prompts (list files, read a file, find a symbol, explain code, the agent's
own tools, a greeting) through soness launched as a subprocess and driven with IntelliJ IDEA 2026.2.3's own
`initialize`, `session/new` and `session/prompt` (`intellij-idea-2026.2.3-ide-session.jsonl`), with IntelliJ's skill
views (`intellij-skill-views`), against a small Scala project, and checks each answer and that it came from the right
IDE tool (`search_file` lists files, `read_file` reads one, `search_symbol` finds a symbol). Its IDE MCP server is
`IntellijMcpFixture`, a mock of IntelliJ's router-only server (`IntellijRouter`) that `IntellijRouterSpec` checks
against the real server's captured exchanges (`intellij-mcp-router-2026.2.3.jsonl`); or the real IDE, run headless:
see AGENTS.md. `SONESS_EVAL_SAMPLES` defaults to 1 here.

`IntellijTaskEvalSpec` runs real-world tasks, in a user's words, harness against harness: soness, Kiro (`kiro-cli acp`)
and Claude Code (its ACP adapter, on Bedrock), each an ACP agent driven by zio-evals' `AcpAgentLoop` with the same
IntelliJ session (`SONESS_EVAL_ARMS`, default `soness,kiro,claude`; `SONESS_EVAL_KIRO_MODEL`,
`SONESS_EVAL_CLAUDE_MODEL`). So far there is one task: one so far, a bug report ("the SAVE10
code doesn't do anything at checkout… find out why and fix it") on a small shop project, which takes finding the
code, reading it and patching it. Each sample starts from a fresh copy of the project and is checked by what's on disk
afterwards (the codes now match whatever their case, nothing else changed) and by the answer (it names the cause).
It prints each turn's tool calls, Jev and LLM calls and tokens, and its latency split into Jev, LLM and tool time, a
summary across samples, and appends one record per sample to `target/evals/intellij-tasks.jsonl` (labelled with
`SONESS_EVAL_LABEL`) so runs before and after an orchestration change can be compared. Each record has its arm; Kiro and Claude
report no Jev or latency split, Claude reports tokens and cost, Kiro neither. Kiro and Claude run with your own
configuration and their own tools (shell included, permissions allowed), on a temporary copy of the project.
`IntellijEvalSpec` writes `target/evals/intellij-prompts.jsonl` the same way. With the real IDE it edits the IDE's project, resetting it
through the IDE's own tools before each sample.

`JevDecisionEvalSpec` is the fast loop for orchestration decisions: Jev requests captured from eval runs' debug logs
(`scripts/capture-jev-decisions.py <out.jsonl> <log>...` into `src/test/resources/jev-decisions/`), each labelled by what
its run's own output showed (did the draft's part pass; did a check show the work working; do most failures share one
cause), replayed against the real Jev in a few seconds, with the accuracy per kind. A kind whose question the code
builds (`shared-cause`) is rebuilt with today's question, so a change to it is measured there before an eval run.

`PlannerEvalSpec` checks the parallel planner against Bedrock alone (no Jev): the eval requests with what the gathering
would have read, planned several times, checked for the parts they should have, named for what they are.

`IntellijParallelEvalSpec` is the eval for parallel work: tasks made of independent parts that each need code
written and tests run, which agents now do one after another. `easyracer` ("Implement EasyRacer in Java 25:
https://github.com/jamesward/easyracer"; a Gradle project with the 11 scenarios as stubs, the spec only on the web,
tests against the scenario server in Docker) is checked by a hidden validator running each scenario against its own
server; `checkout` (three checkout changes, with tests, in an sbt project with a slow integration spec) by hidden
acceptance tests per change; `katas` (six small Java functions, each with its own `java KatasTest.java <kata>`, no
build tool) is the cheap one, measuring the orchestration's own cost. Each run reports which parts pass, and a timeline splitting the turn's time into shared
work, each part, validation (builds, test runs) and the reply, with the parallel estimate (shared + validation +
the longest part); for soness's parallel parts, each loop's span from the calls' `_meta.soness.loop` too. `SONESS_PARALLEL_TASKS`, `SONESS_EVAL_ARMS` (default `soness`), `SONESS_EVAL_SAMPLES`,
`SONESS_EASYRACER_SCENARIOS` (comma-separated or `all`; default `1,2,4`), `SONESS_EVAL_TURN_MINUTES` (default 15); records
in `target/evals/parallel-tasks.jsonl`, each run's final sources in `target/evals/projects/`. It needs Docker and
network; `SONESS_PARALLEL_VALIDATORS=true` checks the validators against the starting projects and reference
solutions.

`InterjectionEvalSpec` measures Jev's routing of mid-task messages: labelled cases (a running bug fix, feature or
EasyRacer task and a message: a constraint, a correction, "stop", an unrelated question or change, "then commit it",
with one or two tasks running), each routed `SONESS_EVAL_SAMPLES` times by the real Jev; it prints the accuracy per kind
and Jev's cost, and needs only `TYPESAFE_API_KEY`. `InterjectionTaskEvalSpec` runs it end to end: the shop's bug report,
and a second message sent after its first tool call (steering only it would satisfy, an unrelated question, a follow-up
feature), checked by the route and what's on disk and in the answers (`target/evals/interjection-tasks.jsonl`).

`LiveE2ESpec` is the live end-to-end check: it launches soness as a subprocess, replays IntelliJ IDEA's
`initialize` and `session/new` (with a stdio stand-in for the IDE's MCP server and the orders server over HTTP), and
for every Bedrock model the agent offers checks plain inference (a session without tools) and a tool-using prompt that
Jev orchestrates (tool call updates, answer, turn metrics). It needs `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and
`BEDROCK_MODEL_ID` (`SONESS_BEDROCK_MODELS` adds models).

The evals need `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` and are skipped without them.
`SONESS_EVAL_SAMPLES` (default 3) sets the samples per model. They call paid APIs.

```bash
./sbta shutdown   # sbt's server must see the new environment
TYPESAFE_API_KEY=… AWS_BEARER_TOKEN_BEDROCK=… BEDROCK_MODEL_ID=… ./sbta "testOnly *SonessEvalSpec"
```

```
== filter a wide customer list (3 samples per model)
model                    verdict checks  latencyMs    tokens  llmTokens  jevTokens toolCalls modelCalls
openai.gpt-oss-120b      …
```

The rest of the suite (`./sbta testFull`) runs offline: the ACP surface with stub harnesses, the Jev harness against a
local fake of Bedrock's OpenAI-compatible API and a fake Jev, the startup model check (`CatalogSpec`), and
`IntelliJReplaySpec`, which launches soness as a subprocess and replays the messages IntelliJ IDEA 2026.2.3 sent it
(`src/test/resources/intellij-idea-2026.2.3.jsonl`, captured from the IDE's ACP transport log), its IDE MCP server
launched over stdio as `IdeMcpFixture`, against HTTP fakes of Bedrock and Jev.
