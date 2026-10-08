# jevness

An agent harness whose only interface is the [Agent Client Protocol](https://agentclientprotocol.com) (ACP). An editor
or any other ACP client launches it over stdio, gives each session its MCP servers, and sends prompts.

Inside, [Jev](https://docs.typesafe.ai/introduction) is the orchestrator: at every runtime checkpoint Jev picks the next
action (call a tool, filter a tool's output, fan out over filtered items, answer), and the host binds tool arguments
from typed tool outputs. A generative LLM on Amazon Bedrock is just one of the tools Jev can call, to extract arguments
it can't bind, to write a filter predicate when a list is too large for Jev to classify, and to write the final answer
from the evidence Jev kept. A pure-LLM tool loop is built in as a baseline, and live evals compare the two.

Built with:

- [zio-acp](https://github.com/jamesward/zio-acp): the ACP agent side and stdio transport;
- [zio-typesafe-ai](https://github.com/jamesward/zio-typesafe-ai): Jev and its provider-neutral orchestration (`GenericOrchestrator`);
- [zio-bedrock](https://github.com/jamesward/zio-bedrock): the Bedrock LLM (Converse or Mantle), for Jev's generative tools and the baseline loop;
- [zio-http-mcp](https://github.com/jamesward/zio-http-mcp): the MCP client for the session's tools;
- [zio-evals](https://github.com/jamesward/zio-evals): the eval runner and Jev judge.

```
ACP client ──stdio──▶ JevnessAgent ──session mode──▶ JevHarness (jev)          LlmHarness (llm, baseline)
                         │                              │ Jev decides each step    │ Bedrock tool loop decides
                         │                              │ Bedrock: extract,        │ every step and reads every
                         │                              │   filter, summarize      │ tool result
                         └── MCP servers from session/new (HTTP) ◀─────────────────┘
```

## Run

```bash
./sbt stage
target/out/jvm/scala-3.10.0/jevness/universal/stage/bin/jevness
```

Configure an ACP client to launch that script. For example, in [Zed](https://zed.dev/docs/ai/external-agents):

```json
{
  "agent_servers": {
    "jevness": {
      "command": "/path/to/jevness/target/out/jvm/scala-3.10.0/jevness/universal/stage/bin/jevness",
      "env": { "TYPESAFE_API_KEY": "…", "AWS_BEARER_TOKEN_BEDROCK": "…", "BEDROCK_MODEL_ID": "…" }
    }
  }
}
```

| Variable | Required | Default | |
| --- | --- | --- | --- |
| `TYPESAFE_API_KEY` | for `jev` | | Jev API key |
| `TYPESAFE_DEFAULT_MODEL` | no | `jev-latest` | Jev model |
| `AWS_BEARER_TOKEN_BEDROCK` | yes | | [Bedrock API key](https://docs.aws.amazon.com/bedrock/latest/userguide/api-keys.html) |
| `BEDROCK_MODEL_ID` | yes | | Bedrock model, for example `us.anthropic.claude-sonnet-4-5-20250929-v1:0` (Converse) or `openai.gpt-oss-120b` (Mantle) |
| `AWS_REGION` | no | `us-east-1` | Bedrock region |
| `JEVNESS_BEDROCK_API` | no | `converse` | `converse` or `mantle` |
| `JEVNESS_MODE` | no | `jev` | the default session mode: `jev` or `llm` |

A mode whose services aren't configured is left out (with a warning on stderr); the agent exits when the default mode
is unavailable.

## Protocol surface

- `session/new` connects to the session's MCP servers. Only HTTP (Streamable HTTP) servers are supported; tool names
  must be unique across them.
- Session modes: `jev` and `llm` (the baseline). Switch with `session/set_mode`.
- `session/prompt` answers the prompt's text. Each tool call is reported as a `tool_call` update (input) and a
  `tool_call_update` (output or failure), the answer as an `agent_message_chunk`, and the turn's cost in the response's
  `_meta.jevness`: `mode`, `latencyMs`, `toolCalls`, and the Bedrock and Jev usage kept apart (`llmCalls`,
  `llmInputTokens`, `llmOutputTokens`, `jevCalls`, `jevInputTokens`, `jevOutputTokens`).

- `session/cancel` stops the turn; `session/close` disconnects the session's MCP servers.
- Each prompt is answered on its own: the harness keeps no conversation history between prompts.

## Evals

`JevnessEvalSpec` runs each task through the ACP agent in both modes against a deterministic order-management MCP
server (`OrdersMcpServer`: 300 wide customer records, their orders, order details), grades the answers with Jev
(zio-evals' `JevJudge`) and deterministic checks, prints a comparison, and asserts that both modes answer correctly and
that `jev` is faster and uses fewer tokens (Jev plus LLM) than `llm`:

- **filter a wide customer list**: which Platinum-tier customers are in EMEA;
- **filter, then fan out**: which of those customers' orders are still placed.

They need `TYPESAFE_API_KEY`, `AWS_BEARER_TOKEN_BEDROCK` and `BEDROCK_MODEL_ID` and are skipped without them.
`JEVNESS_EVAL_SAMPLES` (default 3) sets the samples per mode. They call paid APIs.

```bash
./sbt shutdown   # sbt's server must see the new environment
TYPESAFE_API_KEY=… AWS_BEARER_TOKEN_BEDROCK=… BEDROCK_MODEL_ID=… ./sbt "testOnly *JevnessEvalSpec"
```

```
== filter a wide customer list (3 samples per mode)
mode  verdict checks  latencyMs    tokens  llmTokens  jevTokens toolCalls modelCalls
jev   …
llm   …
```

The rest of the suite (`./sbt testFull`) runs offline: the ACP surface with stub harnesses, and both harnesses against
a local fake of Bedrock's OpenAI-compatible API and a fake Jev.
