#!/usr/bin/env python3
"""Test ACP client for discovering which tools a Kiro ACP session has.

Starts an ACP agent over stdio (by default `kiro-cli acp`), runs initialize and
session/new, then (unless --no-prompt) sends one prompt that asks the agent to
list its tools without calling any. Every JSON-RPC message in either direction
is logged as one JSON line to --log, with a timestamp and a direction, so the
agent's capabilities, session modes/models, extension notifications
(`_kiro.dev/...`), available commands and any tool calls can be read afterwards.

The client advertises fs and terminal capabilities so the log shows whether
Kiro uses the client's file system or its own built-in tools. To stay
side-effect free it:
  - answers fs/read_text_file for real (reads are harmless),
  - refuses fs/write_text_file and every terminal/* request with an error,
  - rejects every session/request_permission (picks a reject option, or cancels).

Usage:
  scripts/kiro-acp-tools.py [--agent 'kiro-cli acp'] [--cwd DIR]
                            [--log /tmp/kiro-acp.jsonl] [--timeout 180]
                            [--no-fs] [--no-terminal] [--no-prompt]
                            [--prompt TEXT]

At the end it prints a summary: the initialize result, the session/new result,
the method names of every notification and request the agent sent, every
tool_call update (title, kind, rawInput) and the agent's reply text.
"""

import argparse
import json
import os
import queue
import shlex
import subprocess
import sys
import threading
import time

DEFAULT_PROMPT = (
    "Without calling any tool, list every tool you can call in this session: "
    "its exact name, where it comes from (built-in, MCP server name, or the ACP client), "
    "and a one-line description. Then list any tools that are disabled or need "
    "permission. Answer as a Markdown table."
)


class Log:
    def __init__(self, path):
        self.lock = threading.Lock()
        self.file = open(path, "w", encoding="utf-8")

    def write(self, direction, message):
        entry = {"ts": time.time(), "dir": direction, "msg": message}
        with self.lock:
            self.file.write(json.dumps(entry) + "\n")
            self.file.flush()

    def close(self):
        with self.lock:
            self.file.close()


class Client:
    def __init__(self, argv, cwd, log, fs, terminal):
        self.log = log
        self.fs = fs
        self.terminal = terminal
        self.proc = subprocess.Popen(
            argv,
            cwd=cwd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )
        self.next_id = 0
        self.pending = {}
        self.pending_lock = threading.Lock()
        self.write_lock = threading.Lock()
        self.methods_seen = {}
        self.tool_calls = []
        self.reply = []
        threading.Thread(target=self._read_stdout, daemon=True).start()
        threading.Thread(target=self._read_stderr, daemon=True).start()

    def _send(self, message):
        self.log.write("client->agent", message)
        with self.write_lock:
            self.proc.stdin.write(json.dumps(message) + "\n")
            self.proc.stdin.flush()

    def request(self, method, params, timeout):
        with self.pending_lock:
            self.next_id += 1
            rid = self.next_id
            box = queue.Queue(maxsize=1)
            self.pending[rid] = box
        self._send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        try:
            response = box.get(timeout=timeout)
        except queue.Empty:
            raise TimeoutError(f"{method}: no response in {timeout}s")
        if "error" in response:
            raise RuntimeError(f"{method}: {json.dumps(response['error'])}")
        return response.get("result")

    def notify(self, method, params):
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    def _respond(self, rid, result=None, error=None):
        message = {"jsonrpc": "2.0", "id": rid}
        if error is not None:
            message["error"] = error
        else:
            message["result"] = result
        self._send(message)

    def _read_stderr(self):
        for line in self.proc.stderr:
            self.log.write("agent-stderr", line.rstrip("\n"))

    def _read_stdout(self):
        for line in self.proc.stdout:
            line = line.strip()
            if not line:
                continue
            try:
                message = json.loads(line)
            except json.JSONDecodeError:
                self.log.write("agent-stdout-nonjson", line)
                continue
            self.log.write("agent->client", message)
            try:
                self._dispatch(message)
            except Exception as e:  # keep reading; the failure is in the log
                self.log.write("client-error", f"dispatch failed: {e!r}")
        self.log.write("agent-exit", self.proc.poll())

    def _dispatch(self, message):
        method = message.get("method")
        if method is None:
            with self.pending_lock:
                box = self.pending.pop(message.get("id"), None)
            if box is not None:
                box.put(message)
            return
        self.methods_seen[method] = self.methods_seen.get(method, 0) + 1
        params = message.get("params") or {}
        if "id" in message:
            self._handle_request(message["id"], method, params)
        elif method == "session/update":
            self._handle_update(params.get("update") or {})

    def _handle_update(self, update):
        kind = update.get("sessionUpdate")
        if kind == "agent_message_chunk":
            content = update.get("content") or {}
            if content.get("type") == "text":
                self.reply.append(content.get("text", ""))
        elif kind in ("tool_call", "tool_call_update"):
            self.tool_calls.append(update)

    def _handle_request(self, rid, method, params):
        if method == "session/request_permission":
            options = params.get("options") or []
            reject = next((o for o in options if str(o.get("kind", "")).startswith("reject")), None)
            if reject is not None:
                outcome = {"outcome": "selected", "optionId": reject["optionId"]}
            else:
                outcome = {"outcome": "cancelled"}
            self._respond(rid, {"outcome": outcome})
        elif method == "fs/read_text_file" and self.fs:
            try:
                with open(params["path"], encoding="utf-8") as f:
                    lines = f.read().splitlines(keepends=True)
                start = max((params.get("line") or 1) - 1, 0)
                limit = params.get("limit")
                chosen = lines[start:] if limit is None else lines[start:start + limit]
                self._respond(rid, {"content": "".join(chosen)})
            except (OSError, KeyError) as e:
                self._respond(rid, error={"code": -32603, "message": f"read failed: {e}"})
        else:
            # fs/write_text_file, terminal/*, or anything unknown: refuse, so the run has no side effects
            self._respond(rid, error={"code": -32601, "message": f"test client refuses {method}"})

    def close(self):
        try:
            self.proc.stdin.close()
        except OSError:
            pass
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.proc.kill()


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--agent", default="kiro-cli acp", help="agent command line (default: kiro-cli acp)")
    parser.add_argument("--cwd", default=os.getcwd(), help="session working directory")
    parser.add_argument("--log", default="/tmp/kiro-acp.jsonl", help="JSONL log of every message")
    parser.add_argument("--timeout", type=float, default=180, help="seconds to wait for each response")
    parser.add_argument("--prompt", default=DEFAULT_PROMPT)
    parser.add_argument("--no-prompt", action="store_true", help="stop after session/new")
    parser.add_argument("--no-fs", action="store_true", help="don't advertise fs capabilities")
    parser.add_argument("--no-terminal", action="store_true", help="don't advertise the terminal capability")
    args = parser.parse_args()

    cwd = os.path.abspath(args.cwd)
    log = Log(args.log)
    client = Client(shlex.split(args.agent), cwd, log, fs=not args.no_fs, terminal=not args.no_terminal)
    status = 0
    try:
        init = client.request(
            "initialize",
            {
                "protocolVersion": 1,
                "clientCapabilities": {
                    "fs": {"readTextFile": not args.no_fs, "writeTextFile": not args.no_fs},
                    "terminal": not args.no_terminal,
                },
                "clientInfo": {"name": "soness-kiro-acp-tools", "version": "0"},
            },
            args.timeout,
        )
        print("== initialize")
        print(json.dumps(init, indent=2))

        session = client.request("session/new", {"cwd": cwd, "mcpServers": []}, args.timeout)
        print("== session/new")
        print(json.dumps(session, indent=2))

        if not args.no_prompt:
            result = client.request(
                "session/prompt",
                {"sessionId": session["sessionId"], "prompt": [{"type": "text", "text": args.prompt}]},
                args.timeout,
            )
            print("== session/prompt")
            print(json.dumps(result, indent=2))
        else:
            # give the agent a moment to send its post-session notifications (commands, MCP servers)
            time.sleep(3)
    except (TimeoutError, RuntimeError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        status = 1
    finally:
        client.close()

    print("== methods the agent sent (count)")
    for method, count in sorted(client.methods_seen.items()):
        print(f"  {method}: {count}")
    print("== tool calls")
    for update in client.tool_calls:
        print("  " + json.dumps({k: update.get(k) for k in ("sessionUpdate", "toolCallId", "title", "kind", "status", "rawInput")}))
    if client.reply:
        print("== reply")
        print("".join(client.reply))
    print(f"== full log: {args.log}")
    log.close()
    sys.exit(status)


if __name__ == "__main__":
    main()
