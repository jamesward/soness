#!/usr/bin/env python3
"""Captures one Bedrock exchange from a soness debug log as an LLM behavior fixture, for LlmBehaviorSpec to replay.

usage: capture-llm-behavior.py <log> <name> <description> <check>[,<check>...] [--tool T] [--match TEXT] [--last|--first] [--no-call] [--observed TEXT]

Finds the argument-writing exchanges (a forced call of tool T, if given) whose request contains TEXT (if given), takes
the last (default) or first (with --no-call, only those where the model didn't call the tool), and writes src/test/resources/llm-behaviors/<name>.json: the model, the exact request
(system prompt, user messages, inference config, the tool and its schema), what the model answered, and the checks the
replay must pass. Checks: calls-the-tool, single-patch-format, differs-from-rejected (see LlmBehaviorSpec).
"""
import json, os, re, sys

args = sys.argv[1:]
opts = {a: args[i + 1] for i, a in enumerate(args) if a in ('--tool', '--match', '--observed')}
first = '--first' in args
pos = [a for i, a in enumerate(args) if not a.startswith('--') and (i == 0 or args[i - 1] not in ('--tool', '--match', '--observed'))]
log, name, description, checks = pos[0], pos[1], pos[2], pos[3].split(',')

D = json.JSONDecoder()
found = []
for line in open(log, errors='replace'):
    m = re.search(r'message="Bedrock (\S+) exchange in (\d+)ms: request=', line)
    if not m:
        continue
    try:
        req, j = D.raw_decode(line, m.end())
        resp, _ = D.raw_decode(line, line.index('response=', j) + 9)
    except Exception:
        continue
    tools = (req.get('toolConfig') or {}).get('tools') or []
    if not tools:
        continue
    spec = tools[0]['toolSpec']
    text = ''.join(c.get('text', '') for msg in req['messages'] for c in msg['content'])
    if opts.get('--tool') and spec['name'] != opts['--tool']:
        continue
    if opts.get('--match') and opts['--match'] not in text:
        continue
    content = resp.get('output', {}).get('message', {}).get('content', [])
    if '--no-call' in args and any('toolUse' in c for c in content):
        continue
    if opts.get('--observed') and opts['--observed'] not in json.dumps(content):
        continue
    found.append((m.group(1), req, resp, spec))
if not found:
    sys.exit('no matching exchange')
model, req, resp, spec = found[0] if first else found[-1]
fixture = {
    'name': name,
    'description': description,
    'model': model,
    'checks': checks,
    'request': {
        'system': ''.join(s.get('text', '') for s in req.get('system', [])),
        'messages': [''.join(c.get('text', '') for c in msg['content']) for msg in req['messages'] if msg['role'] == 'user'],
        'maxTokens': (req.get('inferenceConfig') or {}).get('maxTokens'),
        'temperature': (req.get('inferenceConfig') or {}).get('temperature'),
        'tool': {'name': spec['name'], 'description': spec.get('description', ''), 'schema': spec['inputSchema']['json']},
    },
    'observed': resp.get('output', {}).get('message', {}).get('content', []),
}
out = os.path.join(os.path.dirname(__file__), '..', 'src/test/resources/llm-behaviors', name + '.json')
json.dump(fixture, open(out, 'w'), indent=1)
print(f'wrote {os.path.normpath(out)}: {model}, {spec["name"]}, {sum(len(m) for m in fixture["request"]["messages"])} prompt chars')
