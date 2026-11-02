#!/usr/bin/env python3
"""Captures an ACP writer's (Kiro's) writings from soness --debug logs, for WritingContextEvalSpec: each writing's prompt,
the answer it got, its kind (arguments, with the tool's name, or reply), and its time.

Usage: scripts/capture-writings.py <out.jsonl> [--per-log N] <log>...

A prompt is the text soness sent in a writer session's session/prompt; its answer is the agent_message_chunk text of
that session until the prompt's response. At most N writings a log (default 12): every reply, then arguments spread
over the log, so each log's sizes and tools are represented. A run's writings are of soness's own project, so they
hold its code and docs: keep the file out of version control if that matters.
"""
import json, re, sys

def writings(path):
    text = open(path, errors='replace').read()
    writers = set(re.findall(r'ACP writer session ([0-9a-f]{8}): modes', text))
    pending, done = {}, []
    for line in text.split('\n'):
        if 'ACP send: ' not in line and 'ACP receive: ' not in line:
            continue
        body = line[line.index('ACP ') + (10 if 'ACP send: ' in line else 13):]
        body = body[:body.rfind('}') + 1]
        try:
            msg = json.loads(body)
        except ValueError:
            continue
        params = msg.get('params') or {}
        session = (params.get('sessionId') or '')[:8]
        if 'ACP send: ' in line and msg.get('method') == 'session/prompt' and session in writers:
            prompt = ''.join(b.get('text', '') for b in params.get('prompt', []))
            ts = re.match(r'timestamp=(\S+)', line).group(1)
            pending[msg['id']] = dict(session=session, prompt=prompt, answer=[], at=ts)
        elif 'ACP receive: ' in line and msg.get('method') == 'session/update':
            update = params.get('update') or {}
            if update.get('sessionUpdate') == 'agent_message_chunk':
                for w in pending.values():
                    if w['session'] == session:
                        w['answer'].append((update.get('content') or {}).get('text', ''))
        elif 'ACP receive: ' in line and 'result' in msg and isinstance(msg['result'], dict) and 'stopReason' in msg['result']:
            w = pending.pop(msg.get('id'), None)
            if w and msg['result']['stopReason'] == 'end_turn':
                done.append(w)
    for w in done:
        tail = w['prompt'][-700:]
        kind = 'reply' if 'Answer with only the reply' in tail else 'arguments' if 'Answer with only the arguments' in tail else None
        if not kind:
            continue
        tool = None
        if kind == 'arguments':
            tools = re.findall(r"\nWrite the arguments for the tool '([^']+)'", w['prompt'])
            tool = tools[0] if tools else None
        yield dict(kind=kind, tool=tool, prompt=w['prompt'], answer=''.join(w['answer']).strip(), log=path.split('/')[-1])

def main(argv):
    out, rest = argv[0], argv[1:]
    per = 12
    if rest[:1] == ['--per-log']:
        per, rest = int(rest[1]), rest[2:]
    kept = []
    for path in rest:
        ws = [w for w in writings(path) if w['answer']]
        replies = [w for w in ws if w['kind'] == 'reply']
        args = [w for w in ws if w['kind'] == 'arguments']
        chosen = replies[:max(1, per // 4)]
        room = per - len(chosen)
        if args and room > 0:
            step = max(1, len(args) // room)
            chosen += args[::step][:room]
        kept += chosen
        print(f'{path}: {len(ws)} writings, kept {len(chosen)}', file=sys.stderr)
    with open(out, 'w') as f:
        for i, w in enumerate(kept):
            f.write(json.dumps(dict(id=f'w{i:03d}', **w)) + '\n')
    print(f'{out}: {len(kept)} writings ({sum(w["kind"] == "reply" for w in kept)} replies)', file=sys.stderr)

if __name__ == '__main__':
    main(sys.argv[1:])
