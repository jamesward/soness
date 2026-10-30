# summary of quick.sh runs with a label prefix: per label and task, parts passed, mean turn time, Jev and LLM calls
import json, sys, os
from collections import defaultdict
prefix = sys.argv[1]
rows = [json.loads(l) for l in open(os.path.expanduser('~/projects/soness-dev/target/evals/parallel-tasks.jsonl'))]
by = defaultdict(list)
for r in rows:
    if r.get('label', '').startswith(prefix): by[(r['label'], r['task'])].append(r)
for (label, task), rs in sorted(by.items()):
    parts = [r.get('extra', {}).get('parts', {}) for r in rs]
    ok = sum(sum(1 for v in p.values() if v) for p in parts); n = sum(len(p) for p in parts)
    m = lambda k: sum((r.get(k) or 0) for r in rs) / len(rs)
    timeouts = sum(1 for r in rs if 'within' in (r.get('error') or ''))
    print(f"{label:46} {task:10} {ok:3}/{n:<3} {m('latencyMs')/1000:6.0f}s  jev {m('jevCalls'):5.1f}  llm {m('llmCalls'):5.1f}  timeouts {timeouts}")
