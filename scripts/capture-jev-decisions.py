#!/usr/bin/env python3
"""Captures Jev decisions from soness debug logs, labelled by what the outputs show, as replayable fixtures.

usage: capture-jev-decisions.py <out.jsonl> <log>...

Each fixture is one Jev request body as soness sent it, the question it's about, and the expected answer (true or
false), derived from the request's own content:
  - part_NNN (the draft's judge: did this part's test pass?): the latest run's `scenarioN() PASSED` / `FAILED` line;
  - newest_worked (did this check show the work working?): the check's output (exit code 0 and no FAILED vs. not);
  - shared_cause (do most parts fail for one cause?): whether most FAILED lines are followed by the same cause.
JevDecisionEvalSpec replays them against the real Jev in seconds.
"""
import json, re, sys
from collections import Counter

out, logs = sys.argv[1], sys.argv[2:]
fixtures, seen = [], set()

def body_of(line):
    i = line.find('request=')
    if i < 0: return None
    try:
        obj, _ = json.JSONDecoder().raw_decode(line[i + 8:])
        return obj
    except Exception:
        return None

def text(j):
    return json.dumps(j)

for log in logs:
    for line in open(log, errors='replace'):
        if 'Jev exchange in' not in line: continue
        body = body_of(line)
        if not body: continue
        qs = body.get('questions', {})
        for qid, q in qs.items():
            label = None
            instr = q.get('instructions')
            if qid.startswith('part_') and isinstance(instr, dict) and 'latestTestRun' in instr:
                m = re.search(r'(\d+)', instr.get('part', ''))
                if not m: continue
                n = m.group(1)
                run = instr['latestTestRun']
                st = re.findall(r'scenario' + n + r'\(\) (PASSED|FAILED)', run)
                if not st: continue
                label = st[-1] == 'PASSED'
                kind = 'draft-judge'
            elif qid == 'newest_worked' and isinstance(instr, dict):
                o = instr.get('outcome', '')
                if 'command_exit_code' not in o: continue
                code = re.search(r'command_exit_code\\*"?:\s*(\d+)', o)
                if not code: continue
                label = code.group(1) == '0' and 'FAILED' not in o
                kind = 'check-worked'
            elif qid == 'shared_cause' and isinstance(instr, dict):
                run = instr.get('latestTestRun', '')
                fails = re.findall(r'\(\) FAILED\n\s*(\S[^\n]{0,120})', run)
                if len(fails) < 3: continue
                causes = Counter(re.sub(r'\d+', 'N', f).split(':')[0] for f in fails)
                label = causes.most_common(1)[0][1] / len(fails) >= 0.7
                kind = 'shared-cause'
            if label is None: continue
            # one question per fixture: the request with only that question
            one = dict(body); one['questions'] = {qid: q}
            key = (kind, text(one)[:4000])
            if key in seen: continue
            seen.add(key)
            fixtures.append({'kind': kind, 'question': qid, 'expected': label, 'body': one})

# shared causes, from the raw output of test runs with several failures (any loop's), labelled by their causes: each
# failed test's first line after FAILED that isn't a stack frame
def raw_runs(log):
    for line in open(log, errors='replace'):
        m = re.search(r'message="tool call call-\d+ execute_terminal_command returned in \d+ms: (.*)" session=', line)
        if not m: continue
        try:
            res = json.loads(m.group(1).replace('\\"', '"')) if False else None
        except Exception: res = None
        text = m.group(1).encode().decode('unicode_escape', errors='ignore') if '\\n' in m.group(1) else m.group(1)
        yield text
for log in logs:
    for run in raw_runs(log):
        lines = run.split('\n')
        fails = [i for i, t in enumerate(lines) if re.search(r'\(\)\s+FAILED', t)]
        # asked only when most parts failed: at least 6 failures
        if len(fails) < 6: continue
        firsts = [next((t.strip() for t in lines[i + 1:] if t.strip() and not t.strip().startswith('at ')), '?') for i in fails]
        firsts = [re.sub(r'\d+', 'N', re.sub(r'scenario\d+', '<t>', f))[:100] for f in firsts]
        if any('FAILED' in f or 'PASSED' in f for f in firsts): continue
        top = Counter(firsts).most_common(1)[0][1]
        parts = sorted({'scenario' + n for n in re.findall(r'scenario(\d+)\(\)', run)}, key=lambda p: int(p[8:]))
        key = ('shared-cause', run[:3000])
        if key in seen: continue
        seen.add(key)
        fixtures.append({'kind': 'shared-cause', 'question': 'shared_cause', 'expected': top / len(fails) >= 0.7,
                         'body': {'state': 'Implement EasyRacer scenarios in Java 25', 'parts': parts, 'run': run[-30000:]}})

with open(out, 'w') as f:
    for x in fixtures: f.write(json.dumps(x) + '\n')
c = Counter((x['kind'], x['expected']) for x in fixtures)
print(len(fixtures), 'fixtures:', dict(c))
