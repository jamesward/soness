# per-loop time spans and call counts in a soness debug log: python3 phases.py <log> [session-prefix]
import re, sys
from collections import defaultdict
L = sys.argv[1]; want = sys.argv[2] if len(sys.argv) > 2 else None
span = defaultdict(lambda: [None, None]); calls = defaultdict(int); runs = defaultdict(lambda: [0, 0]); jev = defaultdict(lambda: [0, 0])
llm = defaultdict(lambda: [0, 0])
for l in open(L, errors='replace'):
    m = re.search(r'timestamp=\S+T(\d+):(\d+):([\d.]+)', l)
    if not m: continue
    t = int(m.group(1)) * 3600 + int(m.group(2)) * 60 + float(m.group(3))
    s = re.search(r'session=([0-9a-f]{8})', l)
    if want and (not s or not s.group(1).startswith(want)): continue
    lp = re.search(r'loop=(.+?)(?:\s+\w+=|"?\s*$)', l)
    key = (s.group(1) if s else '?') + ' ' + (lp.group(1).strip() if lp else '(top)')
    sp = span[key]; sp[0] = sp[0] or t; sp[1] = t
    if re.search(r'message="tool call call-\d+ [a-z_]+: ', l): calls[key] += 1
    r = re.search(r'message="tool call call-\d+ (execute_terminal_command|build_project|get_file_problems) returned in (\d+)ms', l)
    if r: runs[key][0] += 1; runs[key][1] += int(r.group(2))
    j = re.search(r'message="Jev exchange in (\d+)ms', l)
    if j: jev[key][0] += 1; jev[key][1] += int(j.group(1))
    b = re.search(r'message="Bedrock \S+ exchange in (\d+)ms', l)
    if b: llm[key][0] += 1; llm[key][1] += int(b.group(1))
t0 = min(v[0] for v in span.values())
for k, (a, b) in sorted(span.items(), key=lambda kv: kv[1][0]):
    print(f"{k:42} {a - t0:7.0f}s → {b - t0:7.0f}s ({b - a:6.0f}s)  calls {calls[k]:3}  runs {runs[k][0]:2}/{runs[k][1] // 1000:4}s  jev {jev[k][0]:3}/{jev[k][1] // 1000:3}s  llm {llm[k][0]:3}/{llm[k][1] // 1000:4}s")
