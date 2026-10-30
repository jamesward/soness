# What went wrong in a run, from its soness debug log (`--debug`): per tool, calls and failures; identical
# calls repeated; course corrections; replies rewritten; turn failures.  python3 scripts/diagnose-run.py <log>
import re, sys, collections
calls = collections.Counter(); fails = collections.Counter(); args = collections.Counter()
events = collections.Counter(); errors = []
for l in open(sys.argv[1], errors='replace'):
    m = re.search(r'message="tool call (call-\d+) ([a-z_-]+): (.{0,400})', l) if ' exchange in ' not in l else None
    if m:
        calls[m.group(2)] += 1
        args[(m.group(2), m.group(3)[:200])] += 1
    m = re.search(r'message="tool call call-\d+ ([a-z_-]+) failed: (.{0,140})', l)
    if m: fails[m.group(1)] += 1; errors.append(f"{m.group(1)}: {m.group(2)}")
    if ' exchange in ' in l: continue  # a model exchange quotes the steps: count each event once, where it's logged
    for k, p in [('course correction', r'course correction: (\w+)'), ('reply rewritten', r'the reply read as planning'),
                 ('turn failed', r'prompt turn of \S+ failed'), ('loop limit', r'took more than \d+ decisions'),
                 ('proposal', r'would call \w+ instead'), ('repeat proposal', r'would repeat step'),
                 ('recalled', r'recalled \d+ of \d+'), ('stalled stop', r'stopped as stalled|judged stalled')]:
        mm = re.search(p, l)
        if mm: events[k + (f" {mm.group(1)}" if mm.groups() else '')] += 1
print("tool                         calls  failed")
for t, n in calls.most_common(): print(f"  {t:26} {n:5}  {fails[t]:6}")
print("repeated identical calls:", sum(n - 1 for n in args.values() if n > 1))
for k, n in sorted(events.items()): print(f"  {k}: {n}")
print("top failures:")
for e, n in collections.Counter(re.sub(r'\d+', 'N', e)[:110] for e in errors).most_common(8): print(f"  {n:3}  {e}")
