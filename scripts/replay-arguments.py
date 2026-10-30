# Replays argument-writing requests from a soness debug log against Bedrock Mantle at several reasoning efforts:
# latency, output tokens, and whether the arguments match what the run wrote (or are valid JSON for edits).
# python3 scripts/replay-arguments.py <log> <efforts comma-separated, "none" = as sent> [per-tool N]
import json, os, re, sys, time, urllib.request, concurrent.futures as cf, collections, statistics
L = sys.argv[1]; efforts = sys.argv[2].split(','); N = int(sys.argv[3]) if len(sys.argv) > 3 else 4
TOKEN = os.environ['AWS_BEARER_TOKEN_BEDROCK']; MODEL = os.environ.get('BEDROCK_MODEL_ID', 'openai.gpt-oss-120b')
URL = 'https://bedrock-mantle.us-east-1.api.aws/v1/chat/completions'
cases = collections.defaultdict(list)
for l in open(L, errors='replace'):
    if 'message="Bedrock' not in l or ' exchange in ' not in l: continue
    try:
        i = l.index('request=') + 8; j = l.index(' response=')
        req = json.loads(l[i:j]); resp = json.loads(l[j + 10:l.rindex('}') + 1])
    except Exception: continue
    uses = [c['toolUse'] for c in resp.get('output', {}).get('message', {}).get('content', []) if 'toolUse' in c]
    tc = req.get('toolConfig') or {}
    forced = (tc.get('toolChoice') or {}).get('tool', {}).get('name')
    if not uses or not forced or uses[0]['name'] != forced: continue
    if len(cases[forced]) < N: cases[forced].append((req, uses[0]['input']))
def mantle(req, effort):
    tools = [t['toolSpec'] for t in req['toolConfig']['tools']]
    body = {'model': MODEL, 'temperature': 0.0, 'max_tokens': req['inferenceConfig'].get('maxTokens', 16384),
            'messages': [{'role': 'system', 'content': '\n'.join(s['text'] for s in req.get('system', []))}] +
                        [{'role': m['role'], 'content': '\n'.join(c.get('text', '') for c in m['content'])} for m in req['messages']],
            'tools': [{'type': 'function', 'function': {'name': t['name'], 'description': t.get('description', ''),
                       'parameters': t['inputSchema']['json']}} for t in tools],
            'tool_choice': {'type': 'function', 'function': {'name': req['toolConfig']['toolChoice']['tool']['name']}}}
    if effort != 'none': body['reasoning_effort'] = effort
    r = urllib.request.Request(URL, json.dumps(body).encode(), {'Authorization': f'Bearer {TOKEN}', 'Content-Type': 'application/json'})
    t = time.time()
    try:
        out = json.load(urllib.request.urlopen(r, timeout=120))
    except Exception as e:
        return time.time() - t, 0, None, str(e)[:120]
    msg = out['choices'][0]['message']; calls = msg.get('tool_calls') or []
    args = None
    if calls:
        try: args = json.loads(calls[0]['function']['arguments'])
        except Exception: args = None
    return time.time() - t, out.get('usage', {}).get('completion_tokens', 0), args, None
jobs = [(tool, k, req, orig, e) for tool, cs in cases.items() for k, (req, orig) in enumerate(cs) for e in efforts]
with cf.ThreadPoolExecutor(8) as ex:
    res = list(ex.map(lambda j: (j, mantle(j[2], j[4])), jobs))
agg = collections.defaultdict(list)
for (tool, k, req, orig, e), (dt, tok, args, err) in res:
    same = args == orig; valid = args is not None
    agg[(tool, e)].append((dt, tok, same, valid, err))
    if os.environ.get('SHOW') == tool: print(e, k, json.dumps(orig)[:160], '=>', json.dumps(args)[:160], err or '')
for (tool, e), v in sorted(agg.items()):
    print(f"{tool:26} {e:7} n={len(v)}  median {statistics.median(x[0] for x in v):5.1f}s  out {statistics.median(x[1] for x in v):6.0f} tok  "
          f"same {sum(x[2] for x in v)}/{len(v)}  valid {sum(x[3] for x in v)}/{len(v)}  errors {sum(1 for x in v if x[4])}")
