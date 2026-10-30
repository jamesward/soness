# Drives a staged soness over ACP stdio in <project> through a sequence of steps, keeping the session open between
# them (for background tasks such as self_improve, and messages sent while they run). Prints tool calls and messages.
# python3 acp-session.py <soness-bin> <project> <steps.json>
# steps: [{"prompt": text} | {"wait": text-in-messages, "timeout": seconds}, ...]; a prompt doesn't wait for its turn.
import json, os, subprocess, sys, threading, time
bin_, project, steps = sys.argv[1], sys.argv[2], json.load(open(sys.argv[3]))
p = subprocess.Popen([bin_, '--debug'], env={**os.environ, **({'SONESS_LOG_FILE': os.environ['ACP_LOG']} if 'ACP_LOG' in os.environ else {})}, stdin=subprocess.PIPE,
                     stdout=subprocess.PIPE, stderr=open(os.environ.get('ACP_ERR', '/tmp/acp-session.err'), 'w'), text=True, bufsize=1)
responses, lock, said = {}, threading.Condition(), []
def stamp(): return time.strftime('%H:%M:%S')
def reader():
    for line in p.stdout:
        try: m = json.loads(line)
        except Exception: continue
        if 'id' in m and ('result' in m or 'error' in m):
            with lock: responses[m['id']] = m; lock.notify_all()
            r = m.get('result') or {}
            print(f"{stamp()} response {m['id']}: {r.get('stopReason') or m.get('error')} "
                  f"{json.dumps((r.get('_meta') or {}).get('soness', {}).get('route', ''))}", flush=True)
        elif m.get('method') == 'session/update':
            u = m['params']['update']; k = u.get('sessionUpdate')
            if k == 'tool_call': print(f"{stamp()}   tool_call {u.get('kind'):8} {u.get('title')}  {json.dumps(u.get('rawInput'))[:140]}", flush=True)
            elif k == 'tool_call_update' and u.get('status') == 'failed':
                print(f"{stamp()}   failed {u['toolCallId']}  {json.dumps(u.get('rawOutput'))[:200]}", flush=True)
            elif k == 'agent_message_chunk':
                t = u['content'].get('text', '')
                with lock: said.append(t); lock.notify_all()
                print(f"{stamp()} > {t.strip()[:2000]}", flush=True)
        elif 'id' in m and 'method' in m:
            p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': m['id'], 'error': {'code': -32601, 'message': 'not supported'}}) + '\n')
threading.Thread(target=reader, daemon=True).start()
def send(i, method, params): p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': i, 'method': method, 'params': params}) + '\n')
def await_response(i, wait):
    end = time.time() + wait
    with lock:
        while i not in responses and time.time() < end: lock.wait(1)
    return responses.get(i)
send(1, 'initialize', {'protocolVersion': 1, 'clientCapabilities': {}}); await_response(1, 120)
send(2, 'session/new', {'cwd': project, 'mcpServers': []}); sid = await_response(2, 240)['result']['sessionId']
n = 10
for step in steps:
    if 'prompt' in step:
        n += 1; print(f"{stamp()} << {step['prompt']}", flush=True)
        send(n, 'session/prompt', {'sessionId': sid, 'prompt': [{'type': 'text', 'text': step['prompt']}]})
    else:
        end = time.time() + step.get('timeout', 600)
        with lock:
            while not any(step['wait'] in t for t in said) and time.time() < end: lock.wait(5)
        print(f"{stamp()} waited for {step['wait']!r}: {'seen' if any(step['wait'] in t for t in said) else 'timed out'}", flush=True)
p.stdin.close(); p.terminate()
