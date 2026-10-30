# Drives a staged soness over ACP stdio in <project> with one prompt; prints tool calls, plan updates and the answer.
# python3 acp-drive.py <soness-bin> <project> <prompt> [timeout-seconds]
import json, os, subprocess, sys, threading, time
bin_, project, prompt = sys.argv[1], sys.argv[2], sys.argv[3]
limit = int(sys.argv[4]) if len(sys.argv) > 4 else 600
p = subprocess.Popen([bin_, '--debug'], env={**os.environ, **({'SONESS_LOG_FILE': os.environ['ACP_LOG']} if 'ACP_LOG' in os.environ else {})}, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=open('/tmp/acp-drive.err', 'w'), text=True, bufsize=1)
responses, lock = {}, threading.Condition()
answer = []
def reader():
    for line in p.stdout:
        try: m = json.loads(line)
        except Exception: continue
        if 'id' in m and ('result' in m or 'error' in m):
            with lock: responses[m['id']] = m; lock.notify_all()
        elif m.get('method') == 'session/update':
            u = m['params']['update']; k = u.get('sessionUpdate')
            if k == 'tool_call': print(f"  tool_call   {u.get('kind'):8} {u.get('title')}  {json.dumps(u.get('rawInput'))[:120]}", flush=True)
            elif k == 'tool_call_update' and u.get('status') in ('completed', 'failed'):
                print(f"  {u['status']:11} {u['toolCallId']}  {json.dumps(u.get('rawOutput'))[:160]}", flush=True)
            elif k == 'plan': print('  plan: ' + ' | '.join(f"{e['status'][:4]} {e['content'][:40]}" for e in u['entries']), flush=True)
            elif k == 'agent_message_chunk': answer.append(u['content'].get('text', ''))
        elif 'id' in m and 'method' in m:  # a client request: refuse politely
            p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': m['id'], 'error': {'code': -32601, 'message': 'not supported'}}) + '\n')
threading.Thread(target=reader, daemon=True).start()
def call(i, method, params, wait=limit):
    p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': i, 'method': method, 'params': params}) + '\n')
    end = time.time() + wait
    with lock:
        while i not in responses and time.time() < end: lock.wait(1)
    return responses.get(i)
call(1, 'initialize', {'protocolVersion': 1, 'clientCapabilities': {}}, 120)
s = call(2, 'session/new', {'cwd': project, 'mcpServers': []}, 240)
sid = s['result']['sessionId']
if os.environ.get('ACP_MODE'):
    print('modes:', [m['id'] for m in s['result'].get('modes', {}).get('availableModes', [])], flush=True)
    print('set_mode:', call(9, 'session/set_mode', {'sessionId': sid, 'modeId': os.environ['ACP_MODE']}, 60), flush=True)
t = time.time()
r = call(3, 'session/prompt', {'sessionId': sid, 'prompt': [{'type': 'text', 'text': prompt}]})
print('stop:', (r or {}).get('result', {}).get('stopReason'), f'in {time.time() - t:.0f}s')
print('meta:', json.dumps((r or {}).get('result', {}).get('_meta', {}).get('soness', {}))[:400])
print('answer:', ''.join(answer)[:1500])
p.stdin.close(); p.terminate()
