# A watcher soness session in <project>: prompts it to watch <log>, then prints what it says and does for <minutes>.
# python3 acp-watch.py <soness-bin> <project> <log> <minutes> [instructions]
import json, os, subprocess, sys, threading, time
bin_, project, log, minutes = sys.argv[1], sys.argv[2], sys.argv[3], float(sys.argv[4])
instructions = sys.argv[5] if len(sys.argv) > 5 else 'improve soness from what you see'
p = subprocess.Popen([bin_, '--debug'], env={**os.environ, **({'SONESS_LOG_FILE': os.environ['ACP_LOG']} if 'ACP_LOG' in os.environ else {})}, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=open('/tmp/acp-watcher.err', 'w'), text=True, bufsize=1)
responses, lock = {}, threading.Condition()
def reader():
    for line in p.stdout:
        try: m = json.loads(line)
        except Exception: continue
        if 'id' in m and ('result' in m or 'error' in m):
            with lock: responses[m['id']] = m; lock.notify_all()
        elif m.get('method') == 'session/update':
            u = m['params']['update']; k = u.get('sessionUpdate')
            if k == 'tool_call': print(f"{time.strftime('%H:%M:%S')}  tool_call {u.get('title')}  {json.dumps(u.get('rawInput'))[:140]}", flush=True)
            elif k == 'tool_call_update' and u.get('status') == 'failed': print(f"{time.strftime('%H:%M:%S')}  failed {json.dumps(u.get('rawOutput'))[:160]}", flush=True)
            elif k == 'agent_message_chunk': print(f"{time.strftime('%H:%M:%S')}  SAYS: {u['content'].get('text', '')[:1500]}", flush=True)
        elif 'id' in m and 'method' in m:
            p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': m['id'], 'error': {'code': -32601, 'message': 'not supported'}}) + '\n')
threading.Thread(target=reader, daemon=True).start()
def call(i, method, params, wait=600):
    p.stdin.write(json.dumps({'jsonrpc': '2.0', 'id': i, 'method': method, 'params': params}) + '\n')
    end = time.time() + wait
    with lock:
        while i not in responses and time.time() < end: lock.wait(1)
    return responses.get(i)
call(1, 'initialize', {'protocolVersion': 1, 'clientCapabilities': {}}, 120)
sid = call(2, 'session/new', {'cwd': project, 'mcpServers': []}, 240)['result']['sessionId']
r = call(3, 'session/prompt', {'sessionId': sid, 'prompt': [{'type': 'text', 'text': f'watch {log} and {instructions}'}]})
print('watch prompt:', (r or {}).get('result', {}).get('stopReason'), flush=True)
time.sleep(minutes * 60)
p.stdin.close(); p.terminate()
