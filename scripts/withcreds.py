# runs ./sbta with the soness credentials from IntelliJ's acp.json (never printed)
import json, os, subprocess, sys
env = dict(os.environ)
acp = json.load(open(os.path.expanduser('~/.jetbrains/acp.json')))['agent_servers']['soness'].get('env', {})
env.update({k: v for k, v in acp.items() if k in ('TYPESAFE_API_KEY', 'AWS_BEARER_TOKEN_BEDROCK', 'BEDROCK_MODEL_ID', 'SONESS_BEDROCK_API', 'AWS_REGION')})
env.pop('SONESS_BEDROCK_MODELS', None)
if os.environ.get('EVAL_MODEL'): env['BEDROCK_MODEL_ID'] = os.environ['EVAL_MODEL']
sys.exit(subprocess.run(['timeout', os.environ.get('WITHCREDS_TIMEOUT', '3600'), './sbta', '--server', '-batch'] + sys.argv[1:], env=env, cwd=os.environ.get('WITHCREDS_DIR', os.path.expanduser('~/projects/soness'))).returncode)
