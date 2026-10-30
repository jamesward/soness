# runs a command with the soness credentials from IntelliJ's acp.json (never printed): python3 envrun.py <cmd...>
import json, os, subprocess, sys
env = dict(os.environ)
acp = json.load(open(os.path.expanduser('~/.jetbrains/acp.json')))['agent_servers']['soness'].get('env', {})
env.update({k: v for k, v in acp.items() if k in ('TYPESAFE_API_KEY', 'AWS_BEARER_TOKEN_BEDROCK', 'BEDROCK_MODEL_ID', 'SONESS_BEDROCK_API', 'AWS_REGION')})
sys.exit(subprocess.run(sys.argv[1:], env=env).returncode)
