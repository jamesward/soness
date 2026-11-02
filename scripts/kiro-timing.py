#!/usr/bin/env python3
"""Where an ACP writer's (Kiro's) writings spent their time, from a soness --debug log: time to Kiro's first update,
reasoning (first thought to first answer text), the answer's streaming, and the turn's end; session/new times; by prompt size.
Usage: scripts/kiro-timing.py <soness-debug.log>"""
import re,sys,json,datetime,statistics as st
L=sys.argv[1]
def ts(l):
    m=re.match(r'timestamp=(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)(\.\d+)?',l)
    return datetime.datetime.fromisoformat(m.group(1))+datetime.timedelta(seconds=float('0'+(m.group(2) or '.0')))
writers=set(re.findall(r'ACP writer session ([0-9a-f]{8}): modes',open(L).read()))
pend={}   # session -> current writing
out=[]; news=[]; newstart={}
for l in open(L):
    if 'ACP send' not in l and 'ACP receive' not in l: continue
    m=re.search(r'"sessionId":"([0-9a-f]{8})',l)
    t=ts(l)
    if 'ACP send' in l and '"method":"session/new"' in l:
        rid=re.search(r'"id":(\d+)',l).group(1); newstart[rid]=t
    if 'ACP receive' in l and '"result":{"sessionId"' in l:
        rid=re.search(r'"id":(\d+)',l).group(1)
        if rid in newstart: news.append((t-newstart.pop(rid)).total_seconds())
    if '"stopReason"' in l and 'ACP receive' in l:
        rid=re.search(r'"id":(\d+)',l)
        for s2,w in list(pend.items()):
            if rid and w['id']==rid.group(1):
                w['end']=t; out.append(w); del pend[s2]
        continue
    if not m or m.group(1) not in writers: continue
    s=m.group(1)
    if 'ACP send' in l and '"method":"session/prompt"' in l:
        rid=re.search(r'"id":(\d+)',l).group(1)
        txt=re.search(r'"text":"(.*)"\}\]\}\}',l)
        pend[s]=dict(start=t,id=rid,chars=len(txt.group(1)) if txt else 0,thought=None,msg=None,last=None,thoughts=0,msgs=0,tchars=0,mchars=0)
    elif s in pend:
        w=pend[s]
        if 'agent_thought_chunk' in l:
            w['thought']=w['thought'] or t; w['thoughts']+=1
            c=re.search(r'"text":"(.*?)"\}',l); w['tchars']+=len(c.group(1)) if c else 0
        elif 'agent_message_chunk' in l:
            w['msg']=w['msg'] or t; w['last']=t; w['msgs']+=1
            c=re.search(r'"text":"(.*?)"\}',l); w['mchars']+=len(c.group(1)) if c else 0
        elif '"stopReason"' in l:
            w['end']=t; out.append(w); del pend[s]
def sec(a,b): return (b-a).total_seconds() if a and b else None
rows=[]
for w in out:
    first=min(x for x in [w['thought'],w['msg']] if x) if (w['thought'] or w['msg']) else None
    rows.append(dict(total=sec(w['start'],w['end']),ttfb=sec(w['start'],first),think=sec(w['thought'],w['msg']) if w['thought'] else 0,
                     answer=sec(w['msg'],w['last']),tail=sec(w['last'],w['end']),chars=w['chars'],tchars=w['tchars'],mchars=w['mchars']))
print(f"writings: {len(rows)}; session/new: {len(news)} at {[round(x,1) for x in news]} s")
def summ(k):
    v=[r[k] for r in rows if r[k] is not None]
    return f"{k:7s} sum {sum(v):7.1f}s  median {st.median(v):5.2f}  p90 {sorted(v)[int(len(v)*.9)]:5.2f}  max {max(v):6.2f}"
for k in ['total','ttfb','think','answer','tail']: print(summ(k))
print('prompt chars median',st.median(r['chars'] for r in rows),'max',max(r['chars'] for r in rows))
print('thought chars median',st.median(r['tchars'] for r in rows),'answer chars median',st.median(r['mchars'] for r in rows))
for r in sorted(rows,key=lambda r:-r['total'])[:6]: print({k:(round(v,2) if isinstance(v,float) else v) for k,v in r.items()})
print('\nttfb by prompt size:')
for lo,hi in [(0,20000),(20000,60000),(60000,100000),(100000,10**7)]:
    v=[r['ttfb'] for r in rows if lo<=r['chars']<hi and r['ttfb']]
    if v: print(f"  {lo//1000}-{hi//1000}k chars: n={len(v)} median ttfb {st.median(v):.2f}s")
