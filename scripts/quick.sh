#!/usr/bin/env bash
# usage: quick.sh <label> [strategy] [samples]: the fast check of a harness change, in the foreground, failing fast:
#   katas (6 parts, 1 s tests) and easyracer 1,2,4 (warm scenario server), parallel/auto mode, a 5-minute turn cap.
# Prints each run's parts and time; whole thing typically 5-10 minutes.
D=~/projects/soness-dev; cd $D
S=~/projects/soness/scripts
[ -n "$2" ] && export SONESS_LOOP_STRATEGY=$2
WITHCREDS_DIR=$D WITHCREDS_TIMEOUT=1800 SONESS_MODE=auto SONESS_EASYRACER_SCENARIOS=1,2,4 SONESS_EVAL_TURN_MINUTES=5 \
SONESS_EVAL_SAMPLES=${3:-2} SONESS_PARALLEL_TASKS=katas,easyracer SONESS_EVAL_LABEL=$1 SONESS_EVAL_LOG=$D/target/evals/logs/$1.log \
  python3 $S/withcreds.py "testOnly com.jamesward.soness.IntellijParallelEvalSpec -- -t parts" > target/evals/logs/$1.out 2>&1
sed 's/\x1b\[[0-9;]*m//g' target/evals/logs/$1.out | grep -a -E 'parts pass;|  error:' | cut -c1-170
