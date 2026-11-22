#!/bin/bash
# agent-eval fixture verifier: one reward member per expected file, no
# "reward" member, so the reward is their mean.
LOGS="${AGENT_EVAL_LOGS_DIR:-/logs}"
mkdir -p "$LOGS/verifier"
a=0; b=0
[ -f a.txt ] && a=1
[ -f b.txt ] && b=1
printf '{"a_file": %s, "b_file": %s, "note": "fixture"}\n' "$a" "$b" > "$LOGS/verifier/reward.json"
echo "a=$a b=$b"
