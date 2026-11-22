#!/bin/bash
# agent-eval fixture verifier. $AGENT_EVAL_LOGS_DIR is set by agent-eval's
# environments; a plain Terminal-Bench run has /logs.
LOGS="${AGENT_EVAL_LOGS_DIR:-/logs}"
mkdir -p "$LOGS/verifier"
if [ "$(cat hello.txt 2>/dev/null)" = "$EXPECTED" ]; then
  echo "hello.txt: ok"
  echo 1 > "$LOGS/verifier/reward.txt"
else
  echo "hello.txt: missing or wrong"
  echo 0 > "$LOGS/verifier/reward.txt"
fi
