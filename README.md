# agent-eval

Agent evals for [cl-stack](https://github.com/egao1980/cl-stack), built on [eval-protocol](https://github.com/egao1980/eval-protocol) (nick `stack-agent-eval`).

- An **agent adapter protocol** that mirrors Harbor's `BaseAgent`: `agent-setup`, `agent-run`, `agent-populate-context`. Built-in adapters are `nop`, `oracle` and `command`. The first real adapter is [cl-stack-code](https://github.com/egao1980/cl-stack-code), run headless (`-p --output-format stream-json`) inside the task environment.
- A **Terminal-Bench task loader**. It reads Terminal-Bench 2.x / Harbor task directories: `task.toml`, `instruction.md`, `environment/Dockerfile`, `tests/test.sh` and `solution/solve.sh`.
- **Task environments** on compute-protocol. There is a local path-mapped environment for fixtures and CI, and a podman container environment (`agent-eval/podman`).
- **Grading through the task's own tests**. The tests are uploaded after the agent finishes, and the reward is read from `/logs/verifier/reward.txt` or `reward.json`.
- **Results as eval-protocol records**: `agent-eval-run` is an `eval-run` with pass@k, tokens, cost, time and transcripts. Runs are saved and loaded as `run.sexp`, `result.json` and per-trial JSONL.
- **Run comparisons**, reported as sexp, JSON or Markdown.

**Status:** E0 (0.2.0: the podman environment on compute-backend-podman 0.3.0 warm sessions), Track E. Brief: [cl-stack/docs/capabilities/evals.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/evals.md).

## Tests

```sh
cl-repo test --system agent-eval
```

The tests use local fixture tasks (`tests/fixtures/tb/`) and a recorded cl-stack-code stream (`tests/fixtures/bin/fake-cl-stack-code`), so they need no network and no model. The container environment (`agent-eval/podman`) runs each trial in one warm podman session (`open-sandbox`, then `podman exec` per command, `close-sandbox`). It builds the task image from `environment/Dockerfile` with `podman-build-image` when the task has no `docker_image` or `:force-build t` is given, and copies files with `sandbox-copy-to` / `sandbox-copy-from` (`podman cp`). Its planning (image, session and exec specs, copy plan) is unit-tested without podman.

The live container lane runs only when podman answers `podman info` and the test image (`AGENT_EVAL_TEST_IMAGE`, default `docker.io/library/debian:bookworm-slim`) is already present locally. Otherwise it is skipped. CI's `test-podman-live` job (ubuntu) pulls the image and sets `AGENT_EVAL_PODMAN_LIVE=require`, so there a blocked lane fails. It also fetches the Terminal-Bench 2 task `regex-log` (harbor-framework/terminal-bench-2 at `2fd12b88`) and runs it end to end with the oracle agent (`AGENT_EVAL_TB2_TASK`); the expected reward is 1.

## License

MIT, see [LICENSE](LICENSE).
