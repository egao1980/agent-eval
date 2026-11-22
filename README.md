# agent-eval

Agent evals for [cl-stack](https://github.com/egao1980/cl-stack), built on [eval-protocol](https://github.com/egao1980/eval-protocol). An agent task is an eval case: an environment, an instruction, a hidden checker and budgets. A runner GF has a [cl-stack-code](https://github.com/egao1980/cl-stack-code) backend that runs in a confined plan area. Every trial records its full event stream, diff, tokens, cost and wall time. Runs go into a run store, so harness configurations can be compared and gated.

**Status:** planned (Track E, slice E0). Brief: [cl-stack/docs/capabilities/evals.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/evals.md).

## License

MIT, see [LICENSE](LICENSE).
