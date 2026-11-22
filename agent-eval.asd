(defsystem "agent-eval"
  :version "0.1.0"
  :description "Agent evals on eval-protocol: agent tasks (environment, instruction, hidden checker, budgets), harness configs, a runner GF with a cl-stack-code backend, trials with event streams, diffs, tokens and time, a run store, config comparisons and gates (stack-agent-eval)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")))
