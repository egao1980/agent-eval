(defsystem "agent-eval"
  :version "0.1.0"
  :description "Agent evals on eval-protocol (stack-agent-eval): an agent adapter protocol (cl-stack-code -p first), a Terminal-Bench task loader, task environments on compute-protocol, grading through the task's tests, eval-protocol run records with pass@k, tokens, cost, time and transcripts, and run comparisons"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "eval-protocol" "0.3.0")
               (:version "compute-protocol" "0.2.0")
               (:version "json-protocol" "0.2.1")
               "json-backend-jzon"
               (:version "toml-protocol" "0.1.0")
               "toml-backend-tomlet"
               (:version "crypto-protocol" "0.2.0")
               "crypto-backend-ironclad")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "tasks")
               (:file "environments")
               (:file "agents")
               (:file "cl-stack-code")
               (:file "grading")
               (:file "runner")
               (:file "metrics")
               (:file "records")
               (:file "compare"))
  :in-order-to ((test-op (test-op "agent-eval/tests"))))

;;; The container environment: Terminal-Bench semantics (one container per
;;; trial, living across agent setup, the agent and the verifier) need warm
;;; podman sessions, writable mounts and file copy: compute-backend-podman
;;; 0.3.0 (egao1980/compute-backend-podman#3, egao1980/compute-protocol#10).
;;; Not a dependency of the test system: tests/container-test.lisp loads it at
;;; run time when podman and that backend version are present, else skips.
(defsystem "agent-eval/podman"
  :version "0.1.0"
  :description "agent-eval container environment on compute-protocol's podman backend"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "agent-eval" "0.1.0")
               "compute-backend-podman")
  :serial t
  :pathname "src/podman"
  :components ((:file "environment")))

(defsystem "agent-eval/tests"
  :depends-on ("agent-eval" "eval-protocol" "compute-protocol" "json-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "tasks-test")
               (:file "environment-test")
               (:file "agents-test")
               (:file "cl-stack-code-test")
               (:file "grading-test")
               (:file "runner-test")
               (:file "metrics-test")
               (:file "records-test")
               (:file "compare-test")
               (:file "container-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
