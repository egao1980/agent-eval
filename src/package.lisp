(defpackage #:agent-eval
  (:use #:cl)
  (:nicknames #:stack-agent-eval)
  (:local-nicknames (#:ev #:eval-protocol)
                    (#:compute #:compute-protocol)
                    (#:json #:json-protocol)
                    (#:toml #:toml-protocol)
                    (#:crypto #:crypto-protocol))
  (:export
   ;; conditions (src/conditions.lisp) and restarts
   #:agent-eval-error
   #:task-format-error #:task-format-error-path #:task-format-error-field
   #:unsupported-task #:unsupported-task-task-id #:unsupported-task-reason
   #:environment-error #:environment-error-task-id #:environment-error-phase
   #:agent-timeout #:agent-timeout-task-id #:agent-timeout-seconds
   #:agent-failed #:agent-failed-task-id #:agent-failed-exit-code
   #:reward-missing #:reward-missing-task-id #:reward-missing-reason
   #:dataset-mismatch #:dataset-mismatch-baseline #:dataset-mismatch-candidate
   #:run-not-found #:run-not-found-path
   #:skip-task #:use-exit-code #:score-zero
   #:+version+

   ;; Terminal-Bench tasks (src/tasks.lisp)
   #:load-tb-task #:load-tb-dataset #:tb-task-p #:task-spec #:find-task
   #:dataset-digest #:task-files-sha256
   #:task-id #:task-name #:task-instruction #:task-directory #:task-image
   #:task-dockerfile-p #:task-compose-p #:task-workdir
   #:task-agent-timeout #:task-verifier-timeout #:task-build-timeout
   #:task-cpus #:task-memory-mb #:task-storage-mb #:task-gpus
   #:task-allow-internet-p #:task-env #:task-verifier-env #:task-solution-p
   #:task-difficulty #:task-category #:task-tags
   #:+default-workdir+

   ;; environments (src/environments.lisp)
   #:task-environment #:environment-task #:environment-trial-directory
   #:start-environment #:stop-environment #:environment-started-p
   #:environment-exec #:upload-path #:download-path #:environment-command-path
   #:with-environment
   #:exec-result #:exec-result-exit-code #:exec-result-stdout #:exec-result-stderr
   #:exec-result-timed-out-p #:exec-result-duration-ms
   #:+output-cap+
   #:local-environment #:make-local-environment #:local-environment-root
   #:local-environment-host-path

   ;; the agent adapter protocol (src/agents.lisp)
   #:agent-adapter #:agent-name #:agent-version #:agent-config
   #:agent-setup #:agent-run #:agent-populate-context
   #:agent-context #:make-agent-context
   #:context-input-tokens #:context-cache-tokens #:context-output-tokens
   #:context-cost-usd #:context-turns #:context-result-text #:context-status-detail
   #:context-transcript #:context-metadata #:context-logs-directory
   #:nop-agent #:make-nop-agent
   #:oracle-agent #:make-oracle-agent
   #:command-agent #:make-command-agent #:command-agent-command

   ;; cl-stack-code -p (src/cl-stack-code.lisp)
   #:cl-stack-code-agent #:make-cl-stack-code-agent #:cl-stack-code-argv
   #:parse-stream-json #:context-from-stream-json #:usage-cost-usd

   ;; grading (src/grading.lisp)
   #:grade-trial #:grade #:grade-reward #:grade-rewards #:grade-passed-p
   #:grade-source #:grade-exit-code #:grade-output #:grade-timed-out-p
   #:grade-duration-ms

   ;; trials and runs (src/runner.lisp)
   #:run-trial #:trial #:make-trial
   #:trial-task-id #:trial-attempt #:trial-agent-name #:trial-agent-version
   #:trial-status #:trial-context #:trial-grade #:trial-reward #:trial-passed-p
   #:trial-timings #:trial-wall-ms #:trial-started-at #:trial-finished-at
   #:trial-directory #:trial-error #:trial-transcript
   #:+trial-statuses+ #:+trial-phases+
   #:run-agent-eval #:agent-eval-run #:make-agent-eval-run
   #:run-id #:run-label #:run-agent-name #:run-agent-config #:run-attempts
   #:run-trials #:run-started-at #:run-finished-at #:run-dataset-digest
   #:run-task-trials

   ;; metrics (src/metrics.lisp)
   #:pass-at-k #:run-pass-at-k #:run-accuracy #:run-totals #:run-task-stats

   ;; records (src/records.lisp)
   #:trial-plist #:save-run #:load-run

   ;; comparisons (src/compare.lisp)
   #:compare-runs #:run-comparison #:comparison-baseline-id #:comparison-candidate-id
   #:comparison-rows #:comparison-wins #:comparison-ties #:comparison-losses
   #:comparison-mean-delta #:comparison-token-delta #:comparison-cost-delta
   #:comparison-wall-delta #:comparison-only-baseline #:comparison-only-candidate
   #:comparison-report))

(in-package #:agent-eval)

;;; agent-eval E0 (0.1.0) contract. Brief: egao1980/cl-stack
;;; docs/capabilities/evals.md. Each src file's header states its rules; the
;;; Rove tests in tests/ are the spec. Every stub calls %E0-PENDING.

(defun %e0-pending (name)
  ;; A plain ERROR, not an AGENT-EVAL-ERROR, so no test that expects one of
  ;; the contract's conditions passes against a stub.
  (error "~(~a~) is not implemented yet (E0 contract)" name))
