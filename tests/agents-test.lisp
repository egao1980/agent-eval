(in-package #:agent-eval/tests)

;;; Spec for src/agents.lisp: the adapter protocol and the built-in adapters.

(defclass bare-agent (agent-adapter) ())

(deftest protocol-defaults
  (let ((a (make-instance 'bare-agent)))
    (ok (equal "bare-agent" (agent-name a)))
    (ok (null (agent-version a)))
    (ok (equal '(:name "bare-agent") (agent-config a)))
    (ok (progn (agent-setup a nil) t) "setup does nothing by default")
    (ok (progn (agent-populate-context a (make-agent-context)) t))
    (ok (signals (agent-run a "x" nil (make-agent-context)) 'agent-eval-error))))

(deftest agent-context-slots
  (let ((c (make-agent-context :logs-directory #p"/tmp/x/")))
    (ok (typep c 'agent-context))
    (ok (null (context-input-tokens c)))
    (ok (null (context-cache-tokens c)))
    (ok (null (context-output-tokens c)))
    (ok (null (context-cost-usd c)))
    (ok (null (context-turns c)))
    (ok (null (context-result-text c)))
    (ok (null (context-status-detail c)))
    (ok (null (context-transcript c)))
    (ok (null (context-metadata c)))
    (ok (equal #p"/tmp/x/" (context-logs-directory c)))
    (setf (context-input-tokens c) 7 (context-cost-usd c) 1/100
          (context-metadata c) '(:k 1))
    (ok (= 7 (context-input-tokens c)))
    (ok (= 1/100 (context-cost-usd c)))
    (ok (equal '(:k 1) (context-metadata c)))))

(deftest built-in-names
  (ok (equal "nop" (agent-name (make-nop-agent))))
  (ok (equal "oracle" (agent-name (make-oracle-agent))))
  (ok (equal "command" (agent-name (make-command-agent :command '("true")))))
  (ok (equal "mine" (agent-name (make-command-agent :command '("true") :name "mine"))))
  (ok (equal '(:name "mine" :command ("echo" "hi"))
             (agent-config (make-command-agent :command '("echo" "hi") :name "mine"))))
  (ok (equal '("echo" "hi") (command-agent-command (make-command-agent :command '("echo" "hi")))))
  (ok (signals (make-command-agent :command '()) 'agent-eval-error))
  (ok (signals (make-command-agent :command '("x" 1)) 'agent-eval-error)))

(defun run-in-env (agent task)
  "AGENT-SETUP + AGENT-RUN of AGENT on TASK in a fresh local environment →
(values environment context condition-or-nil). The environment is stopped."
  (let* ((dir (make-tmpdir))
         (env (make-local-environment task :directory dir))
         (ctx (make-agent-context :logs-directory (path-in dir "agent/")))
         (err nil))
    (start-environment env)
    (unwind-protect
         (handler-case (progn (agent-setup agent env)
                              (agent-run agent (task-instruction task) env ctx))
           (agent-eval-error (c) (setf err c)))
      (stop-environment env))
    (values env ctx err)))

(deftest nop-does-nothing
  (local-lane
   (multiple-value-bind (env ctx err) (run-in-env (make-nop-agent) (hello-task))
     (ok (null err))
     (ok (null (host-file env "/app/hello.txt")))
     (ok (null (context-transcript ctx))))))

(deftest oracle-runs-the-solution
  (local-lane
   (multiple-value-bind (env ctx err) (run-in-env (make-oracle-agent) (hello-task))
     (declare (ignore ctx))
     (ok (null err))
     (testing "solve.sh ran in the work dir with the task's env"
       (ok (equal (format nil "Hello, world!~%") (host-file env "/app/hello.txt"))))
     (testing "the solution was uploaded to /solution"
       (ok (host-file env "/solution/solve.sh"))))
   (with-tmpdir (root)
     (testing "no solution → TASK-FORMAT-ERROR :SOLUTION"
       (multiple-value-bind (env ctx err)
           (run-in-env (make-oracle-agent) (load-tb-task (write-tb-task root "nosol")))
         (declare (ignore env ctx))
         (ok (typep err 'task-format-error))
         (ok (eq :solution (task-format-error-field err)))))
     (testing "a failing solution → AGENT-FAILED with its exit code"
       (multiple-value-bind (env ctx err)
           (run-in-env (make-oracle-agent)
                       (load-tb-task (write-tb-task root "badsol" :solve-sh "#!/bin/bash
exit 4
")))
         (declare (ignore env ctx))
         (ok (typep err 'agent-failed))
         (ok (eql 4 (agent-failed-exit-code err)))))
     (testing "a slow solution → AGENT-TIMEOUT"
       (multiple-value-bind (env ctx err)
           (run-in-env (make-oracle-agent)
                       (load-tb-task (write-tb-task root "slowsol"
                                                    :toml (toml-with "[agent]
timeout_sec = 1.0")
                                                    :solve-sh "#!/bin/bash
sleep 20
")))
         (declare (ignore env ctx))
         (ok (typep err 'agent-timeout))
         (ok (= 1 (agent-timeout-seconds err)))
         (ok (equal "slowsol" (agent-timeout-task-id err))))))))

(deftest command-agent-runs-its-command
  (local-lane
   (let ((agent (make-command-agent
                 :command (list "bash" "-c" "printf '%s' \"$AGENT_EVAL_INSTRUCTION\" > got.txt; echo $FOO")
                 :env '(("FOO" . "bar")))))
     (multiple-value-bind (env ctx err) (run-in-env agent (hello-task))
       (ok (null err))
       (ok (equal (task-instruction (hello-task)) (host-file env "/app/got.txt")))
       (ok (equal "exit 0" (context-status-detail ctx)))
       (ok (search "bar" (uiop:read-file-string
                          (merge-pathnames "command.log" (context-logs-directory ctx)))))))
   (multiple-value-bind (env ctx err)
       (run-in-env (make-command-agent :command '("bash" "-c" "exit 9")) (hello-task))
     (declare (ignore env))
     (ok (typep err 'agent-failed))
     (ok (eql 9 (agent-failed-exit-code err)))
     (ok (equal "exit 9" (context-status-detail ctx))))))
