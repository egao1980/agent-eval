(in-package #:agent-eval/tests)

;;; Spec for src/runner.lisp: trials and runs.

(deftest constants
  (ok (equal '(:completed :agent-timeout :agent-error :environment-error) +trial-statuses+))
  (ok (equal '(:environment-setup :agent-setup :agent-execution :verifier) +trial-phases+)))

(deftest oracle-trial-passes
  (local-lane
   (with-tmpdir (dir)
     (let ((trial (run-trial (hello-task) (make-oracle-agent) :directory dir :attempt 2)))
       (ok (typep trial 'trial))
       (ok (equal "hello-world" (trial-task-id trial)))
       (ok (= 2 (trial-attempt trial)))
       (ok (equal "oracle" (trial-agent-name trial)))
       (ok (eq :completed (trial-status trial)))
       (ok (= 1 (trial-reward trial)))
       (ok (trial-passed-p trial))
       (ok (typep (trial-grade trial) 'grade))
       (ok (null (trial-error trial)))
       (testing "every phase timed, in order"
         (ok (equal +trial-phases+
                    (loop for (k nil) on (trial-timings trial) by #'cddr collect k)))
         (ok (every #'integerp (loop for (nil v) on (trial-timings trial) by #'cddr collect v))))
       (ok (integerp (trial-wall-ms trial)))
       (ok (= 20 (length (trial-started-at trial))))
       (ok (char= #\Z (char (trial-finished-at trial) 19)))
       (ok (equal (uiop:native-namestring (truename dir)) (trial-directory trial)))
       (testing "the verifier's output is kept in the trial directory"
         (ok (search "hello.txt: ok" (read-file dir "verifier/test-output.txt"))))))))

(deftest nop-trial-fails
  (local-lane
   (let ((trial (run-trial (hello-task) (make-nop-agent))))
     (ok (eq :completed (trial-status trial)))
     (ok (= 0 (trial-reward trial)))
     (ok (not (trial-passed-p trial))))))

(deftest cl-stack-code-trial
  (local-lane
   (let ((trial (run-trial (hello-task)
                           (make-cl-stack-code-agent :command (fake-cl-stack-code-command)
                                                     :prices '(:input 1 :output 2)))))
     (ok (eq :completed (trial-status trial)))
     (ok (trial-passed-p trial))
     (ok (= 135 (context-input-tokens (trial-context trial))))
     (ok (= (/ (+ 135 40) 1000000) (context-cost-usd (trial-context trial))))
     (ok (= 5 (length (trial-transcript trial))))
     (ok (probe-file (merge-pathnames "agent/cl-stack-code.jsonl"
                                      (uiop:ensure-directory-pathname (trial-directory trial))))))))

(deftest agent-end-states-are-still-graded
  (local-lane
   (with-tmpdir (root)
     (testing "AGENT-TIMEOUT → :agent-timeout, then graded"
       (let* ((task (load-tb-task (write-tb-task root "late"
                                                 :toml (toml-with "[agent]
timeout_sec = 1.0")
                                                 :solve-sh "#!/bin/bash
echo partial > done.txt
sleep 20
")))
              (trial (run-trial task (make-oracle-agent))))
         (ok (eq :agent-timeout (trial-status trial)))
         (ok (typep (trial-grade trial) 'grade))
         (ok (= 1 (trial-reward trial)) "this task's verifier always passes")
         (ok (stringp (trial-error trial)))))
     (testing "AGENT-FAILED → :agent-error, then graded"
       (let ((trial (run-trial (hello-task)
                               (make-cl-stack-code-agent
                                :command (fake-cl-stack-code-command)
                                :env '(("FAKE_CSC_MODE" . "fail"))))))
         (ok (eq :agent-error (trial-status trial)))
         (ok (typep (trial-grade trial) 'grade))
         (ok (= 0 (trial-reward trial)))
         (ok (search "agent failed" (trial-error trial)))))
     (testing "any other error in AGENT-RUN → :agent-error"
       (let ((trial (run-trial (hello-task) (make-instance 'exploding-agent))))
         (ok (eq :agent-error (trial-status trial)))
         (ok (search "boom" (trial-error trial)))
         (ok (trial-grade trial)))))))

(deftest setup-errors-are-not-graded
  (local-lane
   (let ((trial (run-trial (hello-task) (make-instance 'broken-setup-agent))))
     (ok (eq :environment-error (trial-status trial)))
     (ok (null (trial-grade trial)))
     (ok (= 0 (trial-reward trial)))
     (ok (search "cannot install" (trial-error trial)))
     (ok (getf (trial-timings trial) :agent-setup))
     (ok (null (getf (trial-timings trial) :verifier))))
   (let ((trial (run-trial (hello-task) (make-nop-agent)
                           :environment-factory (lambda (task dir)
                                                  (declare (ignore task dir))
                                                  (error "no environment")))))
     (ok (eq :environment-error (trial-status trial)))
     (ok (search "no environment" (trial-error trial))))))

(deftest unsupported-tasks
  (with-tmpdir (root)
    (ok (signals (run-trial (load-tb-task (write-tb-task root "compose" :compose "services: {}
"))
                            (make-nop-agent))
                 'unsupported-task))
    (ok (signals (run-trial (load-tb-task (write-tb-task root "gpu" :toml (toml-with "[environment]
gpus = 1")))
                            (make-nop-agent))
                 'unsupported-task))))

(deftest missing-reward-policy
  (local-lane
   (with-tmpdir (root)
     (let ((task (load-tb-task (write-tb-task root "exit0" :test-sh "#!/bin/bash
exit 0
"))))
       (ok (eq :missing (grade-source (trial-grade (run-trial task (make-nop-agent))))))
       (ok (= 1 (trial-reward (run-trial task (make-nop-agent) :on-missing-reward :exit-code))))
       (ok (signals (run-trial task (make-nop-agent) :on-missing-reward :error)
                    'reward-missing))))))

(deftest make-trial-defaults
  (let ((tr (make-trial :task-id "t")))
    (ok (= 1 (trial-attempt tr)))
    (ok (eq :completed (trial-status tr)))
    (ok (= 0 (trial-reward tr)))
    (ok (not (trial-passed-p tr)))))

;;; Runs

(deftest run-over-the-fixture-dataset
  (local-lane
   (with-tmpdir (dir)
     (let* ((ds (fixture-dataset))
            (run (run-agent-eval ds (make-oracle-agent) :attempts 2 :directory dir
                                                        :label "oracle")))
       (ok (typep run 'agent-eval-run))
       (ok (typep run 'ev:eval-run))
       (ok (eq ds (ev:eval-run-dataset run)))
       (ok (= 4 (length (ev:eval-run-results run))) "one result per attempt")
       (ok (= 4 (length (run-trials run))))
       (ok (equal '(("hello-world" . 1) ("hello-world" . 2)
                    ("partial-credit" . 1) ("partial-credit" . 2))
                  (mapcar (lambda (tr) (cons (trial-task-id tr) (trial-attempt tr)))
                          (run-trials run))))
       (testing "eval-protocol's aggregates"
         (ok (= 4 (ev:eval-run-n run)))
         (ok (= 2 (ev:eval-run-pass-count run)))
         (ok (= 3/4 (rational (ev:eval-run-mean run)))))
       (testing "each result: the trial as actual, the reward as score"
         (let ((r (first (ev:eval-run-results run))))
           (ok (eq (first (run-trials run)) (ev:eval-case-result-actual r)))
           (ok (eq :pass (ev:eval-score-verdict (ev:eval-case-result-score r))))
           (ok (search "reward 1" (ev:eval-score-rationale (ev:eval-case-result-score r))))))
       (ok (equal "oracle" (run-label run)))
       (ok (equal "oracle" (run-agent-name run)))
       (ok (equal '(:name "oracle") (run-agent-config run)))
       (ok (= 2 (run-attempts run)))
       (ok (equal (dataset-digest ds) (run-dataset-digest run)))
       (ok (eql 0 (search "run-" (run-id run))))
       (ok (= 2 (length (run-task-trials run "partial-credit"))))
       (ok (stringp (run-started-at run)))
       (ok (stringp (run-finished-at run)))
       (testing "trial directories"
         (ok (read-file dir "trials/partial-credit/2/verifier/test-output.txt")))))))

(deftest pass-at-k-over-attempts
  (local-lane
   (let* ((ds (ev:make-eval-dataset :name "one" :cases (list (hello-task))))
          (run (run-agent-eval ds (make-instance 'flaky-agent :pass-on '(2)) :attempts 3)))
     (ok (equal '(nil t nil) (mapcar #'trial-passed-p (run-trials run))))
     (ok (= 1/3 (run-pass-at-k run 1)))
     (ok (= 2/3 (run-pass-at-k run 2)))
     (ok (= 1 (run-pass-at-k run 3))))))

(deftest unsupported-policy
  (local-lane
   (with-tmpdir (root)
     (write-tb-task root "a-compose" :compose "services: {}
")
     (copy-tree-to (tb-fixture "hello-world") (path-in root "b-hello/"))
     (let ((ds (load-tb-dataset root)))
       (testing ":skip (default)"
         (let ((run (run-agent-eval ds (make-oracle-agent) :attempts 2)))
           (ok (= 3 (length (ev:eval-run-results run))))
           (ok (ev:eval-case-result-skipped-p (first (ev:eval-run-results run))))
           (ok (= 2 (length (run-trials run))))
           (ok (= 2 (ev:eval-run-n run)))))
       (testing ":fail"
         (let ((run (run-agent-eval ds (make-oracle-agent) :on-unsupported :fail)))
           (ok (= 2 (ev:eval-run-n run)))
           (ok (= 1 (ev:eval-run-pass-count run)))
           (ok (search "unsupported"
                       (ev:eval-score-rationale
                        (ev:eval-case-result-score (first (ev:eval-run-results run))))))))
       (testing ":error"
         (ok (signals (run-agent-eval ds (make-oracle-agent) :on-unsupported :error)
                      'unsupported-task)))))))

(defun counting-reward-task (root counter)
  "A task whose verifier writes no reward on its first run and reward 1 after
(COUNTER is a host file; the local environment is trusted)."
  (load-tb-task
   (write-tb-task root "counted" :test-sh (format nil "#!/bin/bash
LOGS=\"${AGENT_EVAL_LOGS_DIR:-/logs}\"
mkdir -p \"$LOGS/verifier\"
if [ -f '~a' ]; then echo 1 > \"$LOGS/verifier/reward.txt\"; else touch '~a'; fi
" counter counter))))

(deftest escaping-errors-use-eval-protocol-restarts
  (local-lane
   (with-tmpdir (root)
     (let* ((counter (uiop:native-namestring (path-in root "counter")))
            (ds (ev:make-eval-dataset :name "one" :cases (list (counting-reward-task root counter)))))
       (flet ((fresh () (uiop:delete-file-if-exists counter))
              (go-run (restart)
                (handler-bind ((ev:scorer-error
                                 (lambda (c)
                                   (ok (typep (ev:scorer-error-cause c) 'reward-missing))
                                   (invoke-restart restart))))
                  (run-agent-eval ds (make-nop-agent) :on-missing-reward :error))))
         (testing "unhandled: REWARD-MISSING escapes RUN-TRIAL as EV:SCORER-ERROR"
           (fresh)
           (ok (signals (run-agent-eval ds (make-nop-agent) :on-missing-reward :error)
                        'ev:scorer-error)))
         (testing "SKIP-CASE"
           (fresh)
           (let ((run (go-run 'ev:skip-case)))
             (ok (ev:eval-case-result-skipped-p (first (ev:eval-run-results run))))
             (ok (= 0 (ev:eval-run-n run)))))
         (testing "RETRY-CASE runs the same attempt again"
           (fresh)
           (let ((run (go-run 'ev:retry-case)))
             (ok (= 1 (length (ev:eval-run-results run))))
             (ok (= 1 (ev:eval-run-pass-count run)))
             (ok (= 1 (trial-attempt (first (run-trials run)))))))
         (testing "SCORE-AS-FAILURE"
           (fresh)
           (let ((run (go-run 'ev:score-as-failure)))
             (ok (= 1 (ev:eval-run-n run)))
             (ok (= 0 (ev:eval-run-pass-count run))))))))
   (testing "the trial's own errors are statuses, not escapes"
     (let ((run (run-agent-eval (ev:make-eval-dataset :name "one" :cases (list (hello-task)))
                                (make-instance 'exploding-agent))))
       (ok (eq :agent-error (trial-status (first (run-trials run)))))))))
