(in-package #:agent-eval/tests)

;;; Spec for src/grading.lisp: grading through the task's own tests.

(defun grade-after (task setup-command &rest keys)
  "Start a local environment for TASK, run SETUP-COMMAND (the 'agent'), then
GRADE-TRIAL with KEYS → (values grade environment)."
  (let* ((dir (make-tmpdir))
         (env (make-local-environment task :directory dir)))
    (start-environment env)
    (unwind-protect
         (progn
           (when setup-command (environment-exec env setup-command))
           (values (apply #'grade-trial task env keys) env))
      (stop-environment env))))

(defun reward-script (body)
  (format nil "#!/bin/bash
LOGS=\"${AGENT_EVAL_LOGS_DIR:-/logs}\"
mkdir -p \"$LOGS/verifier\"
~a
" body))

(deftest reward-txt
  (local-lane
   (testing "pass"
     (let ((g (grade-after (hello-task) "printf 'Hello, world!\\n' > hello.txt")))
       (ok (typep g 'grade))
       (ok (= 1 (grade-reward g)))
       (ok (grade-passed-p g))
       (ok (eq :reward-txt (grade-source g)))
       (ok (equal '((:reward . 1)) (grade-rewards g)))
       (ok (eql 0 (grade-exit-code g)))
       (ok (search "hello.txt: ok" (grade-output g)))
       (ok (not (grade-timed-out-p g)))
       (ok (integerp (grade-duration-ms g)))))
   (testing "fail"
     (let ((g (grade-after (hello-task) nil)))
       (ok (= 0 (grade-reward g)))
       (ok (not (grade-passed-p g)))
       (ok (search "missing or wrong" (grade-output g)))))
   (with-tmpdir (root)
     (testing "a fraction, read as a real, not clamped"
       (let ((g (grade-after (load-tb-task (write-tb-task root "half" :test-sh
                                                          (reward-script "echo ' 0.5 ' > \"$LOGS/verifier/reward.txt\"")))
                             nil)))
         (ok (= 0.5 (grade-reward g)))
         (ok (not (grade-passed-p g)))))
     (testing "the verifier env reaches the script"
       (let ((g (grade-after (load-tb-task (write-tb-task root "venv"
                                                          :toml (toml-with "[verifier.env]
WANT = \"yes\"")
                                                          :test-sh (reward-script "[ \"$WANT\" = yes ] && echo 1 > \"$LOGS/verifier/reward.txt\"")))
                             nil)))
         (ok (= 1 (grade-reward g))))))))

(deftest reward-json
  (local-lane
   (testing "no reward member → the mean of the numeric members"
     (let ((g (grade-after (partial-task) "touch a.txt")))
       (ok (= 1/2 (rational (grade-reward g))))
       (ok (eq :reward-json (grade-source g)))
       (ok (equal '((:a-file . 1) (:b-file . 0))
                  (sort (copy-list (grade-rewards g)) #'string< :key #'car)))))
   (with-tmpdir (root)
     (testing "a reward member wins"
       (let ((g (grade-after (load-tb-task (write-tb-task root "rj" :test-sh
                                                          (reward-script "echo '{\"reward\": 1, \"other\": 0}' > \"$LOGS/verifier/reward.json\"")))
                             nil)))
         (ok (= 1 (grade-reward g)))
         (ok (grade-passed-p g))))
     (testing "a bare number"
       (let ((g (grade-after (load-tb-task (write-tb-task root "rn" :test-sh
                                                          (reward-script "echo 0.25 > \"$LOGS/verifier/reward.json\"")))
                             nil)))
         (ok (= 0.25 (grade-reward g))))))))

(deftest stale-rewards-are-removed
  (local-lane
   (with-tmpdir (root)
     (let ((task (load-tb-task (write-tb-task root "quiet" :test-sh "#!/bin/bash
exit 0
"))))
       (testing "an agent cannot pre-write its reward"
         (let ((g (handler-bind ((reward-missing
                                   (lambda (c) (declare (ignore c)) (invoke-restart 'score-zero))))
                    (grade-after task "mkdir -p \"$AGENT_EVAL_LOGS_DIR/verifier\" && echo 1 > \"$AGENT_EVAL_LOGS_DIR/verifier/reward.txt\""))))
           (ok (= 0 (grade-reward g)))
           (ok (eq :missing (grade-source g)))))))))

(deftest tests-arrive-after-the-agent
  (local-lane
   (testing "/tests does not exist before grading, and does after"
     (multiple-value-bind (g env)
         (grade-after (hello-task) "test -e \"$AGENT_EVAL_TESTS_DIR\" && echo seen > seen.txt")
       (declare (ignore g))
       (ok (null (host-file env "/app/seen.txt")))
       (ok (host-file env "/tests/test.sh"))))))

(deftest missing-reward-restarts
  (local-lane
   (with-tmpdir (root)
     (let ((ok-task (load-tb-task (write-tb-task root "exit0" :test-sh "#!/bin/bash
exit 0
")))
           (bad-task (load-tb-task (write-tb-task root "exit1" :test-sh "#!/bin/bash
exit 1
")))
           (junk-task (load-tb-task (write-tb-task root "junk" :test-sh
                                                   (reward-script "echo lots > \"$LOGS/verifier/reward.txt\"")))))
       (ok (signals (grade-after ok-task nil) 'reward-missing))
       (testing "USE-EXIT-CODE"
         (flet ((by-exit (task)
                  (handler-bind ((reward-missing
                                   (lambda (c)
                                     (ok (equal (task-id task) (reward-missing-task-id c)))
                                     (invoke-restart 'use-exit-code))))
                    (grade-after task nil))))
           (let ((g (by-exit ok-task)))
             (ok (= 1 (grade-reward g)))
             (ok (eq :exit-code (grade-source g))))
           (let ((g (by-exit bad-task)))
             (ok (= 0 (grade-reward g)))
             (ok (eql 1 (grade-exit-code g))))))
       (testing "an unparsable reward.txt is missing too"
         (ok (signals (grade-after junk-task nil) 'reward-missing)))))))

(deftest verifier-timeout
  (local-lane
   (with-tmpdir (root)
     (let* ((task (load-tb-task (write-tb-task root "slowv" :test-sh "#!/bin/bash
sleep 20
")))
            (start (get-internal-real-time))
            (g (grade-after task nil :timeout 1)))
       (ok (grade-timed-out-p g))
       (ok (= 0 (grade-reward g)))
       (ok (eq :timeout (grade-source g)))
       (ok (null (grade-exit-code g)))
       (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 10))))))

(deftest output-file
  (local-lane
   (with-tmpdir (host)
     (let ((g (grade-after (hello-task) nil :output-file (path-in host "out/test-output.txt"))))
       (declare (ignore g))
       (ok (search "missing or wrong" (read-file host "out/test-output.txt")))))))
