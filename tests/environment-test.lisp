(in-package #:agent-eval/tests)

;;; Spec for src/environments.lisp: the protocol through LOCAL-ENVIRONMENT.

(deftest local-environment-layout
  (local-lane
   (with-tmpdir (dir)
     (let ((env (make-local-environment (hello-task) :directory dir)))
       (ok (typep env 'task-environment))
       (ok (not (environment-started-p env)))
       (ok (eq env (start-environment env)))
       (ok (environment-started-p env))
       (ok (equal (truename dir) (truename (environment-trial-directory env))))
       (ok (equal "hello-world" (task-id (environment-task env))))
       (ok (equal (truename (path-in dir "root/")) (truename (local-environment-root env))))
       (testing "an environment path maps under the root"
         (ok (equal (uiop:native-namestring (path-in dir "root/app/hello.txt"))
                    (uiop:native-namestring (local-environment-host-path env "/app/hello.txt"))))
         (ok (equal (uiop:native-namestring (local-environment-host-path env "/tests/test.sh"))
                    (environment-command-path env "/tests/test.sh"))))
       (testing "start makes the work dir and the log dirs"
         (ok (uiop:directory-exists-p (local-environment-host-path env "/app/")))
         (ok (uiop:directory-exists-p (local-environment-host-path env "/logs/agent/")))
         (ok (uiop:directory-exists-p (local-environment-host-path env "/logs/verifier/"))))
       (ok (eq env (start-environment env)) "starting twice does nothing")
       (ok (null (stop-environment env)))
       (ok (not (environment-started-p env)))
       (ok (null (stop-environment env)) "stopping twice does nothing")))))

(deftest exec-basics
  (local-lane
   (with-tmpdir (dir)
     (with-environment (env (make-local-environment (hello-task) :directory dir))
       (testing "a string runs under bash -c in the work dir"
         (let ((r (environment-exec env "pwd; echo err >&2; exit 3")))
           (ok (typep r 'exec-result))
           (ok (= 3 (exec-result-exit-code r)))
           (ok (equal (string-right-trim "/" (environment-command-path env "/app"))
                      (string-right-trim '(#\/ #\Newline) (exec-result-stdout r))))
           (ok (equal (format nil "err~%") (exec-result-stderr r)))
           (ok (not (exec-result-timed-out-p r)))
           (ok (and (integerp (exec-result-duration-ms r)) (<= 0 (exec-result-duration-ms r))))))
       (testing "a list is an argv"
         (let ((r (environment-exec env (list "printf" "%s|%s" "a b" "c"))))
           (ok (= 0 (exec-result-exit-code r)))
           (ok (equal "a b|c" (exec-result-stdout r)))))
       (testing ":directory is an environment path"
         (let ((r (environment-exec env "pwd" :directory "/logs")))
           (ok (search "logs" (exec-result-stdout r)))))
       (testing "the task's env, the agent-eval variables, then :env (later wins)"
         (let ((r (environment-exec env "printf '%s|%s|%s' \"$GREETING\" \"$X\" \"$AGENT_EVAL_LOGS_DIR\""
                                    :env '(("X" . "1")))))
           (ok (equal (format nil "Hello, world!|1|~a"
                              (string-right-trim "/" (environment-command-path env "/logs")))
                      (exec-result-stdout r))))
         (let ((r (environment-exec env "printf '%s' \"$GREETING\""
                                    :env '(("GREETING" . "override")))))
           (ok (equal "override" (exec-result-stdout r))))
         (let ((r (environment-exec env "printf '%s|%s' \"$AGENT_EVAL_WORKDIR\" \"$AGENT_EVAL_TESTS_DIR\"")))
           (ok (equal (format nil "~a|~a"
                              (string-right-trim "/" (environment-command-path env "/app"))
                              (string-right-trim "/" (environment-command-path env "/tests")))
                      (exec-result-stdout r)))))
       (testing "files written by a command land under the root"
         (environment-exec env "echo hi > made.txt")
         (ok (equal (format nil "hi~%") (host-file env "/app/made.txt"))))))))

(deftest exec-timeout
  (local-lane
   (with-tmpdir (dir)
     (with-environment (env (make-local-environment (hello-task) :directory dir))
       (let* ((start (get-internal-real-time))
              (r (environment-exec env "sleep 20" :timeout 1))
              (secs (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
         (ok (exec-result-timed-out-p r))
         (ok (null (exec-result-exit-code r)))
         (ok (< secs 10) "killed well before the sleep ends"))))))

(deftest exec-output-cap
  (local-lane
   (with-tmpdir (dir)
     (with-environment (env (make-local-environment (hello-task) :directory dir))
       (let* ((n (+ +output-cap+ 10))
              (r (environment-exec env (format nil "head -c ~d /dev/zero | tr '\\0' x" n)))
              (out (exec-result-stdout r)))
         (ok (= 1048576 +output-cap+))
         (ok (every (lambda (c) (char= c #\x)) (subseq out 0 +output-cap+)))
         (ok (search "[agent-eval: truncated 10 characters]" out)))))))

(deftest exec-needs-a-started-environment
  (with-tmpdir (dir)
    (let ((env (make-local-environment (hello-task) :directory dir)))
      (ok (signals (environment-exec env "true") 'environment-error)))))

(deftest upload-and-download
  (local-lane
   (with-tmpdir (dir)
     (with-tmpdir (host)
       (with-environment (env (make-local-environment (hello-task) :directory dir))
         (write-file host "one.txt" "1")
         (write-file host "tree/a.txt" "a")
         (write-file host "tree/sub/b.txt" "b")
         (upload-path env (path-in host "one.txt") "/data/one.txt")
         (ok (equal "1" (host-file env "/data/one.txt")))
         (testing "a directory's contents go into the target directory"
           (upload-path env (path-in host "tree/") "/tests")
           (ok (equal "a" (host-file env "/tests/a.txt")))
           (ok (equal "b" (host-file env "/tests/sub/b.txt"))))
         (testing "download is the reverse"
           (environment-exec env "mkdir -p out && echo z > out/z.txt")
           (download-path env "/app/out" (path-in host "back/"))
           (ok (equal (format nil "z~%") (read-file host "back/z.txt")))
           (download-path env "/data/one.txt" (path-in host "copy.txt"))
           (ok (equal "1" (read-file host "copy.txt")))))))))

(deftest with-environment-stops-on-unwind
  (local-lane
   (with-tmpdir (dir)
     (let ((seen nil))
       (ignore-errors
        (with-environment (env (make-local-environment (hello-task) :directory dir))
          (setf seen env)
          (error "leave")))
       (ok seen)
       (ok (not (environment-started-p seen)))))))

(defclass counting-backend (compute:native-process-backend)
  ((calls :initform 0 :accessor backend-calls)))

(defmethod compute:run-sandboxed :before ((b counting-backend) spec)
  (declare (ignore spec))
  (incf (backend-calls b)))

(deftest commands-go-through-compute-protocol
  (local-lane
   (with-tmpdir (dir)
     (let ((backend (make-instance 'counting-backend)))
       (with-environment (env (make-local-environment (hello-task) :directory dir
                                                                   :backend backend))
         (let ((before (backend-calls backend)))
           (environment-exec env "true")
           (environment-exec env "true")
           (ok (= (+ before 2) (backend-calls backend)))))))))

;;; agent-eval#5: env values never on a command line

(defclass recording-backend (compute:native-process-backend)
  ((specs :initform '() :accessor recorded-specs)))

(defmethod compute:run-sandboxed ((backend recording-backend) spec)
  (push spec (recorded-specs backend))
  (call-next-method))

(deftest env-values-stay-off-argv
  (testing "ENV-EXPORT-SCRIPT quotes values for sh"
    (ok (equal (format nil "export A='x'\\''y'~%") (env-export-script '(("A" . "x'y")))))
    (ok (signals (env-export-script '(("A B" . "1"))) 'agent-eval-error)))
  (local-lane
   (with-tmpdir (dir)
     (let* ((secret "sk-test-SECRET-0042")
            (backend (make-instance 'recording-backend))
            (env (make-local-environment (hello-task) :directory dir :backend backend)))
       (start-environment env)
       (let ((r (environment-exec env "printf %s \"$OPENAI_API_KEY\""
                                  :env (list (cons "OPENAI_API_KEY" secret)))))
         (ok (equal secret (exec-result-stdout r)) "the command sees the value"))
       (ok (plusp (length (recorded-specs backend))))
       (ok (every (lambda (spec)
                    (notany (lambda (word) (search secret (princ-to-string word)))
                            (compute:sandbox-spec-command spec)))
                  (recorded-specs backend))
           "no argv the environment built holds the secret")
       (ok (notany #'compute:sandbox-spec-env (recorded-specs backend)))
       (ok (null (directory (merge-pathnames "exec/*.*" (truename dir))))
           "the env file is gone after the command")))))
