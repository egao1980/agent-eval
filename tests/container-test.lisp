(in-package #:agent-eval/tests)

;;; Spec for src/podman/environment.lisp (system agent-eval/podman).
;;;
;;; Unit tests (no podman): the image plan, the session and exec specs as
;;; compute-backend-podman turns them into argv, the upload plan.
;;;
;;; The live container lane: GATED. It runs only when podman answers
;;; `podman info` (PODMAN-BACKEND-READY-P) and the test image is present
;;; locally (no pull here: CI pulls it first). Otherwise every live test is
;;; skipped with the reason, unless AGENT_EVAL_PODMAN_LIVE=require (CI's
;;; test-podman-live job), which turns the reason into a failure.
;;; AGENT_EVAL_TEST_IMAGE overrides the image (default: hello-world's own).
;;; AGENT_EVAL_TB2_TASK names a Terminal-Bench 2 task directory (CI fetches
;;; harbor-framework/terminal-bench-2's regex-log at a pinned commit) run end
;;; to end with the oracle agent, its image built from environment/Dockerfile.

(defun test-image ()
  (or (uiop:getenv "AGENT_EVAL_TEST_IMAGE")
      "docker.io/library/debian:bookworm-slim"))

(defun live-required-p ()
  (equal (uiop:getenv "AGENT_EVAL_PODMAN_LIVE") "require"))

(defun podman-exit-code (&rest args)
  (ignore-errors
   (compute:sandbox-result-exit-code
    (compute:run-sandboxed (compute:make-native-process-backend)
                           (compute:make-sandbox-spec :command (cons "podman" args)
                                                      :wall-clock 120)))))

(defvar *container-blocker* :unknown)

(defun container-lane-blocker ()
  "NIL when the lane can run, else why not."
  (when (eq *container-blocker* :unknown)
    (setf *container-blocker*
          (cond ((not (uiop:os-unix-p)) "not a Unix host")
                ((not (agent-eval/podman:podman-backend-ready-p))
                 "podman info failed (no podman, machine or service)")
                ((not (eql 0 (podman-exit-code "image" "exists" (test-image))))
                 (format nil "image ~a is not present locally" (test-image)))
                (t nil))))
  *container-blocker*)

(defmacro container-lane (&body body)
  `(let ((blocker (container-lane-blocker)))
     (cond ((null blocker) ,@body)
           ((live-required-p)
            (ok nil (format nil "container lane required but blocked: ~a" blocker)))
           (t (skip (format nil "container lane: ~a" blocker))))))

(defun container-factory ()
  (lambda (task dir)
    (agent-eval/podman:make-container-environment task :directory dir
                                                        :image (test-image))))

;;; Unit: planning

(deftest container-image-plan
  (testing "an explicit image wins, no build"
    (multiple-value-bind (image context)
        (agent-eval/podman:container-image-plan (hello-task) :image "x:1")
      (ok (equal "x:1" image))
      (ok (null context))))
  (testing "the task's docker_image when not forced"
    (multiple-value-bind (image context) (agent-eval/podman:container-image-plan (hello-task))
      (ok (equal "docker.io/library/debian:bookworm-slim" image))
      (ok (null context))))
  (testing "force-build: built from environment/, tagged by id and content"
    (multiple-value-bind (image context)
        (agent-eval/podman:container-image-plan (hello-task) :force-build t)
      (ok (equal (format nil "agent-eval/hello-world:~a"
                         (subseq (getf (task-spec (hello-task)) :files-sha256) 0 12))
                 image))
      (ok (equal (uiop:native-namestring
                  (merge-pathnames "environment/" (task-directory (hello-task))))
                 (uiop:native-namestring context)))))
  (with-tmpdir (root)
    (testing "a Dockerfile and no docker_image → built"
      (let ((task (load-tb-task (write-tb-task root "built" :toml "[task]
name = \"t/built\"
" :dockerfile "FROM docker.io/library/debian:bookworm-slim
"))))
        (ok (nth-value 1 (agent-eval/podman:container-image-plan task)))))
    (testing "force-build without a Dockerfile → UNSUPPORTED-TASK"
      (ok (signals (agent-eval/podman:container-image-plan
                    (load-tb-task (write-tb-task root "image-only"))
                    :force-build t)
                   'unsupported-task)))))

(deftest container-session-argv
  (with-tmpdir (dir)
    (let* ((env (agent-eval/podman:make-container-environment (hello-task) :directory dir
                                                                          :image "img:1"))
           (argv (compute-backend-podman:podman-open-argv
                  (agent-eval/podman:container-open-spec env) :name "n")))
      (ok (not (environment-started-p env)) "making it runs no podman")
      (ok (null (stop-environment env)) "stopping an unstarted one is a no-op")
      (ok (equal "img:1" (agent-eval/podman:container-environment-image env)))
      (ok (equal '("podman" "run" "-d" "--rm" "--name" "n") (subseq argv 0 6)))
      (testing "no internet for hello-world → --network none"
        (ok (equal "none" (second (member "--network" argv :test #'equal)))))
      (ok (equal "536870912" (second (member "--memory" argv :test #'equal))) "512 MiB")
      (ok (equal "1" (second (member "--cpus" argv :test #'equal))))
      (testing "the trial's logs/ is mounted :rw at /logs"
        (ok (equal (format nil "~a:/logs:rw"
                           (string-right-trim "/" (uiop:native-namestring
                                                   (merge-pathnames "logs/" (truename dir)))))
                   (second (member "-v" argv :test #'equal)))))
      (ok (equal '("--entrypoint" "sleep" "img:1" "infinity") (last argv 4))))))

(deftest container-exec-argv
  (let ((argv (compute-backend-podman:podman-exec-argv
               "cid" (agent-eval/podman:container-exec-spec
                      '("bash" "-c" "pwd") :directory "/app" :env '(("A" . "1"))
                                           :timeout 5))))
    (ok (equal '("podman" "exec" "-w" "/app" "-e" "A=1" "cid" "bash" "-c" "pwd") argv)
        "the work dir is podman exec -w: no cd wrapper"))
  (ok (= 5 (compute:sandbox-spec-wall-clock
            (agent-eval/podman:container-exec-spec '("true") :timeout 5)))))

(deftest container-copy-to-plan
  (with-tmpdir (dir)
    (write-file dir "tests/test.sh" "true")
    (testing "a directory: its contents into ENV-PATH"
      (multiple-value-bind (mkdir source dest)
          (agent-eval/podman:container-copy-to-plan (path-in dir "tests/") "/tests")
        (ok (equal "/tests" mkdir))
        (ok (equal (format nil "~a/." (string-right-trim "/" (uiop:native-namestring
                                                              (truename (path-in dir "tests/")))))
                   source))
        (ok (equal "/tests" dest))))
    (testing "a file: to ENV-PATH, its parent made"
      (multiple-value-bind (mkdir source dest)
          (agent-eval/podman:container-copy-to-plan (path-in dir "tests/test.sh")
                                                    "/logs/agent/config/settings.json")
        (ok (equal "/logs/agent/config" mkdir))
        (ok (equal (uiop:native-namestring (path-in dir "tests/test.sh")) source))
        (ok (equal "/logs/agent/config/settings.json" dest))))
    (testing "a file to a path ending in /: into that directory"
      (multiple-value-bind (mkdir source dest)
          (agent-eval/podman:container-copy-to-plan (path-in dir "tests/test.sh") "/x/")
        (declare (ignore source))
        (ok (equal "/x" mkdir))
        (ok (equal "/x/test.sh" dest))))))

;;; Live

(deftest container-oracle-passes
  (container-lane
   (let ((trial (run-trial (hello-task) (make-oracle-agent)
                           :environment-factory (container-factory))))
     (ok (eq :completed (trial-status trial)))
     (ok (trial-passed-p trial))
     (ok (search "hello.txt: ok"
                 (uiop:read-file-string
                  (merge-pathnames "verifier/test-output.txt"
                                   (uiop:ensure-directory-pathname (trial-directory trial)))))))))

(deftest container-nop-fails
  (container-lane
   (let ((trial (run-trial (hello-task) (make-nop-agent)
                           :environment-factory (container-factory))))
     (ok (eq :completed (trial-status trial)))
     (ok (not (trial-passed-p trial))))))

(deftest container-state-lives-across-commands
  (container-lane
   (with-tmpdir (dir)
     (let ((env (funcall (container-factory) (hello-task) dir)))
       (start-environment env)
       (unwind-protect
            (progn
              (environment-exec env "echo kept > /tmp/state.txt")
              (ok (equal (format nil "kept~%")
                         (exec-result-stdout (environment-exec env "cat /tmp/state.txt"))))
              (ok (equal (format nil "/app~%") (exec-result-stdout (environment-exec env "pwd"))))
              (testing "network off when the task forbids internet"
                (ok (not (eql 0 (exec-result-exit-code
                                 (environment-exec env "getent hosts example.com" :timeout 10))))))
              (testing "/logs is the trial directory's logs/"
                (environment-exec env "echo x > /logs/agent/mark.txt")
                (ok (probe-file (path-in dir "logs/agent/mark.txt"))))
              (testing "upload and download through podman cp"
                (write-file dir "up/a/b.txt" "deep")
                (upload-path env (path-in dir "up/") "/data")
                (ok (equal "deep" (exec-result-stdout (environment-exec env "cat /data/a/b.txt"))))
                (environment-exec env "mkdir -p /out/s && echo one > /out/s/1.txt")
                (download-path env "/out" (path-in dir "down/"))
                (ok (equal (format nil "one~%") (read-file dir "down/s/1.txt")))
                (download-path env "/out/s/1.txt" (path-in dir "single.txt"))
                (ok (equal (format nil "one~%") (read-file dir "single.txt"))))
              (testing "past the timeout → TIMED-OUT-P"
                (let ((r (environment-exec env "sleep 30" :timeout 2)))
                  (ok (exec-result-timed-out-p r))
                  (ok (< (exec-result-duration-ms r) 20000)))))
         (stop-environment env))
       (ok (not (environment-started-p env)))
       (ok (null (stop-environment env)))))))

(deftest container-run-pass-at-k
  (container-lane
   (let* ((ds (ev:make-eval-dataset :name "one" :cases (list (hello-task))))
          (run (run-agent-eval ds (make-instance 'flaky-agent :pass-on '(1))
                               :attempts 2 :environment-factory (container-factory))))
     (ok (= 1 (run-pass-at-k run 2)))
     (ok (= 1/2 (run-pass-at-k run 1))))))

(deftest container-terminal-bench-2-oracle
  (let ((dir (uiop:getenv "AGENT_EVAL_TB2_TASK")))
    (cond ((and (null dir) (live-required-p))
           (ok nil "AGENT_EVAL_TB2_TASK is required in the live lane"))
          ((null dir) (skip "AGENT_EVAL_TB2_TASK is not set"))
          (t
           (container-lane
            (let* ((task (load-tb-task (uiop:ensure-directory-pathname dir)))
                   (trial (run-trial task (make-oracle-agent)
                                     :environment-factory
                                     (lambda (task dir)
                                       (agent-eval/podman:make-container-environment
                                        task :directory dir :force-build t)))))
              (ok (eq :completed (trial-status trial))
                  (format nil "~a: ~a" (task-id task) (trial-error trial)))
              (ok (= 1 (trial-reward trial)) "the oracle earns reward 1")
              (ok (trial-passed-p trial))))))))
