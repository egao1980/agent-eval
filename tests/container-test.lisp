(in-package #:agent-eval/tests)

;;; Spec for src/podman/environment.lisp (system agent-eval/podman): the live
;;; container lane. GATED: it runs only when podman is on PATH and answers
;;; `podman info`, the test image is present locally (no pull: the lane never
;;; touches the network), and agent-eval/podman loads with
;;; compute-backend-podman >= 0.3.0 (egao1980/compute-backend-podman#3).
;;; Otherwise every test here is skipped with the reason.
;;; AGENT_EVAL_TEST_IMAGE overrides the image (default: hello-world's own).

(defun test-image ()
  (or (uiop:getenv "AGENT_EVAL_TEST_IMAGE")
      "docker.io/library/debian:bookworm-slim"))

(defun sh-ok-p (argv)
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program argv :ignore-error-status t
                                               :output nil :error-output nil)))))

(defun podman-sym (name)
  (let ((pkg (find-package "AGENT-EVAL/PODMAN")))
    (and pkg (find-symbol name pkg))))

(defun container-lane-blocker ()
  "NIL when the lane can run, else why not."
  (cond ((not (uiop:os-unix-p)) "not a Unix host")
        ((not (sh-ok-p '("podman" "--version"))) "podman is not on PATH")
        ((not (sh-ok-p '("podman" "info"))) "podman info failed (no machine or service)")
        ((not (sh-ok-p (list "podman" "image" "exists" (test-image))))
         (format nil "image ~a is not present locally" (test-image)))
        ((not (ignore-errors
               (let ((sys (asdf:find-system "compute-backend-podman" nil)))
                 (and sys (uiop:version<= "0.3.0" (asdf:component-version sys))))))
         "compute-backend-podman < 0.3.0 (egao1980/compute-backend-podman#3)")
        ((not (ignore-errors
               (asdf:load-system "agent-eval/podman")
               (funcall (podman-sym "PODMAN-BACKEND-READY-P"))))
         "agent-eval/podman did not load or is not ready")
        (t nil)))

(defmacro container-lane (&body body)
  `(let ((blocker (container-lane-blocker)))
     (if blocker
         (skip (format nil "container lane: ~a" blocker))
         (progn ,@body))))

(defun container-factory ()
  (lambda (task dir)
    (funcall (podman-sym "MAKE-CONTAINER-ENVIRONMENT") task :directory dir
             :image (test-image))))

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
                (ok (probe-file (path-in dir "logs/agent/mark.txt")))))
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
