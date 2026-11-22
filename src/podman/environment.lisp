(defpackage #:agent-eval/podman
  (:use #:cl #:agent-eval)
  (:local-nicknames (#:compute #:compute-protocol))
  (:export #:container-environment #:make-container-environment
           #:container-environment-image #:container-environment-backend
           #:+minimum-podman-backend-version+ #:podman-backend-ready-p))

(in-package #:agent-eval/podman)

;;; The container environment (E0, system agent-eval/podman). Terminal-Bench
;;; semantics: ONE container per trial, alive from START-ENVIRONMENT to
;;; STOP-ENVIRONMENT, so what the agent leaves is what the verifier sees.
;;; Needs compute-backend-podman >= 0.3.0 (egao1980/compute-backend-podman#3:
;;; warm sessions on podman exec, :rw mounts, copy in / out, image build;
;;; egao1980/compute-protocol#10: sandbox-spec :directory / :input). Spec:
;;; tests/container-test.lisp (gated: skipped without podman, without the
;;; test image, or without that backend version).
;;;
;;; MAKE-CONTAINER-ENVIRONMENT task &key directory backend image
;;;   DIRECTORY the trial's host directory (as MAKE-LOCAL-ENVIRONMENT).
;;;   BACKEND a compute-protocol backend (default
;;;   (COMPUTE-BACKEND-PODMAN:MAKE-PODMAN-COMPUTE-BACKEND)). IMAGE overrides
;;;   the task's; else the task's :IMAGE; else, with a Dockerfile, the image
;;;   built from the task's environment/ (tag
;;;   "agent-eval/<task-id>:<first 12 of files-sha256>", built once). No image
;;;   and no build → UNSUPPORTED-TASK.
;;; START-ENVIRONMENT: COMPUTE:OPEN-SANDBOX with runtime IMAGE, :cpu-limit
;;;   the task's cpus, :memory-limit memory_mb × 2^20 bytes, network :ALLOW
;;;   when the task allows internet else :NONE, and DIRECTORY/logs/ mounted
;;;   :rw at /logs (so agent and verifier logs land on the host as they are
;;;   written); then makes the work dir, /logs/agent and /logs/verifier.
;;; ENVIRONMENT-EXEC: COMPUTE:RUN-SANDBOXED on the session (podman exec), the
;;;   rules of src/environments.lisp; ENVIRONMENT-COMMAND-PATH is the path
;;;   itself. UPLOAD-PATH / DOWNLOAD-PATH: the backend's copy. STOP: closes the
;;;   session (the container is removed); idempotent.
;;; PODMAN-BACKEND-READY-P → T when the loaded compute-backend-podman's
;;;   version is >= +MINIMUM-PODMAN-BACKEND-VERSION+ and `podman info`
;;;   succeeds; never signals.

(defparameter +minimum-podman-backend-version+ "0.3.0")

(defgeneric container-environment-image (environment))
(defgeneric container-environment-backend (environment))

(defclass container-environment (task-environment)
  ((task :initarg :task :reader environment-task)
   (trial-directory :initarg :directory :reader environment-trial-directory)
   (backend :initarg :backend :reader container-environment-backend)
   (image :initarg :image :reader container-environment-image)
   (session :initform nil :accessor %session))
  (:documentation "A task environment in one podman container, alive from
START-ENVIRONMENT to STOP-ENVIRONMENT (compute-backend-podman >= 0.3.0)."))

(defmethod environment-started-p ((environment container-environment))
  (and (%session environment) t))

(defun %podman-call (name &rest args)
  "Call compute-backend-podman's NAME (an 0.3.0 export, absent from 0.2.0),
so this file compiles against either version."
  (apply #'uiop:symbol-call :compute-backend-podman name args))

(defun %task-image-tag (task)
  (format nil "agent-eval/~(~a~):~a" (task-id task)
          (subseq (getf (task-spec task) :files-sha256) 0 12)))

(defun make-container-environment (task &key directory backend image)
  "A CONTAINER-ENVIRONMENT for TASK (see the header). No image and no
Dockerfile → UNSUPPORTED-TASK."
  (let* ((dir (if directory
                  (agent-eval::%directory-truename directory)
                  (agent-eval::%fresh-directory "agent-eval-trial")))
         (backend (or backend (compute-backend-podman:make-podman-compute-backend)))
         (image (or image
                    (task-image task)
                    (and (task-dockerfile-p task)
                         (%podman-call '#:podman-build-image
                                       (merge-pathnames "environment/"
                                                        (uiop:ensure-directory-pathname
                                                         (task-directory task)))
                                       :tag (%task-image-tag task)))
                    (error 'unsupported-task :task-id (task-id task)
                                             :reason "no image and no Dockerfile"
                                             :message "no image and no Dockerfile"))))
    (make-instance 'container-environment :task task :directory dir
                                          :backend backend :image image)))

(defmethod environment-command-path ((environment container-environment) env-path)
  env-path)

(defmethod start-environment ((environment container-environment))
  (unless (%session environment)
    (let* ((task (environment-task environment))
           (logs (merge-pathnames "logs/" (environment-trial-directory environment))))
      (dolist (sub '("agent/" "verifier/"))
        (ensure-directories-exist (merge-pathnames sub logs)))
      (handler-case
          (setf (%session environment)
                (compute:open-sandbox
                 (container-environment-backend environment)
                 (compute:make-sandbox-spec
                  :runtime (container-environment-image environment)
                  :cpu-limit (task-cpus task)
                  :memory-limit (* (task-memory-mb task) (expt 2 20))
                  :network (if (task-allow-internet-p task) :allow :none)
                  :mounts (list (list (uiop:native-namestring logs) "/logs" :rw)))))
        (compute:compute-error (c)
          (error 'environment-error :task-id (task-id task) :phase :environment-setup
                                    :message (princ-to-string c))))
      (let ((r (environment-exec environment
                                 (list "mkdir" "-p" (task-workdir task) "/logs/agent" "/logs/verifier")
                                 :directory "/")))
        (unless (eql 0 (exec-result-exit-code r))
          (stop-environment environment)
          (error 'environment-error :task-id (task-id task) :phase :environment-setup
                                    :message (exec-result-stderr r))))))
  environment)

(defmethod stop-environment ((environment container-environment))
  (let ((session (%session environment)))
    (when session
      (setf (%session environment) nil)
      (handler-case (compute:close-sandbox session)
        (compute:compute-error (c)
          (error 'environment-error :task-id (task-id (environment-task environment))
                                    :phase :stop :message (princ-to-string c))))))
  nil)

(defmethod agent-eval::%run-command ((environment container-environment) argv
                                     &key directory env timeout)
  (let ((spec (compute:make-sandbox-spec
               ;; compute-protocol has no :directory yet (compute-protocol#10).
               :command (list* "sh" "-c" "cd \"$1\" || exit 125; shift; exec \"$@\""
                               "agent-eval" directory argv)
               :env env
               :wall-clock timeout)))
    (let ((result (handler-case
                      (handler-bind ((compute:sandbox-timeout
                                       (lambda (c)
                                         (let ((r (find-restart 'compute:abort-execution c)))
                                           (when r (invoke-restart r))))))
                        (compute:run-sandboxed (%session environment) spec))
                    (compute:compute-error (c)
                      (error 'environment-error :task-id (task-id (environment-task environment))
                                                :message (princ-to-string c))))))
      (if result
          (values (compute:sandbox-result-exit-code result)
                  (compute:sandbox-result-stdout result)
                  (compute:sandbox-result-stderr result)
                  nil)
          (values nil "" "" t)))))

(defmethod upload-path ((environment container-environment) host-path env-path)
  (%podman-call '#:podman-copy-to (%session environment) host-path env-path))

(defmethod download-path ((environment container-environment) env-path host-path)
  (ensure-directories-exist host-path)
  (%podman-call '#:podman-copy-from (%session environment) env-path host-path))

(defun podman-backend-ready-p ()
  "T when the loaded compute-backend-podman is >= +MINIMUM-PODMAN-BACKEND-VERSION+
and `podman info` succeeds (through compute-protocol). Never signals."
  (handler-case
      (let ((system (asdf:find-system "compute-backend-podman" nil)))
        (and system
             (asdf:component-version system)
             (uiop:version<= +minimum-podman-backend-version+ (asdf:component-version system))
             (let ((r (compute:run-sandboxed (compute:make-native-process-backend)
                                             (compute:make-sandbox-spec
                                              :command '("podman" "info") :wall-clock 60))))
               (and r (eql 0 (compute:sandbox-result-exit-code r))))
             t))
    (error () nil)))
