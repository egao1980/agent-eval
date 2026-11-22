(defpackage #:agent-eval/podman
  (:use #:cl #:agent-eval)
  (:local-nicknames (#:compute #:compute-protocol)
                    (#:podman #:compute-backend-podman))
  (:export #:container-environment #:make-container-environment
           #:container-environment-image #:container-environment-backend
           #:container-environment-session
           #:container-image-plan #:container-open-spec #:container-exec-spec
           #:container-copy-to-plan
           #:+minimum-podman-backend-version+ #:podman-backend-ready-p))

(in-package #:agent-eval/podman)

;;; The container environment (E0, system agent-eval/podman). Terminal-Bench
;;; semantics: ONE container per trial, a warm compute-protocol session alive
;;; from START-ENVIRONMENT to STOP-ENVIRONMENT, so what the agent leaves is
;;; what the verifier sees. Needs compute-protocol >= 0.4.0 (sandbox-spec
;;; :directory, (src dst :rw) mounts, SANDBOX-COPY-TO / -FROM) and
;;; compute-backend-podman >= 0.3.0 (warm sessions on podman exec, podman cp,
;;; PODMAN-BUILD-IMAGE). Spec: tests/container-test.lisp (unit tests of the
;;; planning functions below, without podman; plus the live lane, gated on
;;; podman, and required in CI's test-podman-live job).
;;;
;;; MAKE-CONTAINER-ENVIRONMENT task &key directory backend image force-build
;;;   DIRECTORY the trial's host directory (as MAKE-LOCAL-ENVIRONMENT).
;;;   BACKEND a compute-backend-podman backend (default
;;;   (PODMAN:MAKE-PODMAN-COMPUTE-BACKEND)). The image is CONTAINER-IMAGE-PLAN's:
;;;   when it names a build context, PODMAN:PODMAN-BUILD-IMAGE builds it
;;;   (BACKEND's podman, timeout the task's build timeout; podman's layer cache
;;;   makes a rebuild cheap). A failed build → ENVIRONMENT-ERROR
;;;   :PHASE :ENVIRONMENT-SETUP.
;;; CONTAINER-IMAGE-PLAN task &key image force-build → (values IMAGE CONTEXT):
;;;   IMAGE given → (IMAGE NIL); FORCE-BUILD or no task :IMAGE, with a
;;;   Dockerfile → (TAG CONTEXT), TAG "agent-eval/<task-id>:<first 12 of
;;;   files-sha256>" and CONTEXT the task's environment/ directory; else the
;;;   task's :IMAGE → (IMAGE NIL); else UNSUPPORTED-TASK (FORCE-BUILD without a
;;;   Dockerfile, or neither image nor Dockerfile).
;;; CONTAINER-OPEN-SPEC environment → the session's SANDBOX-SPEC: runtime the
;;;   image, :cpu-limit the task's cpus, :memory-limit memory_mb × 2^20 bytes,
;;;   network :ALLOW when the task allows internet else :NONE, and
;;;   DIRECTORY/logs/ mounted :rw at /logs (agent and verifier logs land on the
;;;   host as they are written).
;;; CONTAINER-EXEC-SPEC argv &key directory env timeout → the SANDBOX-SPEC of
;;;   one command on the session: :command ("sh" "-c" +EXEC-WRAPPER+
;;;   "agent-eval" ARGV…), :input (ENV-EXPORT-SCRIPT ENV), :directory
;;;   DIRECTORY (podman exec -w), :wall-clock TIMEOUT, no :env. The wrapper
;;;   evals stdin, then execs ARGV with stdin at EOF: env values travel on
;;;   podman exec's stdin, never as `-e K=V` argv words visible in ps
;;;   (agent-eval#5).
;;; CONTAINER-COPY-TO-PLAN host-path env-path → (values MKDIR SOURCE DEST) for
;;;   UPLOAD-PATH: a host directory → (ENV-PATH "<host>/." ENV-PATH), so its
;;;   contents land in ENV-PATH; a file → DEST = ENV-PATH (or ENV-PATH plus the
;;;   file's name when ENV-PATH ends in /), MKDIR its parent.
;;; START-ENVIRONMENT: COMPUTE:OPEN-SANDBOX on BACKEND with CONTAINER-OPEN-SPEC,
;;;   then makes the work dir, /logs/agent and /logs/verifier.
;;; ENVIRONMENT-EXEC: COMPUTE:RUN-SANDBOXED on the session (podman exec), the
;;;   rules of src/environments.lisp; past the wall clock the command is
;;;   aborted (ABORT-EXECUTION) → TIMED-OUT-P. ENVIRONMENT-COMMAND-PATH is the
;;;   path itself.
;;; UPLOAD-PATH: `mkdir -p MKDIR` in the container, then COMPUTE:SANDBOX-COPY-TO
;;;   SOURCE DEST (podman cp). DOWNLOAD-PATH: `test -d ENV-PATH` in the
;;;   container; a directory's contents (ENV-PATH/.) are copied into the host
;;;   directory HOST-PATH, a file to HOST-PATH (or into it when HOST-PATH is a
;;;   directory pathname), through COMPUTE:SANDBOX-COPY-FROM. A failed copy →
;;;   ENVIRONMENT-ERROR.
;;; STOP-ENVIRONMENT: COMPUTE:CLOSE-SANDBOX (podman rm -f); idempotent.
;;; PODMAN-BACKEND-READY-P &optional backend → T when the loaded
;;;   compute-backend-podman's version is >= +MINIMUM-PODMAN-BACKEND-VERSION+
;;;   and PODMAN:PODMAN-AVAILABLE-P (`podman info` succeeds); never signals.

(defparameter +minimum-podman-backend-version+ "0.3.0")

(defgeneric container-environment-image (environment))
(defgeneric container-environment-backend (environment))
(defgeneric container-environment-session (environment))

(defclass container-environment (task-environment)
  ((task :initarg :task :reader environment-task)
   (trial-directory :initarg :directory :reader environment-trial-directory)
   (backend :initarg :backend :reader container-environment-backend)
   (image :initarg :image :reader container-environment-image)
   (session :initform nil :accessor %session :reader container-environment-session))
  (:documentation "A task environment in one podman container (a warm
compute-protocol session), alive from START-ENVIRONMENT to STOP-ENVIRONMENT."))

(defmethod environment-started-p ((environment container-environment))
  (and (%session environment) t))

(defun %task-error (task phase control &rest args)
  (error 'environment-error :task-id (task-id task) :phase phase
                            :message (apply #'format nil control args)))

;;; Planning (pure: no podman)

(defun %task-image-tag (task)
  (format nil "agent-eval/~(~a~):~a" (task-id task)
          (subseq (getf (task-spec task) :files-sha256) 0 12)))

(defun %unsupported (task reason)
  (error 'unsupported-task :task-id (task-id task) :reason reason :message reason))

(defun container-image-plan (task &key image force-build)
  "→ (values IMAGE BUILD-CONTEXT-OR-NIL) for TASK (see the header)."
  (let ((context (merge-pathnames "environment/"
                                  (uiop:ensure-directory-pathname (task-directory task)))))
    (cond (image (values image nil))
          ((and (task-dockerfile-p task) (or force-build (null (task-image task))))
           (values (%task-image-tag task) context))
          (force-build (%unsupported task "force-build needs environment/Dockerfile"))
          ((task-image task) (values (task-image task) nil))
          (t (%unsupported task "no image and no Dockerfile")))))

(defun container-open-spec (environment)
  "The warm session's SANDBOX-SPEC for ENVIRONMENT (see the header)."
  (let ((task (environment-task environment))
        (logs (merge-pathnames "logs/" (environment-trial-directory environment))))
    (compute:make-sandbox-spec
     :runtime (container-environment-image environment)
     :cpu-limit (task-cpus task)
     :memory-limit (* (task-memory-mb task) (expt 2 20))
     :network (if (task-allow-internet-p task) :allow :none)
     :mounts (list (list (uiop:native-namestring logs) "/logs" :rw)))))

(defparameter +exec-wrapper+ "eval \"$(cat)\" || exit 125; exec \"$@\""
  "sh -c script of every container command: export the variables read from
stdin, then exec the argv.")

(defun container-exec-spec (argv &key directory env timeout)
  "The SANDBOX-SPEC of one command on the session (see the header)."
  (compute:make-sandbox-spec :command (list* "sh" "-c" +exec-wrapper+ "agent-eval" argv)
                             :input (env-export-script env)
                             :directory directory
                             :wall-clock timeout))

(defun %parent (env-path)
  (let ((slash (position #\/ (string-right-trim "/" env-path) :from-end t)))
    (if (and slash (plusp slash)) (subseq env-path 0 slash) "/")))

(defun %host-directory-p (host-path)
  (and (uiop:directory-exists-p (uiop:ensure-directory-pathname host-path)) t))

(defun container-copy-to-plan (host-path env-path)
  "→ (values MKDIR SOURCE DEST) for UPLOAD-PATH (see the header)."
  (check-type env-path string)
  (if (%host-directory-p host-path)
      (values env-path
              (concatenate 'string
                           (string-right-trim "/\\" (uiop:native-namestring
                                                     (uiop:ensure-directory-pathname host-path)))
                           "/.")
              env-path)
      (let ((dest (if (and (plusp (length env-path))
                           (char= #\/ (char env-path (1- (length env-path)))))
                      (concatenate 'string env-path (file-namestring host-path))
                      env-path)))
        (values (%parent dest) (uiop:native-namestring host-path) dest))))

;;; The environment

(defun make-container-environment (task &key directory backend image force-build)
  "A CONTAINER-ENVIRONMENT for TASK (see the header)."
  (multiple-value-bind (image context) (container-image-plan task :image image
                                                                  :force-build force-build)
    (let* ((dir (if directory
                    (agent-eval::%directory-truename directory)
                    (agent-eval::%fresh-directory "agent-eval-trial")))
           (backend (or backend (podman:make-podman-compute-backend))))
      (when context
        (handler-case (podman:podman-build-image context :tag image :backend backend
                                                         :timeout (task-build-timeout task))
          (compute:compute-error (c)
            (%task-error task :environment-setup "building ~a: ~a" image c))))
      (make-instance 'container-environment :task task :directory dir
                                            :backend backend :image image))))

(defmethod environment-command-path ((environment container-environment) env-path)
  env-path)

(defmethod start-environment ((environment container-environment))
  (unless (%session environment)
    (let ((task (environment-task environment)))
      (dolist (sub '("logs/agent/" "logs/verifier/"))
        (ensure-directories-exist (merge-pathnames sub (environment-trial-directory environment))))
      (handler-case
          (setf (%session environment)
                (compute:open-sandbox (container-environment-backend environment)
                                      (container-open-spec environment)))
        (compute:compute-error (c)
          (%task-error task :environment-setup "~a" c)))
      (let ((r (environment-exec environment
                                 (list "mkdir" "-p" (task-workdir task) "/logs/agent" "/logs/verifier")
                                 :directory "/")))
        (unless (eql 0 (exec-result-exit-code r))
          (stop-environment environment)
          (%task-error task :environment-setup "~a" (exec-result-stderr r))))))
  environment)

(defmethod stop-environment ((environment container-environment))
  (let ((session (%session environment)))
    (when session
      (setf (%session environment) nil)
      (handler-case (compute:close-sandbox session)
        (compute:compute-error (c)
          (%task-error (environment-task environment) :stop "~a" c)))))
  nil)

(defmethod agent-eval::%run-command ((environment container-environment) argv
                                     &key directory env timeout)
  (let ((result (handler-case
                    (handler-bind ((compute:sandbox-timeout
                                     (lambda (c)
                                       (let ((r (find-restart 'compute:abort-execution c)))
                                         (when r (invoke-restart r))))))
                      (compute:run-sandboxed (%session environment)
                                             (container-exec-spec argv :directory directory
                                                                       :env env :timeout timeout)))
                  (compute:compute-error (c)
                    (%task-error (environment-task environment) nil "~a" c)))))
    (if result
        (values (compute:sandbox-result-exit-code result)
                (compute:sandbox-result-stdout result)
                (compute:sandbox-result-stderr result)
                nil)
        (values nil "" "" t))))

(defun %exec-ok (environment argv what)
  (let ((r (environment-exec environment argv :directory "/")))
    (unless (eql 0 (exec-result-exit-code r))
      (%task-error (environment-task environment) nil "~a failed: ~a" what (exec-result-stderr r)))
    r))

(defmacro %copying ((environment) &body body)
  `(handler-case (progn ,@body)
     (compute:compute-error (c)
       (%task-error (environment-task ,environment) nil "~a" c))))

(defmethod upload-path ((environment container-environment) host-path env-path)
  (multiple-value-bind (mkdir source dest) (container-copy-to-plan host-path env-path)
    (%exec-ok environment (list "mkdir" "-p" mkdir) "mkdir")
    (%copying (environment)
      (compute:sandbox-copy-to (%session environment) source dest))
    nil))

(defmethod download-path ((environment container-environment) env-path host-path)
  (check-type env-path string)
  (let ((dir-p (eql 0 (exec-result-exit-code
                       (environment-exec environment (list "test" "-d" env-path)
                                         :directory "/")))))
    (if dir-p
        (let ((to (uiop:ensure-directory-pathname host-path)))
          (ensure-directories-exist to)
          (%copying (environment)
            (compute:sandbox-copy-from (%session environment)
                                       (concatenate 'string (string-right-trim "/" env-path) "/.")
                                       (uiop:native-namestring to))))
        (let ((to (if (uiop:directory-pathname-p (pathname host-path))
                      (merge-pathnames (file-namestring (string-right-trim "/" env-path))
                                       (pathname host-path))
                      (pathname host-path))))
          (ensure-directories-exist to)
          (%copying (environment)
            (compute:sandbox-copy-from (%session environment) env-path
                                       (uiop:native-namestring to)))))
    nil))

(defun podman-backend-ready-p (&optional backend)
  "T when the loaded compute-backend-podman is >= +MINIMUM-PODMAN-BACKEND-VERSION+
and `podman info` succeeds (PODMAN:PODMAN-AVAILABLE-P). Never signals."
  (handler-case
      (let* ((system (asdf:find-system "compute-backend-podman" nil))
             (version (and system (asdf:component-version system))))
        (and version
             (uiop:version<= +minimum-podman-backend-version+ version)
             (podman:podman-available-p backend)
             t))
    (error () nil)))
