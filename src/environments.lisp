(in-package #:agent-eval)

;;; Task environments (E0): where a trial's agent and verifier run. Mirrors
;;; Harbor's BaseEnvironment (start / stop / exec / upload / download). Every
;;; command goes through compute-protocol (RUN-SANDBOXED, or a session from
;;; OPEN-SANDBOX). Spec: tests/environment-test.lisp; the container class is
;;; src/podman/environment.lisp (system agent-eval/podman).
;;;
;;; TASK-ENVIRONMENT (given: the protocol class). ENVIRONMENT-TASK → the
;;; task (an eval-case); ENVIRONMENT-TRIAL-DIRECTORY → the trial's host
;;; directory (logs and artifacts); ENVIRONMENT-STARTED-P.
;;;
;;; GFs (paths "inside" the environment are absolute POSIX strings such as
;;; "/app" or "/logs/verifier/reward.txt"):
;;;   START-ENVIRONMENT env → env. Makes the task's work dir, /logs/agent and
;;;     /logs/verifier exist. Starting a started environment does nothing.
;;;     A failure → ENVIRONMENT-ERROR :PHASE :ENVIRONMENT-SETUP.
;;;   STOP-ENVIRONMENT env → NIL. Idempotent; never signals for an
;;;     environment that never started.
;;;   ENVIRONMENT-EXEC env command &key timeout directory env → EXEC-RESULT.
;;;     COMMAND is a string (run as ("bash" "-c" COMMAND), as Harbor does) or
;;;     a list of strings (argv). DIRECTORY is an environment path (default
;;;     the task's work dir). ENV is an alist (NAME . VALUE) layered over the
;;;     task's :ENV and the variables below (later wins). TIMEOUT is seconds
;;;     (NIL: none); past it the command is killed and the result has
;;;     TIMED-OUT-P T and EXIT-CODE NIL; no condition is signalled. A command
;;;     that cannot be started → ENVIRONMENT-ERROR. Not started →
;;;     ENVIRONMENT-ERROR "environment is not started".
;;;     Every command sees AGENT_EVAL_WORKDIR, AGENT_EVAL_LOGS_DIR and
;;;     AGENT_EVAL_TESTS_DIR: the command paths (ENVIRONMENT-COMMAND-PATH) of
;;;     the work dir, /logs and /tests, with no trailing separator.
;;;   UPLOAD-PATH env host-path env-path: a file is copied to ENV-PATH; a
;;;     directory's contents are copied, recursively, into the directory
;;;     ENV-PATH (made when missing). DOWNLOAD-PATH env env-path host-path:
;;;     the reverse.
;;;   ENVIRONMENT-COMMAND-PATH env env-path → the string a command running in
;;;     ENV uses for ENV-PATH (container: ENV-PATH itself).
;;; WITH-ENVIRONMENT (var form) &body: VAR bound to FORM's environment,
;;; started; STOP-ENVIRONMENT on every exit.
;;;
;;; EXEC-RESULT readers: EXIT-CODE (integer or NIL), STDOUT, STDERR (strings,
;;; each capped at +OUTPUT-CAP+ characters: the first +OUTPUT-CAP+ kept, then
;;; "~%[agent-eval: truncated N characters]"), TIMED-OUT-P, DURATION-MS
;;; (integer, measured by agent-eval).
;;;
;;; LOCAL-ENVIRONMENT (trusted, path-mapped, for fixtures and CI). Built by
;;; MAKE-LOCAL-ENVIRONMENT task &key directory backend: DIRECTORY is the
;;; trial directory (default a fresh directory under the temporary
;;; directory); BACKEND a compute-protocol backend (default
;;; (COMPUTE:MAKE-NATIVE-PROCESS-BACKEND)). LOCAL-ENVIRONMENT-ROOT is
;;; DIRECTORY/root/, and the environment path /P maps to ROOT/P
;;; (LOCAL-ENVIRONMENT-HOST-PATH env path → that pathname;
;;; ENVIRONMENT-COMMAND-PATH → its native namestring). The Dockerfile is NOT
;;; built and commands are NOT rewritten: a script that writes to /logs on
;;; the host is not confined. Fixture scripts use $AGENT_EVAL_LOGS_DIR
;;; (falling back to /logs, so the same script runs in a container). Each
;;; command runs through COMPUTE:RUN-SANDBOXED on BACKEND, in the mapped
;;; DIRECTORY, network :ALLOW when the task allows internet else :NONE, with
;;; the spec's wall clock = TIMEOUT; a COMPUTE:SANDBOX-TIMEOUT becomes
;;; TIMED-OUT-P.

(defparameter +output-cap+ 1048576
  "Characters of stdout / stderr an EXEC-RESULT keeps.")

(defclass task-environment ()
  ()
  (:documentation "Protocol class: where a trial's agent and verifier run."))

(defgeneric environment-task (environment))
(defgeneric environment-trial-directory (environment))
(defgeneric environment-started-p (environment))
(defgeneric start-environment (environment))
(defgeneric stop-environment (environment))
(defgeneric environment-exec (environment command &key timeout directory env))
(defgeneric upload-path (environment host-path env-path))
(defgeneric download-path (environment env-path host-path))
(defgeneric environment-command-path (environment env-path))

(defmacro with-environment ((var form) &body body)
  ;; Stub: signals before FORM runs; the expansion keeps BODY so callers compile.
  `(progn (%e0-pending 'with-environment)
          (let ((,var ,form)) ,@body)))

(defgeneric exec-result-exit-code (result))
(defgeneric exec-result-stdout (result))
(defgeneric exec-result-stderr (result))
(defgeneric exec-result-timed-out-p (result))
(defgeneric exec-result-duration-ms (result))

(defclass exec-result () ()
  (:documentation "One command's outcome in a task environment (stub: the
slots and readers are the implementation's)."))

(defclass local-environment (task-environment) ()
  (:documentation "A path-mapped host directory (stub)."))

(defun make-local-environment (task &key directory backend)
  (declare (ignore task directory backend))
  (%e0-pending 'make-local-environment))

(defgeneric local-environment-root (environment))
(defgeneric local-environment-host-path (environment env-path))
