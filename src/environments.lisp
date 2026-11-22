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

(defgeneric %run-command (environment argv &key directory env timeout)
  (:documentation "Internal: the backend half of ENVIRONMENT-EXEC. Run ARGV (a
list of strings) in environment DIRECTORY with ENV (an alist, already
layered) and TIMEOUT seconds → (values exit-code stdout stderr timed-out-p).
A timeout returns TIMED-OUT-P T, never a condition; a command that cannot be
started signals ENVIRONMENT-ERROR."))

(defmacro with-environment ((var form) &body body)
  "Bind VAR to FORM's environment, started; STOP-ENVIRONMENT on every exit."
  `(let ((,var ,form))
     (unwind-protect (progn (start-environment ,var) ,@body)
       (stop-environment ,var))))

(defgeneric exec-result-exit-code (result))
(defgeneric exec-result-stdout (result))
(defgeneric exec-result-stderr (result))
(defgeneric exec-result-timed-out-p (result))
(defgeneric exec-result-duration-ms (result))

(defclass exec-result ()
  ((exit-code :initarg :exit-code :reader exec-result-exit-code :initform nil)
   (stdout :initarg :stdout :reader exec-result-stdout :initform "")
   (stderr :initarg :stderr :reader exec-result-stderr :initform "")
   (timed-out-p :initarg :timed-out-p :reader exec-result-timed-out-p :initform nil)
   (duration-ms :initarg :duration-ms :reader exec-result-duration-ms :initform 0))
  (:documentation "One command's outcome in a task environment."))

(defmethod print-object ((r exec-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "exit ~a~:[~; timed out~] ~d ms" (exec-result-exit-code r)
            (exec-result-timed-out-p r) (exec-result-duration-ms r))))

(defun %cap-output (text)
  "TEXT, keeping the first +OUTPUT-CAP+ characters and noting the rest."
  (let ((n (length text)))
    (if (<= n +output-cap+)
        text
        (concatenate 'string (subseq text 0 +output-cap+)
                     (format nil "~%[agent-eval: truncated ~d characters]" (- n +output-cap+))))))

(defun %env-name (key)
  (etypecase key
    (string key)
    (symbol (symbol-name key))))

(defun %layer-env (&rest alists)
  "ALISTS merged in order (a later name wins), as (NAME-STRING . VALUE-STRING)."
  (let ((out '()))
    (dolist (alist alists (nreverse out))
      (dolist (pair alist)
        (let ((name (%env-name (car pair)))
              (value (let ((v (cdr pair))) (if (stringp v) v (princ-to-string v)))))
          (setf out (cons (cons name value)
                          (remove name out :key #'car :test #'string=))))))))

(defun %environment-error (environment control &rest args)
  (error 'environment-error
         :task-id (let ((task (environment-task environment)))
                    (and (tb-task-p task) (task-id task)))
         :message (apply #'format nil control args)))

(defun %agent-eval-vars (environment)
  (flet ((p (path) (string-right-trim "/" (environment-command-path environment path))))
    (list (cons "AGENT_EVAL_WORKDIR" (p (task-workdir (environment-task environment))))
          (cons "AGENT_EVAL_LOGS_DIR" (p "/logs"))
          (cons "AGENT_EVAL_TESTS_DIR" (p "/tests")))))

(defmethod environment-exec ((environment task-environment) command &key timeout directory env)
  "The protocol rules every backend shares (see the header); the backend part
is %RUN-COMMAND."
  (unless (environment-started-p environment)
    (%environment-error environment "environment is not started"))
  (let ((argv (etypecase command
                (string (list "bash" "-c" command))
                (cons (unless (every #'stringp command)
                        (%environment-error environment "a command list must hold strings: ~s" command))
                      command)))
        (task (environment-task environment))
        (start (get-internal-real-time)))
    (check-type timeout (or null (real (0))))
    (multiple-value-bind (exit-code stdout stderr timed-out-p)
        (%run-command environment argv
                      :directory (or directory (task-workdir task))
                      :env (%layer-env (task-env task) (%agent-eval-vars environment) env)
                      :timeout timeout)
      (make-instance 'exec-result
                     :exit-code (if timed-out-p nil exit-code)
                     :stdout (%cap-output (or stdout ""))
                     :stderr (%cap-output (or stderr ""))
                     :timed-out-p (and timed-out-p t)
                     :duration-ms (%elapsed-ms start)))))

;;; The local environment

(defclass local-environment (task-environment)
  ((task :initarg :task :reader environment-task)
   (trial-directory :initarg :directory :reader environment-trial-directory)
   (root :initarg :root :reader local-environment-root)
   (backend :initarg :backend :reader %local-backend)
   (started-p :initform nil :accessor environment-started-p)
   (exec-count :initform 0 :accessor %exec-count))
  (:documentation "A trusted, path-mapped host directory: the environment path
/P is ROOT/P. Commands run on a compute-protocol backend (native by
default); nothing is confined."))

(defun make-local-environment (task &key directory backend)
  "A LOCAL-ENVIRONMENT for TASK in trial DIRECTORY (default a fresh one)."
  (task-spec task)
  (let ((dir (if directory
                 (%directory-truename directory)
                 (%fresh-directory "agent-eval-trial"))))
    (make-instance 'local-environment
                   :task task
                   :directory dir
                   :root (merge-pathnames "root/" dir)
                   :backend (or backend (compute:make-native-process-backend)))))

(defgeneric local-environment-root (environment))
(defgeneric local-environment-host-path (environment env-path))

(defmethod local-environment-host-path ((environment local-environment) env-path)
  (check-type env-path string)
  (let* ((rel (string-left-trim "/" env-path))
         (path (uiop:parse-unix-namestring rel)))
    (when (some (lambda (c) (member c '(:up :back ".." "..") :test #'equal))
                (rest (pathname-directory path)))
      (%environment-error environment "environment path ~s leaves the root" env-path))
    (when (equal (file-namestring path) "..")
      (%environment-error environment "environment path ~s leaves the root" env-path))
    (merge-pathnames path (local-environment-root environment))))

(defmethod environment-command-path ((environment local-environment) env-path)
  (uiop:native-namestring (local-environment-host-path environment env-path)))

(defun %host-directory (pathname)
  (uiop:ensure-directory-pathname pathname))

(defmethod start-environment ((environment local-environment))
  (unless (environment-started-p environment)
    (handler-case
        (dolist (p (list "/" (task-workdir (environment-task environment))
                         "/logs/agent/" "/logs/verifier/"))
          (ensure-directories-exist
           (%host-directory (local-environment-host-path environment p))))
      (file-error (c)
        (error 'environment-error :task-id (task-id (environment-task environment))
                                  :phase :environment-setup
                                  :message (princ-to-string c))))
    (setf (environment-started-p environment) t))
  environment)

(defmethod stop-environment ((environment local-environment))
  (setf (environment-started-p environment) nil)
  nil)

(defun %sandboxed (environment backend spec)
  "COMPUTE:RUN-SANDBOXED BACKEND SPEC → the SANDBOX-RESULT, or NIL when the
wall clock ran out (ABORT-EXECUTION, no extension). Other compute errors →
ENVIRONMENT-ERROR."
  (handler-case
      (handler-bind ((compute:sandbox-timeout
                       (lambda (c)
                         (let ((r (find-restart 'compute:abort-execution c)))
                           (when r (invoke-restart r))))))
        (compute:run-sandboxed backend spec))
    (compute:compute-error (c)
      (%environment-error environment "~a" c))))

(defparameter +local-wrapper+
  "cd \"$1\" || exit 125; o=$2; e=$3; p=$4; shift 4
while [ $# -gt 0 ] && [ \"$1\" != -- ]; do export \"$1\" || exit 125; shift; done; shift
set -m
\"$@\" >\"$o\" 2>\"$e\" </dev/null &
c=$!
printf '%s\\n' \"$c\" >\"$p\"
wait \"$c\""
  "bash -c script run by the local environment: cd to $1, export the K=V
arguments before --, then run the argv after -- in its own process group
(set -m) with stdout / stderr to the files $2 / $3, writing its pid (= its
process group) to $4.")

(defun %kill-process-group (environment pgid-file)
  "Kill the process group named in PGID-FILE (a timed-out command and its
children), through compute-protocol."
  (let ((pgid (string-trim '(#\Space #\Newline #\Return) (%read-text-file pgid-file))))
    (when (and (plusp (length pgid)) (every #'digit-char-p pgid))
      (handler-case
          (compute:run-sandboxed (%local-backend environment)
                                 (compute:make-sandbox-spec
                                  :command (list "sh" "-c" "kill -KILL -- -\"$1\" 2>/dev/null; exit 0"
                                                 "agent-eval" pgid)
                                  :wall-clock 30))
        (compute:compute-error () nil)))))

(defmethod %run-command ((environment local-environment) argv &key directory env timeout)
  (let ((cwd (%host-directory (local-environment-host-path environment directory))))
    (unless (uiop:directory-exists-p cwd)
      (%environment-error environment "no directory ~a in the environment" directory))
    (let* ((n (incf (%exec-count environment)))
           (base (merge-pathnames "exec/" (environment-trial-directory environment)))
           (out (merge-pathnames (format nil "~d.out" n) base))
           (err (merge-pathnames (format nil "~d.err" n) base))
           (pid (merge-pathnames (format nil "~d.pid" n) base)))
      (ensure-directories-exist base)
      (unwind-protect
           (let* ((spec (compute:make-sandbox-spec
                         :command (list* "bash" "-c" +local-wrapper+ "agent-eval"
                                         (uiop:native-namestring cwd)
                                         (uiop:native-namestring out)
                                         (uiop:native-namestring err)
                                         (uiop:native-namestring pid)
                                         (append (mapcar (lambda (pair)
                                                           (format nil "~a=~a" (car pair) (cdr pair)))
                                                         env)
                                                 (list "--")
                                                 argv))
                         :network (if (task-allow-internet-p (environment-task environment))
                                      :allow :none)
                         :wall-clock timeout))
                  (result (%sandboxed environment (%local-backend environment) spec)))
             (cond (result
                    (values (compute:sandbox-result-exit-code result)
                            (%read-text-file out) (%read-text-file err) nil))
                   (timeout
                    (%kill-process-group environment pid)
                    (values nil (%read-text-file out) (%read-text-file err) t))
                   (t (%environment-error environment "the command was aborted: ~s" argv))))
        (dolist (f (list out err pid))
          (uiop:delete-file-if-exists f))))))

(defun %copy-on-backend (environment from to)
  "Copy host FROM (a file, or a directory's contents) to host TO through
compute-protocol, keeping modes."
  (let* ((dir-p (uiop:directory-exists-p (%host-directory from)))
         (file-p (and (not dir-p) (probe-file from))))
    (unless (or dir-p file-p)
      (%environment-error environment "nothing to copy at ~a" (uiop:native-namestring from)))
    (let* ((src (uiop:native-namestring (if dir-p (%host-directory from) from)))
           (dst (uiop:native-namestring (if dir-p (%host-directory to) to)))
           (script (if dir-p
                       "mkdir -p \"$2\" && cp -pR \"$1\"/. \"$2\""
                       "mkdir -p \"$(dirname \"$2\")\" && cp -p \"$1\" \"$2\""))
           (result (%sandboxed environment (%local-backend environment)
                               (compute:make-sandbox-spec
                                :command (list "sh" "-c" script "agent-eval"
                                               (string-right-trim "/" src)
                                               (if dir-p (string-right-trim "/" dst) dst))))))
      (unless (and result (eql 0 (compute:sandbox-result-exit-code result)))
        (%environment-error environment "copying ~a to ~a failed~@[: ~a~]" src dst
                            (and result (compute:sandbox-result-stderr result))))
      nil)))

(defmethod upload-path ((environment local-environment) host-path env-path)
  (let ((to (local-environment-host-path environment env-path)))
    (%copy-on-backend environment (pathname host-path)
                      (if (and (uiop:directory-pathname-p to)
                               (not (uiop:directory-exists-p (%host-directory host-path))))
                          (merge-pathnames (file-namestring host-path) to)
                          to))))

(defmethod download-path ((environment local-environment) env-path host-path)
  (let ((from (local-environment-host-path environment env-path))
        (to (pathname host-path)))
    (%copy-on-backend environment from
                      (if (and (uiop:directory-pathname-p to)
                               (not (uiop:directory-exists-p (%host-directory from))))
                          (merge-pathnames (file-namestring from) to)
                          to))))
