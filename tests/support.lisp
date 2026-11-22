(in-package #:agent-eval/tests)

;;; Test support (complete code, not stubs): the fixture tasks, temporary
;;; directories, a task writer for format variants, lane gates, synthetic
;;; trials and runs. Nothing here reads the network or ~/.config.

(defparameter *fixtures*
  (asdf:system-relative-pathname "agent-eval" "tests/fixtures/"))

(defun fixture (rel)
  (merge-pathnames (uiop:parse-unix-namestring rel) *fixtures*))

(defun tb-fixture (name)
  (uiop:ensure-directory-pathname (fixture (format nil "tb/~a" name))))

(defun fake-cl-stack-code-command ()
  (list "sh" (uiop:native-namestring (fixture "bin/fake-cl-stack-code"))))

;;; Directories and files

(defun random-hex (n)
  (with-output-to-string (s)
    (dotimes (i n) (format s "~(~x~)" (random 16 (make-random-state t))))))

(defun make-tmpdir (&optional (prefix "agent-eval-test"))
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "~a-~a" prefix (random-hex 12))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (truename dir)))

(defmacro with-tmpdir ((var &optional (prefix "agent-eval-test")) &body body)
  `(let ((,var (make-tmpdir ,prefix)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,var :validate t
                                                       :if-does-not-exist :ignore)))))

(defun path-in (dir rel)
  (merge-pathnames (uiop:parse-unix-namestring rel) (uiop:ensure-directory-pathname dir)))

(defun write-file (dir rel text)
  (let ((p (path-in dir rel)))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text s))
    p))

(defun read-file (dir rel)
  (let ((p (path-in dir rel)))
    (and (probe-file p) (uiop:read-file-string p :external-format :utf-8))))

(defun copy-tree-to (from to)
  "Copy directory FROM's files into directory TO (made), recursively."
  (let ((from (truename from)))
    (dolist (file (directory (merge-pathnames "**/*.*" from)))
      (unless (uiop:directory-pathname-p file)
        (let ((dest (merge-pathnames (enough-namestring file from)
                                     (uiop:ensure-directory-pathname to))))
          (ensure-directories-exist dest)
          (uiop:copy-file file dest))))
    to))

(defparameter +minimal-toml+
  "[task]
name = \"t/minimal\"

[environment]
docker_image = \"docker.io/library/debian:bookworm-slim\"
")

(defparameter +reward-txt-script+
  "#!/bin/bash
LOGS=\"${AGENT_EVAL_LOGS_DIR:-/logs}\"
mkdir -p \"$LOGS/verifier\"
echo 1 > \"$LOGS/verifier/reward.txt\"
")

(defun write-tb-task (root name &key (toml +minimal-toml+)
                                     (instruction "Do the thing.
")
                                     dockerfile compose
                                     (test-sh +reward-txt-script+)
                                     solve-sh files)
  "A Terminal-Bench task directory ROOT/NAME/ from parts; a part given as
NIL is left out. FILES: extra (REL . TEXT). → the directory."
  (let ((dir (uiop:ensure-directory-pathname (path-in root name))))
    (ensure-directories-exist dir)
    (when toml (write-file dir "task.toml" toml))
    (when instruction (write-file dir "instruction.md" instruction))
    (when dockerfile (write-file dir "environment/Dockerfile" dockerfile))
    (when compose (write-file dir "environment/docker-compose.yaml" compose))
    (when test-sh (write-file dir "tests/test.sh" test-sh))
    (when solve-sh (write-file dir "solution/solve.sh" solve-sh))
    (loop for (rel . text) in files do (write-file dir rel text))
    dir))

(defun toml-with (&rest sections)
  "A task.toml: +MINIMAL-TOML+ followed by SECTIONS (strings)."
  (format nil "~a~{~%~a~%~}" +minimal-toml+ sections))

;;; Lanes

(defun unix-p ()
  (and (uiop:os-unix-p)
       (zerop (nth-value 2 (uiop:run-program (quote ("sh" "-c" "command -v bash"))
                                             :ignore-error-status t :output nil)))))

(defmacro local-lane (&body body)
  "BODY needs sh and bash on the host (the local environment runs the
fixtures' shell scripts); skipped elsewhere (Windows)."
  `(if (unix-p)
       (progn ,@body)
       (skip "the local environment lane needs sh and bash")))

(defun hello-task ()
  (load-tb-task (tb-fixture "hello-world")))

(defun partial-task ()
  (load-tb-task (tb-fixture "partial-credit")))

(defun fixture-dataset ()
  (load-tb-dataset (uiop:ensure-directory-pathname (fixture "tb/")) :name "fixtures"))

(defun host-file (env path)
  "The text of environment PATH in a local environment, or NIL."
  (let ((p (local-environment-host-path env path)))
    (and (probe-file p) (uiop:read-file-string p))))

(defun json-get (object &rest keys)
  (let ((v object))
    (dolist (k keys v)
      (setf v (and (hash-table-p v) (gethash k v))))))

;;; Agents used by the runner tests (they exercise the adapter protocol).

(defclass flaky-agent (agent-adapter)
  ((pass-on :initarg :pass-on :reader flaky-pass-on)
   (calls :initform 0 :accessor flaky-calls))
  (:documentation "Writes the right hello.txt only on the attempts in
PASS-ON (1-based call count), a wrong one otherwise."))

(defmethod agent-name ((a flaky-agent)) "flaky")

(defmethod agent-run ((a flaky-agent) instruction env context)
  (declare (ignore instruction))
  (let* ((n (incf (flaky-calls a)))
         (text (if (member n (flaky-pass-on a)) "Hello, world!" "nope")))
    (environment-exec env (format nil "printf '%s\\n' '~a' > hello.txt" text))
    (setf (context-input-tokens context) 10
          (context-output-tokens context) 5
          (context-turns context) 1
          (context-status-detail context) "success")))

(defclass exploding-agent (agent-adapter) ()
  (:documentation "AGENT-RUN signals a plain error (not an adapter
condition)."))

(defmethod agent-name ((a exploding-agent)) "exploding")

(defmethod agent-run ((a exploding-agent) instruction env context)
  (declare (ignore instruction env context))
  (error "boom"))

(defclass broken-setup-agent (agent-adapter) ())

(defmethod agent-name ((a broken-setup-agent)) "broken-setup")

(defmethod agent-setup ((a broken-setup-agent) env)
  (declare (ignore env))
  (error "cannot install"))

(defmethod agent-run ((a broken-setup-agent) instruction env context)
  (declare (ignore instruction env context))
  nil)

;;; Synthetic trials and runs (records and comparisons without running).

(defun fake-trial (task-id attempt reward &key (status :completed) (input 100) (output 20)
                                            (cache 0) cost (wall 1000))
  (let ((ctx (make-agent-context)))
    (setf (context-input-tokens ctx) input
          (context-output-tokens ctx) output
          (context-cache-tokens ctx) cache
          (context-cost-usd ctx) cost
          (context-turns ctx) 2
          (context-status-detail ctx) "success"
          (context-transcript ctx) (list (json:decode "{\"type\":\"result\",\"subtype\":\"success\"}")))
    (make-trial :task-id task-id :attempt attempt :agent-name "fake" :status status
                :context ctx :reward reward :passed-p (>= reward 1)
                :timings (list :agent-execution (floor wall 2)) :wall-ms wall
                :started-at "2026-11-22T10:00:00Z" :finished-at "2026-11-22T10:00:01Z")))

(defun fake-run (dataset trials &key (id "run-a") (attempts 1) (agent-name "fake"))
  "An AGENT-EVAL-RUN over DATASET from TRIALS, results built as RUN-AGENT-EVAL does."
  (make-agent-eval-run
   :dataset dataset
   :results (mapcar (lambda (tr)
                      (ev:make-eval-case-result
                       :case (find-task dataset (trial-task-id tr))
                       :actual tr
                       :score (ev:make-eval-score
                               :value (max 0 (min 1 (trial-reward tr)))
                               :verdict (if (trial-passed-p tr) :pass :fail))))
                    trials)
   :trials trials :id id :agent-name agent-name :agent-config (list :name agent-name)
   :attempts attempts :started-at "2026-11-22T10:00:00Z" :finished-at "2026-11-22T10:01:00Z"
   :dataset-digest (dataset-digest dataset)))
