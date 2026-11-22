(in-package #:agent-eval)

;;; Terminal-Bench tasks (E0). Spec: tests/tasks-test.lisp.
;;;
;;; THE FORMAT (Terminal-Bench 2.x / Harbor task directories, e.g.
;;; harbor-framework/terminal-bench-2 at 2fd12b88):
;;;   task.toml               required; TOML (decoded with toml-protocol)
;;;   instruction.md          required; the prompt, at least one non-blank char
;;;   environment/Dockerfile  the image recipe (WORKDIR sets the work dir)
;;;   environment/docker-compose.yaml   multi-container tasks (unsupported in E0)
;;;   tests/test.sh           required; the verifier; writes
;;;                           /logs/verifier/reward.txt or reward.json
;;;   solution/solve.sh       optional; the reference solution (ORACLE-AGENT)
;;; task.toml keys read (all optional; defaults in brackets):
;;;   [task] name [the id]  keywords
;;;   [metadata] difficulty category tags expert_time_estimate_min
;;;   [agent] timeout_sec [600]       [verifier] timeout_sec [600]
;;;   [environment] docker_image  build_timeout_sec [600]  cpus [1]
;;;     memory_mb [2048]  storage_mb [10240]  gpus [0]  allow_internet [true]
;;;   [environment.env] [verifier.env]   string tables → alists, sorted by key
;;; Other keys are ignored.
;;;
;;; LOAD-TB-TASK DIRECTORY &key ID → an EVAL-PROTOCOL:EVAL-CASE (a plain one;
;;; eval-protocol copies cases as plain EVAL-CASEs) made by EV:MAKE-EVAL-CASE:
;;;   :input    the SPEC, a plist with exactly these keys in this order:
;;;     (:id :name :instruction :directory :files-sha256 :image :dockerfile-p
;;;      :compose-p :workdir :agent-timeout :verifier-timeout :build-timeout
;;;      :cpus :memory-mb :storage-mb :gpus :allow-internet :env :verifier-env
;;;      :solution-p :difficulty :category :tags)
;;;   :metadata (:kind :tb-task :id ID :files-sha256 HEX)
;;;   :source   :terminal-bench
;;; Values: ID — given, else DIRECTORY's last component; 1–128 chars, the
;;; first alphanumeric, the rest alphanumeric or . _ - (else TASK-FORMAT-ERROR
;;; :ID). INSTRUCTION is instruction.md's text as read (UTF-8), not trimmed.
;;; DIRECTORY is the task directory's native namestring (a directory, ending
;;; in the separator). IMAGE is docker_image or NIL. DOCKERFILE-P / COMPOSE-P /
;;; SOLUTION-P say whether those files exist. WORKDIR is the argument of the
;;; Dockerfile's last WORKDIR line when it is absolute, else +DEFAULT-WORKDIR+.
;;; Timeouts are positive reals in seconds as given (else TASK-FORMAT-ERROR
;;; :TIMEOUT). ALLOW-INTERNET is T or NIL. TAGS is a list of strings.
;;; DIFFICULTY and CATEGORY are strings or NIL.
;;; Errors, each TASK-FORMAT-ERROR with PATH = the directory's namestring:
;;;   no task.toml or a TOML parse error → :TASK-TOML; no or blank
;;;   instruction.md → :INSTRUCTION; neither docker_image nor
;;;   environment/Dockerfile nor docker-compose.yaml → :ENVIRONMENT; no
;;;   tests/test.sh → :TESTS; a bad id → :ID; a bad timeout → :TIMEOUT.
;;;
;;; FILES-SHA256: lowercase hex SHA-256 (crypto-protocol DIGEST) over every
;;; regular file under the task directory except .git/, in order of relative
;;; path with / separators (STRING<): for each file the path's UTF-8 octets,
;;; one 0 octet, the file's length in decimal ASCII, one 0 octet, its octets.
;;; It depends on content only, never on where the checkout lives.
;;; TASK-FILES-SHA256 DIRECTORY computes it.
;;;
;;; LOAD-TB-DATASET DIRECTORY &key NAME INCLUDE EXCLUDE → EV:EVAL-DATASET.
;;; Tasks are the immediate subdirectories holding a task.toml, by name
;;; (STRING<). INCLUDE (a list of ids) keeps only those; EXCLUDE drops those.
;;; NAME defaults to DIRECTORY's last component; provenance is
;;; ((:terminal-bench DIRECTORY-NAMESTRING)). A task that fails to load
;;; signals its TASK-FORMAT-ERROR with restarts SKIP-TASK (leave it out) and
;;; USE-VALUE (an eval-case to use instead). No task left → TASK-FORMAT-ERROR
;;; :TASKS. DATASET-DIGEST DATASET → lowercase hex SHA-256 of the lines
;;; "ID:FILES-SHA256\n" in id order; equal for the same tasks anywhere.
;;;
;;; TB-TASK-P X: an EVAL-CASE whose metadata :KIND is :TB-TASK. TASK-SPEC
;;; TASK: its input, or TASK-FORMAT-ERROR :TASK when it is not a task. The
;;; TASK-* readers read the spec (TASK-ALLOW-INTERNET-P reads
;;; :ALLOW-INTERNET). FIND-TASK DATASET ID → that task or NIL.

(defparameter +default-workdir+ "/app"
  "The work directory when the Dockerfile has no absolute WORKDIR.")

(defun load-tb-task (directory &key id)
  (declare (ignore directory id))
  (%e0-pending 'load-tb-task))

(defun load-tb-dataset (directory &key name include exclude)
  (declare (ignore directory name include exclude))
  (%e0-pending 'load-tb-dataset))

(defun task-files-sha256 (directory)
  (declare (ignore directory))
  (%e0-pending 'task-files-sha256))

(defun dataset-digest (dataset)
  (declare (ignore dataset))
  (%e0-pending 'dataset-digest))

(defun tb-task-p (x)
  (declare (ignore x))
  (%e0-pending 'tb-task-p))

(defun task-spec (task)
  (declare (ignore task))
  (%e0-pending 'task-spec))

(defun find-task (dataset id)
  (declare (ignore dataset id))
  (%e0-pending 'find-task))

(macrolet ((readers (&rest names)
             `(progn
                ,@(loop for name in names
                        collect `(defun ,name (task)
                                   (declare (ignore task))
                                   (%e0-pending ',name))))))
  (readers task-id task-name task-instruction task-directory task-image
           task-dockerfile-p task-compose-p task-workdir task-agent-timeout
           task-verifier-timeout task-build-timeout task-cpus task-memory-mb
           task-storage-mb task-gpus task-allow-internet-p task-env
           task-verifier-env task-solution-p task-difficulty task-category
           task-tags))
