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

(defun %format-error (path field control &rest args)
  (error 'task-format-error :path path :field field
                            :message (apply #'format nil control args)))

;;; files-sha256

(defun %task-files (root)
  "(REL . PATHNAME) for every regular file under directory ROOT (a
truename) except .git/, sorted by REL (STRING<)."
  (let ((files '()))
    (dolist (p (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                          :name :wild :type :wild)
                                           root)
                          #+sbcl :resolve-symlinks #+sbcl nil))
      (when (and (uiop:file-pathname-p p)
                 (not (uiop:directory-exists-p p)))
        (let ((rel (uiop:unix-namestring (uiop:enough-pathname p root))))
          (unless (or (string= rel ".git") (eql 0 (search ".git/" rel)))
            (push (cons rel p) files)))))
    (sort files #'string< :key #'car)))

(defun task-files-sha256 (directory)
  "Lowercase hex SHA-256 over DIRECTORY's regular files (not .git/): per file,
its relative path (UTF-8), 0, its length in decimal, 0, its octets."
  (let* ((root (truename (uiop:ensure-directory-pathname directory)))
         (zero (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0))
         (parts '()))
    (loop for (rel . path) in (%task-files root)
          for octets = (%read-octets path)
          do (push (%utf8 rel) parts)
             (push zero parts)
             (push (%utf8 (princ-to-string (length octets))) parts)
             (push zero parts)
             (push octets parts))
    (%sha256-hex (%concatenate-octets (nreverse parts)))))

;;; task.toml

(defun %toml-get (table &rest keys)
  "The value at KEYS in nested TOML TABLE → (values value present-p)."
  (let ((v table))
    (dolist (k keys (values v t))
      (unless (hash-table-p v) (return (values nil nil)))
      (multiple-value-bind (x present) (gethash k v)
        (unless present (return (values nil nil)))
        (setf v x)))))

(defun %toml-string (table &rest keys)
  (let ((v (apply #'%toml-get table keys)))
    (and (stringp v) v)))

(defun %toml-env (table &rest keys)
  "A TOML string table → an alist (NAME . VALUE) sorted by NAME."
  (let ((v (apply #'%toml-get table keys))
        (alist '()))
    (when (hash-table-p v)
      (maphash (lambda (k x)
                 (push (cons k (if (stringp x) x (princ-to-string x))) alist))
               v))
    (sort alist #'string< :key #'car)))

(defun %toml-timeout (table path section key default)
  (multiple-value-bind (v present) (%toml-get table section key)
    (cond ((not present) default)
          ((and (realp v) (plusp v)) v)
          (t (%format-error path :timeout "[~a] ~a must be a positive number of seconds, not ~s"
                            section key v)))))

(defun %toml-number (table section key default)
  (multiple-value-bind (v present) (%toml-get table section key)
    (if (and present (realp v)) v default)))

(defun %read-task-toml (dir path)
  (let ((file (merge-pathnames "task.toml" dir)))
    (unless (probe-file file)
      (%format-error path :task-toml "no task.toml"))
    (handler-case (let ((table (toml:decode file)))
                    (unless (hash-table-p table)
                      (%format-error path :task-toml "task.toml is not a table"))
                    table)
      (toml:toml-error (c)
        (%format-error path :task-toml "task.toml does not parse: ~a" c)))))

;;; Dockerfile

(defun %dockerfile-workdir (dockerfile)
  "The argument of DOCKERFILE's last WORKDIR line when absolute, else NIL."
  (let ((last nil))
    (with-open-file (s dockerfile :external-format (%text-external-format))
      (loop for line = (read-line s nil nil)
            while line
            do (let* ((l (string-trim '(#\Space #\Tab #\Return) line))
                      (sp (position-if (lambda (c) (member c '(#\Space #\Tab))) l)))
                 (when (and sp (string-equal "WORKDIR" (subseq l 0 sp)))
                   (setf last (string-trim "\"' 	" (subseq l sp)))))))
    (and last (plusp (length last)) (char= #\/ (char last 0)) last)))

;;; ids

(defun %ascii-alnum-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9)))

(defun %valid-id-p (id)
  (and (stringp id)
       (<= 1 (length id) 128)
       (%ascii-alnum-p (char id 0))
       (every (lambda (c) (or (%ascii-alnum-p c) (find c "._-"))) id)))

(defun %directory-name (dir)
  (let ((last (car (last (pathname-directory dir)))))
    (if (stringp last) last "")))

;;; the loader

(defun load-tb-task (directory &key id)
  "The Terminal-Bench task in DIRECTORY as a plain EV:EVAL-CASE (see the header)."
  (let* ((given (uiop:ensure-directory-pathname directory))
         (dir (or (probe-file given) given))
         (path (uiop:native-namestring dir)))
    (unless (uiop:directory-exists-p dir)
      (%format-error path :task-toml "not a directory"))
    (let* ((table (%read-task-toml dir path))
           (id (or id (%directory-name dir)))
           (instruction-file (merge-pathnames "instruction.md" dir))
           (instruction (if (probe-file instruction-file)
                            (uiop:read-file-string instruction-file :external-format :utf-8)
                            (%format-error path :instruction "no instruction.md")))
           (dockerfile (probe-file (merge-pathnames "environment/Dockerfile" dir)))
           (compose (or (probe-file (merge-pathnames "environment/docker-compose.yaml" dir))
                        (probe-file (merge-pathnames "environment/docker-compose.yml" dir))))
           (image (%toml-string table "environment" "docker_image")))
      (when (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))) instruction)
        (%format-error path :instruction "instruction.md is blank"))
      (unless (or image dockerfile compose)
        (%format-error path :environment "neither docker_image nor environment/Dockerfile nor docker-compose.yaml"))
      (unless (probe-file (merge-pathnames "tests/test.sh" dir))
        (%format-error path :tests "no tests/test.sh"))
      (unless (%valid-id-p id)
        (%format-error path :id "bad task id ~s" id))
      (let* ((sha (task-files-sha256 dir))
             (tags (%toml-get table "metadata" "tags"))
             (spec (list :id id
                         :name (or (%toml-string table "task" "name") id)
                         :instruction instruction
                         :directory path
                         :files-sha256 sha
                         :image image
                         :dockerfile-p (and dockerfile t)
                         :compose-p (and compose t)
                         :workdir (or (and dockerfile (%dockerfile-workdir dockerfile))
                                      +default-workdir+)
                         :agent-timeout (%toml-timeout table path "agent" "timeout_sec" 600)
                         :verifier-timeout (%toml-timeout table path "verifier" "timeout_sec" 600)
                         :build-timeout (%toml-timeout table path "environment" "build_timeout_sec" 600)
                         :cpus (%toml-number table "environment" "cpus" 1)
                         :memory-mb (%toml-number table "environment" "memory_mb" 2048)
                         :storage-mb (%toml-number table "environment" "storage_mb" 10240)
                         :gpus (%toml-number table "environment" "gpus" 0)
                         :allow-internet (multiple-value-bind (v present)
                                             (%toml-get table "environment" "allow_internet")
                                           (if present (and v t) t))
                         :env (%toml-env table "environment" "env")
                         :verifier-env (%toml-env table "verifier" "env")
                         :solution-p (and (probe-file (merge-pathnames "solution/solve.sh" dir)) t)
                         :difficulty (%toml-string table "metadata" "difficulty")
                         :category (%toml-string table "metadata" "category")
                         :tags (and (or (vectorp tags) (listp tags)) (not (stringp tags))
                                    (remove-if-not #'stringp (coerce tags 'list))))))
        (ev:make-eval-case :input spec
                           :metadata (list :kind :tb-task :id id :files-sha256 sha)
                           :source :terminal-bench)))))

(defun load-tb-dataset (directory &key name include exclude)
  "The tasks in DIRECTORY's immediate subdirectories as an EV:EVAL-DATASET.
A task that fails to load signals its TASK-FORMAT-ERROR with restarts
SKIP-TASK and USE-VALUE."
  (let* ((given (uiop:ensure-directory-pathname directory))
         (dir (or (probe-file given) given))
         (path (uiop:native-namestring dir))
         (subdirs (sort (remove-if-not (lambda (d) (probe-file (merge-pathnames "task.toml" d)))
                                       (and (uiop:directory-exists-p dir)
                                            (uiop:subdirectories dir)))
                        #'string< :key #'%directory-name))
         (subdirs (remove-if-not (lambda (d)
                                   (let ((id (%directory-name d)))
                                     (and (or (null include) (member id include :test #'string=))
                                          (not (member id exclude :test #'string=)))))
                                 subdirs))
         (tasks (loop for d in subdirs
                      for task = (restart-case (load-tb-task d)
                                   (skip-task ()
                                     :report "Leave this task out of the dataset."
                                     nil)
                                   (use-value (value)
                                     :report "Use another task (an eval-case) instead."
                                     :interactive (lambda ()
                                                    (format *query-io* "Task (evaluated): ")
                                                    (force-output *query-io*)
                                                    (list (eval (read *query-io*))))
                                     value))
                      when task collect task)))
    (unless tasks
      (%format-error path :tasks "no tasks in ~a" path))
    (ev:make-eval-dataset :name (or name (%directory-name dir))
                          :cases tasks
                          :provenance (list (list :terminal-bench path)))))

(defun dataset-digest (dataset)
  "Lowercase hex SHA-256 of the lines \"ID:FILES-SHA256\\n\" in id order."
  (let ((lines (sort (mapcar (lambda (task)
                               (format nil "~a:~a~%" (task-id task)
                                       (getf (task-spec task) :files-sha256)))
                             (ev:eval-dataset-cases dataset))
                     #'string<)))
    (%sha256-hex (%utf8 (apply #'concatenate 'string lines)))))

(defun tb-task-p (x)
  "True when X is an EVAL-CASE whose metadata :KIND is :TB-TASK."
  (and (ev:eval-case-p x)
       (eq :tb-task (%plist-get (ev:eval-case-metadata x) :kind))))

(defun task-spec (task)
  "TASK's spec plist, or TASK-FORMAT-ERROR :TASK when it is not a task."
  (if (tb-task-p task)
      (ev:eval-case-input task)
      (error 'task-format-error :field :task
                                :message (format nil "not a Terminal-Bench task: ~s" task))))

(defun find-task (dataset id)
  "DATASET's task whose id is ID, or NIL."
  (find-if (lambda (c) (and (tb-task-p c) (equal id (task-id c))))
           (ev:eval-dataset-cases dataset)))

(macrolet ((readers (&rest pairs)
             `(progn
                ,@(loop for (name key) on pairs by #'cddr
                        collect `(defun ,name (task)
                                   ,(format nil "TASK's spec ~s." key)
                                   (getf (task-spec task) ,key))))))
  (readers task-id :id  task-name :name  task-instruction :instruction
           task-directory :directory  task-image :image
           task-dockerfile-p :dockerfile-p  task-compose-p :compose-p
           task-workdir :workdir  task-agent-timeout :agent-timeout
           task-verifier-timeout :verifier-timeout  task-build-timeout :build-timeout
           task-cpus :cpus  task-memory-mb :memory-mb  task-storage-mb :storage-mb
           task-gpus :gpus  task-allow-internet-p :allow-internet  task-env :env
           task-verifier-env :verifier-env  task-solution-p :solution-p
           task-difficulty :difficulty  task-category :category  task-tags :tags))
