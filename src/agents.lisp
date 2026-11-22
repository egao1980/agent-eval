(in-package #:agent-eval)

;;; The agent adapter protocol (E0). It mirrors Harbor's BaseAgent
;;; (harbor-framework/harbor 0.26.0, src/harbor/agents/base.py: name(),
;;; version(), setup(environment), run(instruction, environment, context),
;;; populate_context_post_run(context)), so a Harbor agent maps one to one
;;; and a TB-format task means the same thing to both. Spec:
;;; tests/agents-test.lisp; the cl-stack-code adapter is src/cl-stack-code.lisp.
;;;
;;; AGENT-ADAPTER (given: the protocol class; adapters subclass it).
;;;   AGENT-NAME adapter → a string (the adapter's kind: "nop", "oracle",
;;;     "command", "cl-stack-code"; a subclass without a method → its class
;;;     name, downcased).
;;;   AGENT-VERSION adapter → a string or NIL (default NIL).
;;;   AGENT-CONFIG adapter → a plist of the settings that shape a run, pure
;;;     data, stored with every run (default (:name NAME)).
;;;   AGENT-SETUP adapter environment → ignored value; runs before AGENT-RUN
;;;     in the started environment (install, config files). Default: nothing.
;;;   AGENT-RUN adapter instruction environment context → ignored value. Does
;;;     the task; fills CONTEXT as it goes (so a timeout keeps what was
;;;     known). Must signal AGENT-TIMEOUT when the agent passed the task's
;;;     agent timeout, AGENT-FAILED when the agent itself failed. No default
;;;     method for a bare AGENT-ADAPTER: AGENT-EVAL-ERROR.
;;;   AGENT-POPULATE-CONTEXT adapter context → ignored value; after AGENT-RUN
;;;     (also after a timeout or failure), to backfill from logs. Default:
;;;     nothing.
;;;
;;; AGENT-CONTEXT (MAKE-AGENT-CONTEXT &key logs-directory): Harbor's
;;; AgentContext. SETF-able accessors, all NIL until set except TRANSCRIPT
;;; (NIL = empty list) and METADATA (a plist):
;;;   CONTEXT-INPUT-TOKENS   input tokens INCLUDING cache reads and writes
;;;   CONTEXT-CACHE-TOKENS   cache-read tokens
;;;   CONTEXT-OUTPUT-TOKENS  CONTEXT-COST-USD (a real or NIL)
;;;   CONTEXT-TURNS          model calls   CONTEXT-RESULT-TEXT  final text
;;;   CONTEXT-STATUS-DETAIL  the agent's own end state (e.g. "success",
;;;                          "error_max_turns")
;;;   CONTEXT-TRANSCRIPT     the agent's events in order (json-protocol
;;;                          values: string-key hash tables); saved as JSONL
;;;   CONTEXT-METADATA       CONTEXT-LOGS-DIRECTORY (host dir for the
;;;                          adapter's own logs: TRIAL-DIR/agent/)
;;;
;;; Built-in adapters (Harbor's nop and oracle, plus a command agent):
;;;   NOP-AGENT (MAKE-NOP-AGENT): does nothing. Name "nop".
;;;   ORACLE-AGENT (MAKE-ORACLE-AGENT): uploads the task's solution/ to
;;;     /solution and runs ("bash" CMD-PATH-of-/solution/solve.sh) in the
;;;     work dir with the task's agent timeout. No solution →
;;;     TASK-FORMAT-ERROR :SOLUTION; a timeout → AGENT-TIMEOUT; a non-zero
;;;     exit → AGENT-FAILED with the exit code. Name "oracle".
;;;   COMMAND-AGENT (MAKE-COMMAND-AGENT &key command env name): runs COMMAND
;;;     (a non-empty list of strings, else AGENT-EVAL-ERROR) in the work dir
;;;     with the task's agent timeout, ENV plus AGENT_EVAL_INSTRUCTION = the
;;;     instruction. Its stdout and stderr go to logs-directory/command.log;
;;;     CONTEXT-STATUS-DETAIL is "exit N". Timeout → AGENT-TIMEOUT; non-zero
;;;     exit → AGENT-FAILED. Name NAME (default "command"); AGENT-CONFIG
;;;     (:name NAME :command COMMAND).

(defclass agent-adapter ()
  ()
  (:documentation "Protocol class: an agent that can do a task in an environment."))

(defgeneric agent-name (adapter))
(defgeneric agent-version (adapter))
(defgeneric agent-config (adapter))
(defgeneric agent-setup (adapter environment))
(defgeneric agent-run (adapter instruction environment context))
(defgeneric agent-populate-context (adapter context))

(defmethod agent-name ((adapter agent-adapter))
  (string-downcase (symbol-name (class-name (class-of adapter)))))

(defmethod agent-version ((adapter agent-adapter))
  nil)

(defmethod agent-config ((adapter agent-adapter))
  (list :name (agent-name adapter)))

(defmethod agent-setup ((adapter agent-adapter) environment)
  (declare (ignore environment))
  nil)

(defmethod agent-run ((adapter agent-adapter) instruction environment context)
  (declare (ignore instruction environment context))
  (error 'agent-eval-error
         :message (format nil "~a has no AGENT-RUN method" (agent-name adapter))))

(defmethod agent-populate-context ((adapter agent-adapter) context)
  (declare (ignore context))
  nil)

(defgeneric context-input-tokens (context))
(defgeneric context-cache-tokens (context))
(defgeneric context-output-tokens (context))
(defgeneric context-cost-usd (context))
(defgeneric context-turns (context))
(defgeneric context-result-text (context))
(defgeneric context-status-detail (context))
(defgeneric context-transcript (context))
(defgeneric context-metadata (context))
(defgeneric context-logs-directory (context))
(defgeneric (setf context-input-tokens) (value context))
(defgeneric (setf context-cache-tokens) (value context))
(defgeneric (setf context-output-tokens) (value context))
(defgeneric (setf context-cost-usd) (value context))
(defgeneric (setf context-turns) (value context))
(defgeneric (setf context-result-text) (value context))
(defgeneric (setf context-status-detail) (value context))
(defgeneric (setf context-transcript) (value context))
(defgeneric (setf context-metadata) (value context))

(defclass agent-context ()
  ((input-tokens :initform nil :accessor context-input-tokens)
   (cache-tokens :initform nil :accessor context-cache-tokens)
   (output-tokens :initform nil :accessor context-output-tokens)
   (cost-usd :initform nil :accessor context-cost-usd)
   (turns :initform nil :accessor context-turns)
   (result-text :initform nil :accessor context-result-text)
   (status-detail :initform nil :accessor context-status-detail)
   (transcript :initform nil :accessor context-transcript)
   (metadata :initform nil :accessor context-metadata)
   (logs-directory :initarg :logs-directory :initform nil :reader context-logs-directory))
  (:documentation "What an agent run produced (Harbor's AgentContext)."))

(defun make-agent-context (&key logs-directory)
  "A fresh AGENT-CONTEXT whose adapter logs go to LOGS-DIRECTORY (or nowhere)."
  (make-instance 'agent-context
                 :logs-directory (and logs-directory
                                      (uiop:ensure-directory-pathname logs-directory))))

;;; Helpers shared by the built-in adapters

(defun %write-agent-log (context name text)
  "Write TEXT to CONTEXT's logs-directory/NAME when it has a logs directory."
  (let ((dir (context-logs-directory context)))
    (when dir
      (%write-text-file (merge-pathnames name dir) text))))

(defun %agent-verdict (task result)
  "Signal AGENT-TIMEOUT or AGENT-FAILED for an agent command's RESULT."
  (cond ((exec-result-timed-out-p result)
         (error 'agent-timeout :task-id (task-id task) :seconds (task-agent-timeout task)
                               :message "the agent command timed out"))
        ((not (eql 0 (exec-result-exit-code result)))
         (error 'agent-failed :task-id (task-id task) :exit-code (exec-result-exit-code result)
                              :message "the agent command exited non-zero"))))

(defun %output-text (result)
  (concatenate 'string (exec-result-stdout result) (exec-result-stderr result)))

;;; nop

(defclass nop-agent (agent-adapter) ()
  (:documentation "Harbor's nop agent: does nothing."))

(defun make-nop-agent ()
  (make-instance 'nop-agent))

(defmethod agent-name ((agent nop-agent)) "nop")

(defmethod agent-run ((agent nop-agent) instruction environment context)
  (declare (ignore instruction environment context))
  nil)

;;; oracle

(defclass oracle-agent (agent-adapter) ()
  (:documentation "Harbor's oracle agent: runs the task's solution/solve.sh."))

(defun make-oracle-agent ()
  (make-instance 'oracle-agent))

(defmethod agent-name ((agent oracle-agent)) "oracle")

(defmethod agent-run ((agent oracle-agent) instruction environment context)
  (declare (ignore instruction))
  (let ((task (environment-task environment)))
    (unless (task-solution-p task)
      (error 'task-format-error :path (task-directory task) :field :solution
                                :message "no solution/solve.sh"))
    (upload-path environment
                 (merge-pathnames "solution/" (uiop:ensure-directory-pathname (task-directory task)))
                 "/solution")
    (let ((result (environment-exec environment
                                    (list "bash" (environment-command-path environment
                                                                           "/solution/solve.sh"))
                                    :timeout (task-agent-timeout task))))
      (%write-agent-log context "oracle.log" (%output-text result))
      (setf (context-status-detail context)
            (if (exec-result-timed-out-p result)
                "timeout"
                (format nil "exit ~a" (exec-result-exit-code result))))
      (%agent-verdict task result))))

;;; command

(defgeneric command-agent-command (agent))

(defclass command-agent (agent-adapter)
  ((command :initarg :command :reader command-agent-command)
   (env :initarg :env :reader %command-agent-env)
   (name :initarg :name :reader %command-agent-name))
  (:documentation "Runs a fixed command in the work dir with the instruction in
$AGENT_EVAL_INSTRUCTION."))

(defun make-command-agent (&key command env name)
  "A COMMAND-AGENT running COMMAND (a non-empty list of strings)."
  (unless (and (consp command) (every #'stringp command))
    (error 'agent-eval-error
           :message (format nil "a command agent needs a non-empty list of strings, not ~s"
                            command)))
  (unless (or (null name) (stringp name))
    (error 'agent-eval-error :message (format nil "the agent name must be a string, not ~s" name)))
  (make-instance 'command-agent :command (copy-list command) :env (copy-alist env)
                                :name (or name "command")))

(defmethod agent-name ((agent command-agent))
  (%command-agent-name agent))

(defmethod agent-config ((agent command-agent))
  (list :name (agent-name agent) :command (command-agent-command agent)))

(defmethod agent-run ((agent command-agent) instruction environment context)
  (let* ((task (environment-task environment))
         (result (environment-exec environment (command-agent-command agent)
                                   :timeout (task-agent-timeout task)
                                   :env (append (%command-agent-env agent)
                                                (list (cons "AGENT_EVAL_INSTRUCTION" instruction))))))
    (%write-agent-log context "command.log" (%output-text result))
    (setf (context-status-detail context)
          (if (exec-result-timed-out-p result)
              "timeout"
              (format nil "exit ~a" (exec-result-exit-code result))))
    (%agent-verdict task result)))
