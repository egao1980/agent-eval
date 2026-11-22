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

(defclass agent-context () ()
  (:documentation "What an agent run produced (stub)."))

(defun make-agent-context (&key logs-directory)
  (declare (ignore logs-directory))
  (%e0-pending 'make-agent-context))

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

(defclass nop-agent (agent-adapter) ())
(defclass oracle-agent (agent-adapter) ())
(defclass command-agent (agent-adapter) ())

(defun make-nop-agent ()
  (%e0-pending 'make-nop-agent))

(defun make-oracle-agent ()
  (%e0-pending 'make-oracle-agent))

(defun make-command-agent (&key command env name)
  (declare (ignore command env name))
  (%e0-pending 'make-command-agent))

(defgeneric command-agent-command (agent))
