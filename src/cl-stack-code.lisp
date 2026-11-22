(in-package #:agent-eval)

;;; The first adapter: cl-stack-code headless (-p, stream-json), run as a
;;; command INSIDE the task environment, as Harbor's installed agents run
;;; theirs (e.g. claude_code.py: `claude --print --output-format=stream-json`
;;; tee'd to /logs/agent). No Lisp dependency on cl-stack-code: the wire is
;;; its stream-json (cl-stack-code 0.10.0, src/headless.lisp). Spec:
;;; tests/cl-stack-code-test.lisp.
;;;
;;; MAKE-CL-STACK-CODE-AGENT &key (command '("cl-stack-code")) model max-turns
;;;   (permission-mode "bypassPermissions") append-system-prompt model-params
;;;   settings env prices version
;;; COMMAND: a non-empty list of strings (the executable and leading args; a
;;; container needs it on its PATH or mounted). MODEL: a cl-stack-code
;;; designator ("openai:qwen…") or NIL. MAX-TURNS: a positive integer or NIL.
;;; MODEL-PARAMS: an alist (KEY . VALUE). SETTINGS: NIL or a JSON object
;;; (string-key hash table) written as settings.json. ENV: an alist passed to
;;; the command (keys and base URLs: OPENAI_BASE_URL, OPENAI_API_KEY, …).
;;; PRICES: NIL or a plist (:input :output :cache-read :cache-write) in USD
;;; per million tokens. Bad values → AGENT-EVAL-ERROR. AGENT-NAME
;;; "cl-stack-code"; AGENT-VERSION VERSION; AGENT-CONFIG (:name
;;; "cl-stack-code" :command C :model M :max-turns N :permission-mode P
;;; :append-system-prompt S :model-params A :settings-json TEXT-or-NIL
;;; :prices P) — never ENV (it holds keys).
;;;
;;; CL-STACK-CODE-ARGV agent instruction workdir → the argv, exactly:
;;;   COMMAND… "-p" INSTRUCTION "--output-format" "stream-json"
;;;   "--permission-mode" PERMISSION-MODE "--cwd" WORKDIR
;;;   ["--model" MODEL] ["--max-turns" N] ["--append-system-prompt" S]
;;;   then "--model-param" "KEY=VALUE" per MODEL-PARAMS entry in order
;;;   (VALUE as given when a string, else its JSON text).
;;; bypassPermissions is the default because the environment is the sandbox
;;; (Harbor's Claude Code adapter does the same); -p refuses every ask.
;;;
;;; AGENT-SETUP: when SETTINGS is given, writes it (json-protocol ENCODE) to
;;; /logs/agent/config/settings.json in the environment.
;;; AGENT-RUN: ENVIRONMENT-EXEC of the argv (WORKDIR = the command path of
;;; the task's work dir) in the work dir, timeout = the task's agent timeout,
;;; ENV plus CL_STACK_CODE_CONFIG_DIR (the command path of /logs/agent/config)
;;; and CL_STACK_CODE_DATA_DIR (/logs/agent/data). stdout is written to
;;; logs-directory/cl-stack-code.jsonl and stderr to
;;; logs-directory/cl-stack-code.stderr, then CONTEXT-FROM-STREAM-JSON fills
;;; the context. Then: a timeout → AGENT-TIMEOUT; a non-zero exit with no
;;; "result" event → AGENT-FAILED (exit code). A "result" with an error
;;; subtype is not a failure (the run ended; the verifier decides).
;;;
;;; PARSE-STREAM-JSON text → a list of decoded objects, one per non-blank
;;; line, in order; a line that is not a JSON object is skipped.
;;; CONTEXT-FROM-STREAM-JSON events context &key prices → context. Sets
;;; TRANSCRIPT to EVENTS. From the LAST event whose "type" is "result":
;;;   usage {input_tokens output_tokens cache_read_tokens cache_write_tokens}
;;;   → INPUT-TOKENS = input + cache read + cache write (Harbor counts cache
;;;     inside input), CACHE-TOKENS = cache read, OUTPUT-TOKENS = output;
;;;   num_turns → TURNS; "result" → RESULT-TEXT; "subtype" → STATUS-DETAIL;
;;;   COST-USD = "total_cost_usd" when the event has it, else
;;;   (USAGE-COST-USD usage prices) when PRICES, else NIL.
;;; No result event → STATUS-DETAIL "no_result", the counts stay NIL.
;;; USAGE-COST-USD usage prices → (input×:input + output×:output +
;;; cache-read×:cache-read + cache-write×:cache-write) / 10^6, a missing price
;;; counting as 0; USAGE the decoded "usage" object.

(defclass cl-stack-code-agent (agent-adapter)
  ((command :initarg :command :reader %csc-command)
   (model :initarg :model :reader %csc-model)
   (max-turns :initarg :max-turns :reader %csc-max-turns)
   (permission-mode :initarg :permission-mode :reader %csc-permission-mode)
   (append-system-prompt :initarg :append-system-prompt :reader %csc-append-system-prompt)
   (model-params :initarg :model-params :reader %csc-model-params)
   (settings :initarg :settings :reader %csc-settings)
   (env :initarg :env :reader %csc-env)
   (prices :initarg :prices :reader %csc-prices)
   (version :initarg :version :reader %csc-version))
  (:documentation "cl-stack-code -p (stream-json), run as a command inside the
task environment."))

(defparameter +price-keys+ '(:input :output :cache-read :cache-write))

(defun %csc-invalid (control &rest args)
  (error 'agent-eval-error :message (apply #'format nil control args)))

(defun %alist-p (x)
  (and (listp x) (every #'consp x)))

(defun %validate-prices (prices)
  (unless (or (null prices)
              (and (%plist-p prices)
                   (loop for (k v) on prices by #'cddr
                         always (and (member k +price-keys+) (typep v '(real 0))))))
    (%csc-invalid "prices must be a plist of ~{~s~^ ~} to non-negative reals, not ~s"
                  +price-keys+ prices)))

(defun make-cl-stack-code-agent (&key (command '("cl-stack-code")) model max-turns
                                   (permission-mode "bypassPermissions")
                                   append-system-prompt model-params settings env
                                   prices version)
  "A CL-STACK-CODE-AGENT (see the header). Bad values → AGENT-EVAL-ERROR."
  (unless (and (consp command) (every #'stringp command))
    (%csc-invalid "command must be a non-empty list of strings, not ~s" command))
  (unless (or (null model) (stringp model))
    (%csc-invalid "model must be a string or NIL, not ~s" model))
  (unless (or (null max-turns) (typep max-turns '(integer 1)))
    (%csc-invalid "max-turns must be a positive integer or NIL, not ~s" max-turns))
  (unless (stringp permission-mode)
    (%csc-invalid "permission-mode must be a string, not ~s" permission-mode))
  (unless (or (null append-system-prompt) (stringp append-system-prompt))
    (%csc-invalid "append-system-prompt must be a string or NIL, not ~s" append-system-prompt))
  (unless (and (%alist-p model-params)
               (every (lambda (p) (typep (car p) '(or string symbol))) model-params))
    (%csc-invalid "model-params must be an alist (KEY . VALUE), not ~s" model-params))
  (unless (or (null settings) (hash-table-p settings))
    (%csc-invalid "settings must be a JSON object (a hash table) or NIL, not ~s" settings))
  (unless (and (%alist-p env)
               (every (lambda (p) (typep (car p) '(or string symbol))) env))
    (%csc-invalid "env must be an alist (NAME . VALUE)"))
  (%validate-prices prices)
  (unless (or (null version) (stringp version))
    (%csc-invalid "version must be a string or NIL, not ~s" version))
  (make-instance 'cl-stack-code-agent
                 :command (copy-list command) :model model :max-turns max-turns
                 :permission-mode permission-mode
                 :append-system-prompt append-system-prompt
                 :model-params (copy-alist model-params) :settings settings
                 :env (copy-alist env) :prices (copy-list prices) :version version))

(defmethod agent-name ((agent cl-stack-code-agent)) "cl-stack-code")

(defmethod agent-version ((agent cl-stack-code-agent))
  (%csc-version agent))

(defmethod agent-config ((agent cl-stack-code-agent))
  (let ((settings (%csc-settings agent)))
    (list :name "cl-stack-code"
          :command (%csc-command agent)
          :model (%csc-model agent)
          :max-turns (%csc-max-turns agent)
          :permission-mode (%csc-permission-mode agent)
          :append-system-prompt (%csc-append-system-prompt agent)
          :model-params (%csc-model-params agent)
          :settings-json (and settings (json:encode settings))
          :prices (%csc-prices agent))))

(defun %param-text (x)
  (cond ((stringp x) x)
        ((symbolp x) (string-downcase (symbol-name x)))
        (t (json:encode x))))

(defun cl-stack-code-argv (agent instruction workdir)
  "The argv of one cl-stack-code -p run (see the header)."
  (append (%csc-command agent)
          (list "-p" instruction "--output-format" "stream-json"
                "--permission-mode" (%csc-permission-mode agent) "--cwd" workdir)
          (let ((m (%csc-model agent))) (and m (list "--model" m)))
          (let ((n (%csc-max-turns agent))) (and n (list "--max-turns" (princ-to-string n))))
          (let ((s (%csc-append-system-prompt agent))) (and s (list "--append-system-prompt" s)))
          (loop for (k . v) in (%csc-model-params agent)
                append (list "--model-param"
                             (format nil "~a=~a" (%param-text k)
                                     (if (stringp v) v (json:encode v)))))))

(defmethod agent-setup ((agent cl-stack-code-agent) environment)
  (let ((settings (%csc-settings agent)))
    (when settings
      (uiop:with-temporary-file (:pathname tmp :stream s :direction :output
                                 :external-format :utf-8 :type "json")
        (write-string (json:encode settings) s)
        (finish-output s)
        (close s)
        (upload-path environment tmp "/logs/agent/config/settings.json")))))

;;; The stream

(defun %event-type (event)
  (and (hash-table-p event) (gethash "type" event)))

(defun parse-stream-json (text)
  "One decoded JSON object per non-blank line of TEXT, in order; other lines
are skipped."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for l = (string-trim '(#\Space #\Tab #\Return) line)
        for value = (and (plusp (length l))
                         (handler-case (json:decode l)
                           (json:json-error () nil)))
        when (hash-table-p value) collect value))

(defun %count (usage key)
  "USAGE's integer member KEY, 0 when missing or not an integer."
  (let ((v (and (hash-table-p usage) (gethash key usage))))
    (if (integerp v) v 0)))

(defun usage-cost-usd (usage prices)
  "USD for USAGE (a decoded \"usage\" object) at PRICES (USD per million
tokens; a missing price counts as 0)."
  (/ (+ (* (%count usage "input_tokens") (getf prices :input 0))
        (* (%count usage "output_tokens") (getf prices :output 0))
        (* (%count usage "cache_read_tokens") (getf prices :cache-read 0))
        (* (%count usage "cache_write_tokens") (getf prices :cache-write 0)))
     1000000))

(defun context-from-stream-json (events context &key prices)
  "Fill CONTEXT from stream-json EVENTS (see the header). → CONTEXT."
  (setf (context-transcript context) events)
  (let ((result (find "result" events :key #'%event-type :test #'equal :from-end t)))
    (if (null result)
        (setf (context-status-detail context) "no_result")
        (let ((usage (gethash "usage" result))
              (turns (gethash "num_turns" result))
              (text (gethash "result" result))
              (subtype (gethash "subtype" result))
              (cost (gethash "total_cost_usd" result)))
          (when (hash-table-p usage)
            (setf (context-input-tokens context) (+ (%count usage "input_tokens")
                                                    (%count usage "cache_read_tokens")
                                                    (%count usage "cache_write_tokens"))
                  (context-cache-tokens context) (%count usage "cache_read_tokens")
                  (context-output-tokens context) (%count usage "output_tokens")))
          (when (integerp turns) (setf (context-turns context) turns))
          (when (stringp text) (setf (context-result-text context) text))
          (when (stringp subtype) (setf (context-status-detail context) subtype))
          (setf (context-cost-usd context)
                (cond ((realp cost) cost)
                      ((and prices (hash-table-p usage)) (usage-cost-usd usage prices))
                      (t nil))))))
  context)

(defun %effective-prices (prices)
  "PRICES for a run's cost. With neither cache price given, cache tokens are
priced as input (Harbor counts cache inside input); when either is given, a
missing one counts as 0 (USAGE-COST-USD)."
  (if (and prices
           (not (member :cache-read prices))
           (not (member :cache-write prices))
           (getf prices :input))
      (list* :cache-read (getf prices :input) :cache-write (getf prices :input) prices)
      prices))

(defmethod agent-run ((agent cl-stack-code-agent) instruction environment context)
  (let* ((task (environment-task environment))
         (argv (cl-stack-code-argv agent instruction
                                   (environment-command-path environment (task-workdir task))))
         (result (environment-exec
                  environment argv
                  :timeout (task-agent-timeout task)
                  :env (append (%csc-env agent)
                               (list (cons "CL_STACK_CODE_CONFIG_DIR"
                                           (environment-command-path environment "/logs/agent/config"))
                                     (cons "CL_STACK_CODE_DATA_DIR"
                                           (environment-command-path environment "/logs/agent/data"))))))
         (stdout (exec-result-stdout result)))
    (%write-agent-log context "cl-stack-code.jsonl" stdout)
    (%write-agent-log context "cl-stack-code.stderr" (exec-result-stderr result))
    (let ((events (parse-stream-json stdout)))
      (context-from-stream-json events context :prices (%effective-prices (%csc-prices agent)))
      (cond ((exec-result-timed-out-p result)
             (error 'agent-timeout :task-id (task-id task) :seconds (task-agent-timeout task)
                                   :message "cl-stack-code timed out"))
            ((and (not (eql 0 (exec-result-exit-code result)))
                  (not (find "result" events :key #'%event-type :test #'equal)))
             (error 'agent-failed :task-id (task-id task) :exit-code (exec-result-exit-code result)
                                  :message "cl-stack-code exited without a result"))))))
