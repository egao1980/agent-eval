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

(defclass cl-stack-code-agent (agent-adapter) ())

(defun make-cl-stack-code-agent (&key (command '("cl-stack-code")) model max-turns
                                   (permission-mode "bypassPermissions")
                                   append-system-prompt model-params settings env
                                   prices version)
  (declare (ignore command model max-turns permission-mode append-system-prompt
                   model-params settings env prices version))
  (%e0-pending 'make-cl-stack-code-agent))

(defun cl-stack-code-argv (agent instruction workdir)
  (declare (ignore agent instruction workdir))
  (%e0-pending 'cl-stack-code-argv))

(defun parse-stream-json (text)
  (declare (ignore text))
  (%e0-pending 'parse-stream-json))

(defun context-from-stream-json (events context &key prices)
  (declare (ignore events context prices))
  (%e0-pending 'context-from-stream-json))

(defun usage-cost-usd (usage prices)
  (declare (ignore usage prices))
  (%e0-pending 'usage-cost-usd))
