(in-package #:agent-eval/tests)

;;; Spec for src/cl-stack-code.lisp: the cl-stack-code -p adapter. The live
;;; binary is not needed: tests/fixtures/bin/fake-cl-stack-code prints a
;;; recorded cl-stack-code 0.10.0 stream.

(deftest argv-exact
  (let ((agent (make-cl-stack-code-agent)))
    (ok (equal '("cl-stack-code" "-p" "Fix it." "--output-format" "stream-json"
                 "--permission-mode" "bypassPermissions" "--cwd" "/app")
               (cl-stack-code-argv agent "Fix it." "/app"))))
  (let ((agent (make-cl-stack-code-agent
                :command '("cl-repo" "run" "cl-stack-code" "--")
                :model "openai:qwen3" :max-turns 8 :permission-mode "acceptEdits"
                :append-system-prompt "Be brief."
                :model-params '(("top_k" . 20) ("stop" . "END")))))
    (ok (equal '("cl-repo" "run" "cl-stack-code" "--"
                 "-p" "Do." "--output-format" "stream-json"
                 "--permission-mode" "acceptEdits" "--cwd" "/w"
                 "--model" "openai:qwen3" "--max-turns" "8"
                 "--append-system-prompt" "Be brief."
                 "--model-param" "top_k=20" "--model-param" "stop=END")
               (cl-stack-code-argv agent "Do." "/w")))))

(deftest constructor-validation
  (ok (signals (make-cl-stack-code-agent :command '()) 'agent-eval-error))
  (ok (signals (make-cl-stack-code-agent :max-turns 0) 'agent-eval-error))
  (ok (signals (make-cl-stack-code-agent :model 7) 'agent-eval-error))
  (ok (signals (make-cl-stack-code-agent :prices '(:input -1)) 'agent-eval-error)))

(deftest name-version-config
  (let ((agent (make-cl-stack-code-agent :model "openai:m" :max-turns 3 :version "0.10.0"
                                         :env '(("OPENAI_API_KEY" . "sk-secret")))))
    (ok (equal "cl-stack-code" (agent-name agent)))
    (ok (equal "0.10.0" (agent-version agent)))
    (let ((config (agent-config agent)))
      (ok (equal "cl-stack-code" (getf config :name)))
      (ok (equal "openai:m" (getf config :model)))
      (ok (eql 3 (getf config :max-turns)))
      (ok (equal "bypassPermissions" (getf config :permission-mode)))
      (testing "the env (keys) never reaches the stored config"
        (ok (not (search "sk-secret" (prin1-to-string config))))))))

(defparameter +stream+
  (format nil "~{~a~%~}"
          '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"s\"}"
            ""
            "garbage line"
            "[1,2]"
            "{\"type\":\"assistant\",\"step\":1}"
            "{\"type\":\"result\",\"subtype\":\"error_max_turns\",\"result\":\"\",\"num_turns\":1,\"usage\":{\"input_tokens\":1,\"output_tokens\":1,\"cache_read_tokens\":0,\"cache_write_tokens\":0}}"
            "{\"type\":\"result\",\"subtype\":\"success\",\"result\":\"done\",\"num_turns\":4,\"usage\":{\"input_tokens\":1000,\"output_tokens\":200,\"cache_read_tokens\":300,\"cache_write_tokens\":50}}")))

(deftest stream-json-parsing
  (let ((events (parse-stream-json +stream+)))
    (ok (= 4 (length events)) "blank, non-JSON and non-object lines are skipped")
    (ok (equal '("system" "assistant" "result" "result")
               (mapcar (lambda (e) (gethash "type" e)) events)))))

(deftest context-from-the-last-result
  (let* ((events (parse-stream-json +stream+))
         (ctx (context-from-stream-json events (make-agent-context))))
    (ok (eq events (context-transcript ctx)))
    (ok (= 1350 (context-input-tokens ctx)) "input + cache read + cache write")
    (ok (= 300 (context-cache-tokens ctx)))
    (ok (= 200 (context-output-tokens ctx)))
    (ok (= 4 (context-turns ctx)))
    (ok (equal "done" (context-result-text ctx)))
    (ok (equal "success" (context-status-detail ctx)))
    (ok (null (context-cost-usd ctx)) "no prices, no total_cost_usd"))
  (testing "prices → cost"
    (let ((ctx (context-from-stream-json (parse-stream-json +stream+) (make-agent-context)
                                         :prices '(:input 3 :output 15 :cache-read 1/2))))
      (ok (= (/ (+ (* 1000 3) (* 200 15) (* 300 1/2)) 1000000) (context-cost-usd ctx)))))
  (testing "total_cost_usd wins"
    (let ((ctx (context-from-stream-json
                (parse-stream-json "{\"type\":\"result\",\"subtype\":\"success\",\"total_cost_usd\":0.25,\"usage\":{\"input_tokens\":1,\"output_tokens\":1,\"cache_read_tokens\":0,\"cache_write_tokens\":0}}")
                (make-agent-context) :prices '(:input 1000))))
      (ok (= 0.25 (context-cost-usd ctx)))))
  (testing "no result event"
    (let ((ctx (context-from-stream-json (parse-stream-json "{\"type\":\"system\"}")
                                         (make-agent-context))))
      (ok (equal "no_result" (context-status-detail ctx)))
      (ok (null (context-input-tokens ctx)))
      (ok (= 1 (length (context-transcript ctx)))))))

(deftest usage-cost
  (let ((usage (json:decode "{\"input_tokens\":2000000,\"output_tokens\":1000000,\"cache_read_tokens\":0,\"cache_write_tokens\":1000000}")))
    (ok (= (+ 2 5 1) (usage-cost-usd usage '(:input 1 :output 5 :cache-write 1))))
    (ok (= 2 (usage-cost-usd usage '(:input 1))))))

(defun fake-agent (&rest keys)
  (apply #'make-cl-stack-code-agent :command (fake-cl-stack-code-command) keys))

(deftest runs-in-the-environment
  (local-lane
   (multiple-value-bind (env ctx err)
       (run-in-env (fake-agent :model "openai:fake" :max-turns 5) (hello-task))
     (ok (null err))
     (testing "the agent acted on the work dir"
       (ok (equal (format nil "Hello, world!~%") (host-file env "/app/hello.txt"))))
     (testing "its argv, with --cwd the work dir's command path"
       (let ((argv (uiop:split-string (string-right-trim '(#\Newline)
                                                         (host-file env "/logs/agent/fake-argv.txt"))
                                      :separator '(#\Newline))))
         (ok (equal "-p" (first argv)))
         (ok (eql 0 (search "Create a file named" (second argv))) "then the instruction")
         (ok (member "--cwd" argv :test #'equal))
         (ok (equal (environment-command-path env "/app")
                    (second (member "--cwd" argv :test #'equal))))
         (ok (equal "5" (second (member "--max-turns" argv :test #'equal))))))
     (testing "CL_STACK_CODE_CONFIG_DIR is /logs/agent/config"
       (ok (equal (format nil "~a~%" (environment-command-path env "/logs/agent/config"))
                  (host-file env "/logs/agent/fake-config-dir.txt"))))
     (testing "the stream is kept and parsed"
       (ok (probe-file (merge-pathnames "cl-stack-code.jsonl" (context-logs-directory ctx))))
       (ok (= 5 (length (context-transcript ctx))))
       (ok (= 135 (context-input-tokens ctx)))
       (ok (= 30 (context-cache-tokens ctx)))
       (ok (= 20 (context-output-tokens ctx)))
       (ok (= 2 (context-turns ctx)))
       (ok (equal "success" (context-status-detail ctx)))
       (ok (equal "Created hello.txt" (context-result-text ctx)))))))

(deftest stream-is-not-capped
  (local-lane
   (testing "a stream longer than +OUTPUT-CAP+ keeps its final result event (agent-eval#2)"
     (let ((agent-eval::+output-cap+ 200))
       (multiple-value-bind (env ctx err) (run-in-env (fake-agent) (hello-task))
         (declare (ignore env))
         (ok (null err))
         (ok (equal "success" (context-status-detail ctx)))
         (ok (= 135 (context-input-tokens ctx)))
         (ok (< 200 (length (uiop:read-file-string
                             (merge-pathnames "cl-stack-code.jsonl"
                                              (context-logs-directory ctx)))))
             "the kept stream is whole"))))))

(deftest settings-file
  (local-lane
   (let ((settings (json:decode "{\"maxTurns\":3,\"appendSystemPrompt\":\"x\"}")))
     (multiple-value-bind (env ctx err) (run-in-env (fake-agent :settings settings) (hello-task))
       (declare (ignore ctx))
       (ok (null err))
       (ok (equalp settings (json:decode (host-file env "/logs/agent/config/settings.json"))))))))

(deftest failures-and-end-states
  (local-lane
   (testing "a non-zero exit with no result → AGENT-FAILED; stderr kept"
     (multiple-value-bind (env ctx err)
         (run-in-env (fake-agent :env '(("FAKE_CSC_MODE" . "fail"))) (hello-task))
       (declare (ignore env))
       (ok (typep err 'agent-failed))
       (ok (eql 1 (agent-failed-exit-code err)))
       (ok (equal "no_result" (context-status-detail ctx)))
       (ok (search "model unreachable"
                   (uiop:read-file-string
                    (merge-pathnames "cl-stack-code.stderr" (context-logs-directory ctx)))))))
   (testing "error_max_turns is an end state, not a failure"
     (multiple-value-bind (env ctx err)
         (run-in-env (fake-agent :env '(("FAKE_CSC_MODE" . "max-turns"))) (hello-task))
       (ok (null err))
       (ok (equal "error_max_turns" (context-status-detail ctx)))
       (ok (null (host-file env "/app/hello.txt")))))
   (with-tmpdir (root)
     (testing "past the agent timeout → AGENT-TIMEOUT, the context still filled"
       (multiple-value-bind (env ctx err)
           (run-in-env (fake-agent :env '(("FAKE_CSC_MODE" . "sleep")))
                       (load-tb-task (write-tb-task root "slow" :toml (toml-with "[agent]
timeout_sec = 1.0"))))
         (declare (ignore env))
         (ok (typep err 'agent-timeout))
         (ok (stringp (context-status-detail ctx))))))))
