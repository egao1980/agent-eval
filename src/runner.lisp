(in-package #:agent-eval)

;;; Trials and runs (E0). Spec: tests/runner-test.lisp.
;;;
;;; RUN-TRIAL task agent &key environment-factory (attempt 1) directory
;;;   (on-missing-reward :zero) → TRIAL
;;; ENVIRONMENT-FACTORY (lambda (task trial-directory) → an unstarted
;;; TASK-ENVIRONMENT); default MAKE-LOCAL-ENVIRONMENT with :DIRECTORY.
;;; DIRECTORY is the trial directory (default a fresh one under the temporary
;;; directory); it gets agent/ (CONTEXT-LOGS-DIRECTORY) and verifier/.
;;; Before anything starts: a COMPOSE-P task, or GPUS > 0, → UNSUPPORTED-TASK
;;; (no restart; RUN-AGENT-EVAL applies its policy). The phases, in order,
;;; each timed into TRIAL-TIMINGS (a plist PHASE → ms, only phases that ran):
;;;   :ENVIRONMENT-SETUP  factory + START-ENVIRONMENT
;;;   :AGENT-SETUP        AGENT-SETUP
;;;   :AGENT-EXECUTION    AGENT-RUN (instruction = TASK-INSTRUCTION) with a
;;;                       fresh context, then AGENT-POPULATE-CONTEXT
;;;   :VERIFIER           GRADE-TRIAL with OUTPUT-FILE verifier/test-output.txt
;;; then STOP-ENVIRONMENT on every exit. Status (one of +TRIAL-STATUSES+):
;;;   :COMPLETED          AGENT-RUN returned
;;;   :AGENT-TIMEOUT      it signalled AGENT-TIMEOUT          } still graded
;;;   :AGENT-ERROR        AGENT-FAILED or any other ERROR      } (TB semantics)
;;;   :ENVIRONMENT-ERROR  an ERROR in environment or agent setup, or an
;;;                       ENVIRONMENT-ERROR while grading: not graded, reward 0
;;; TRIAL-ERROR is the condition's text (PRINC) for every status but
;;; :COMPLETED, else NIL. REWARD-MISSING while grading follows
;;; ON-MISSING-REWARD: :ZERO → SCORE-ZERO, :EXIT-CODE → USE-EXIT-CODE,
;;; :ERROR → not handled (it propagates; the environment is still stopped).
;;;
;;; TRIAL readers: TASK-ID ATTEMPT AGENT-NAME AGENT-VERSION STATUS CONTEXT
;;; GRADE (NIL when not graded) REWARD (the grade's, else 0) PASSED-P TIMINGS
;;; WALL-MS (the whole trial) STARTED-AT FINISHED-AT (UTC
;;; "YYYY-MM-DDTHH:MM:SSZ") DIRECTORY (native namestring) ERROR TRANSCRIPT
;;; (the context's). MAKE-TRIAL &key (every reader's name as a key) builds one
;;; from parts (records and tests); REWARD and PASSED-P default from GRADE
;;; (else 0 and NIL), ATTEMPT to 1, STATUS to :COMPLETED.
;;;
;;; RUN-AGENT-EVAL dataset agent &key (attempts 1) environment-factory
;;;   directory (on-unsupported :skip) (on-missing-reward :zero) id label
;;;   → AGENT-EVAL-RUN
;;; Tasks in dataset order, attempts 1..ATTEMPTS each (sequential), trial
;;; directories DIRECTORY/trials/<task-id>/<attempt>/ (DIRECTORY default a
;;; fresh temporary one). Each trial → EV:MAKE-EVAL-CASE-RESULT :case TASK
;;; :actual TRIAL :score (EV:MAKE-EVAL-SCORE :value REWARD clamped to [0, 1]
;;; :verdict :PASS when PASSED-P else :FAIL :rationale "reward R (SOURCE)").
;;; UNSUPPORTED-TASK: :SKIP → one skipped result per task (EV's own skip
;;; shape: :skipped-p T, score value 0 verdict :SKIP), no trials; :FAIL → one
;;; result per attempt, value 0 :FAIL, rationale "unsupported: REASON", no
;;; trial; :ERROR → it propagates. Any other ERROR escaping RUN-TRIAL (such
;;; as REWARD-MISSING under :ON-MISSING-REWARD :ERROR) →
;;; EV:SCORER-ERROR (:case TASK :scorer AGENT :cause C :message (princ C))
;;; with eval-protocol's restarts: SKIP-CASE (a skipped result), RETRY-CASE
;;; (the same attempt again), SCORE-AS-FAILURE (value 0, :FAIL).
;;; AGENT-EVAL-RUN is an EV:EVAL-RUN (so EV:EVAL-RUN-MEAN / -N / -PASS-COUNT
;;; and eval-protocol's gates work; one result per attempt) made with
;;; MAKE-AGENT-EVAL-RUN &key dataset results trials id label agent-name
;;; agent-config attempts started-at finished-at dataset-digest. Readers:
;;; RUN-ID (default "run-YYYYMMDDTHHMMSSZ-XXXXXX", X hex), RUN-LABEL,
;;; RUN-AGENT-NAME, RUN-AGENT-CONFIG (AGENT-CONFIG), RUN-ATTEMPTS,
;;; RUN-TRIALS (in run order), RUN-STARTED-AT, RUN-FINISHED-AT,
;;; RUN-DATASET-DIGEST (DATASET-DIGEST of the dataset), RUN-TASK-TRIALS run
;;; task-id → that task's trials by attempt.

(defparameter +trial-statuses+
  '(:completed :agent-timeout :agent-error :environment-error))

(defparameter +trial-phases+
  '(:environment-setup :agent-setup :agent-execution :verifier))

(defclass trial () ()
  (:documentation "One attempt of one task by one agent (stub)."))

(defun run-trial (task agent &key environment-factory (attempt 1) directory
                               (on-missing-reward :zero))
  (declare (ignore task agent environment-factory attempt directory on-missing-reward))
  (%e0-pending 'run-trial))

(defun make-trial (&key task-id attempt agent-name agent-version status context grade
                     reward passed-p timings wall-ms started-at finished-at directory
                     error)
  (declare (ignore task-id attempt agent-name agent-version status context grade
                   reward passed-p timings wall-ms started-at finished-at directory
                   error))
  (%e0-pending 'make-trial))

(defgeneric trial-task-id (trial))
(defgeneric trial-attempt (trial))
(defgeneric trial-agent-name (trial))
(defgeneric trial-agent-version (trial))
(defgeneric trial-status (trial))
(defgeneric trial-context (trial))
(defgeneric trial-grade (trial))
(defgeneric trial-reward (trial))
(defgeneric trial-passed-p (trial))
(defgeneric trial-timings (trial))
(defgeneric trial-wall-ms (trial))
(defgeneric trial-started-at (trial))
(defgeneric trial-finished-at (trial))
(defgeneric trial-directory (trial))
(defgeneric trial-error (trial))
(defgeneric trial-transcript (trial))

(defclass agent-eval-run (ev:eval-run) ()
  (:documentation "An eval-protocol run of agent trials (stub)."))

(defun make-agent-eval-run (&key dataset results trials id label agent-name agent-config
                              attempts started-at finished-at dataset-digest)
  (declare (ignore dataset results trials id label agent-name agent-config attempts
                   started-at finished-at dataset-digest))
  (%e0-pending 'make-agent-eval-run))

(defun run-agent-eval (dataset agent &key (attempts 1) environment-factory directory
                                       (on-unsupported :skip) (on-missing-reward :zero)
                                       id label)
  (declare (ignore dataset agent attempts environment-factory directory on-unsupported
                   on-missing-reward id label))
  (%e0-pending 'run-agent-eval))

(defgeneric run-id (run))
(defgeneric run-label (run))
(defgeneric run-agent-name (run))
(defgeneric run-agent-config (run))
(defgeneric run-attempts (run))
(defgeneric run-trials (run))
(defgeneric run-started-at (run))
(defgeneric run-finished-at (run))
(defgeneric run-dataset-digest (run))
(defgeneric run-task-trials (run task-id))
