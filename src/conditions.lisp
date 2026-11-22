(in-package #:agent-eval)

;;; Conditions (given: they are the contract's error vocabulary). Every
;;; agent-eval error is an EVAL-PROTOCOL:EVAL-ERROR, so a handler written for
;;; eval-protocol sees it. Errors that escape a trial inside RUN-AGENT-EVAL
;;; reach its caller as EVAL-PROTOCOL:SCORER-ERROR with eval-protocol's own
;;; restarts SKIP-CASE, RETRY-CASE and SCORE-AS-FAILURE (src/runner.lisp).
;;;
;;; Restarts agent-eval establishes (exported names):
;;;   SKIP-TASK      LOAD-TB-DATASET, around each TASK-FORMAT-ERROR: leave the
;;;                  task out of the dataset. USE-VALUE (a task) replaces it.
;;;   USE-EXIT-CODE  GRADE-TRIAL, around REWARD-MISSING: reward 1 when the
;;;                  test script exited 0, else 0 (source :EXIT-CODE).
;;;   SCORE-ZERO     GRADE-TRIAL, around REWARD-MISSING: reward 0 (source
;;;                  :MISSING).
;;;   CONTINUE       COMPARE-RUNS, around DATASET-MISMATCH: compare the task
;;;                  ids the two runs share.

(defconstant +version+ (if (boundp (quote +version+)) (symbol-value (quote +version+)) "0.1.0")
  "agent-eval's version.")

(define-condition agent-eval-error (ev:eval-error)
  ()
  (:report (lambda (c s)
             (format s "agent-eval: ~a" (or (ev:eval-error-message c) "error")))))

(define-condition task-format-error (agent-eval-error)
  ((path :initarg :path :reader task-format-error-path :initform nil)
   (field :initarg :field :reader task-format-error-field :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: bad Terminal-Bench task~@[ ~a~] (~(~a~))~@[: ~a~]"
                     (task-format-error-path c)
                     (or (task-format-error-field c) :task)
                     (ev:eval-error-message c))))
  (:documentation "A task directory that is not a valid Terminal-Bench task.
PATH is the task directory (a native namestring), FIELD a keyword naming the
bad part: :TASK-TOML :INSTRUCTION :ENVIRONMENT :TESTS :ID :TIMEOUT :SOLUTION
:TASKS (an empty dataset) or :TASK (not a task)."))

(define-condition unsupported-task (agent-eval-error)
  ((task-id :initarg :task-id :reader unsupported-task-task-id :initform nil)
   (reason :initarg :reason :reader unsupported-task-reason :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: task ~@[~a ~]cannot run here: ~a"
                     (unsupported-task-task-id c) (unsupported-task-reason c))))
  (:documentation "A valid task this environment cannot run: a
docker-compose task, a task asking for GPUs, a task with neither image nor
Dockerfile build support. RUN-AGENT-EVAL's :ON-UNSUPPORTED decides."))

(define-condition environment-error (agent-eval-error)
  ((task-id :initarg :task-id :reader environment-error-task-id :initform nil)
   (phase :initarg :phase :reader environment-error-phase :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: environment for ~a failed~@[ in ~(~a~)~]~@[: ~a~]"
                     (or (environment-error-task-id c) "a task")
                     (environment-error-phase c)
                     (ev:eval-error-message c))))
  (:documentation "Starting, using or stopping a task environment failed.
PHASE is one of +TRIAL-PHASES+ or :STOP."))

(define-condition agent-timeout (agent-eval-error)
  ((task-id :initarg :task-id :reader agent-timeout-task-id :initform nil)
   (seconds :initarg :seconds :reader agent-timeout-seconds :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: the agent passed its ~a s budget on ~a"
                     (agent-timeout-seconds c) (or (agent-timeout-task-id c) "a task"))))
  (:documentation "Signalled by an adapter's AGENT-RUN when the agent ran
past the task's agent timeout. RUN-TRIAL turns it into status :AGENT-TIMEOUT
and still grades the environment."))

(define-condition agent-failed (agent-eval-error)
  ((task-id :initarg :task-id :reader agent-failed-task-id :initform nil)
   (exit-code :initarg :exit-code :reader agent-failed-exit-code :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: the agent failed on ~a~@[ (exit ~a)~]~@[: ~a~]"
                     (or (agent-failed-task-id c) "a task")
                     (agent-failed-exit-code c)
                     (ev:eval-error-message c))))
  (:documentation "Signalled by an adapter's AGENT-RUN when the agent itself
failed (a non-zero exit, no result). RUN-TRIAL turns it into status
:AGENT-ERROR and still grades the environment."))

(define-condition reward-missing (agent-eval-error)
  ((task-id :initarg :task-id :reader reward-missing-task-id :initform nil)
   (reason :initarg :reason :reader reward-missing-reason :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: no reward for ~a: ~a"
                     (or (reward-missing-task-id c) "a task") (reward-missing-reason c))))
  (:documentation "The verifier wrote neither a usable reward.txt nor
reward.json. Restarts USE-EXIT-CODE and SCORE-ZERO."))

(define-condition dataset-mismatch (agent-eval-error)
  ((baseline :initarg :baseline :reader dataset-mismatch-baseline :initform nil)
   (candidate :initarg :candidate :reader dataset-mismatch-candidate :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: runs on different datasets (~a vs ~a)"
                     (dataset-mismatch-baseline c) (dataset-mismatch-candidate c))))
  (:documentation "COMPARE-RUNS on runs whose DATASET-DIGESTs differ.
BASELINE and CANDIDATE are the digests. Restart CONTINUE."))

(define-condition run-not-found (agent-eval-error)
  ((path :initarg :path :reader run-not-found-path :initform nil))
  (:report (lambda (c s)
             (format s "agent-eval: no saved run at ~a" (run-not-found-path c)))))
