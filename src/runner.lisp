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

(defclass trial ()
  ((task-id :initarg :task-id :reader trial-task-id)
   (attempt :initarg :attempt :reader trial-attempt)
   (agent-name :initarg :agent-name :reader trial-agent-name)
   (agent-version :initarg :agent-version :reader trial-agent-version)
   (status :initarg :status :reader trial-status)
   (context :initarg :context :reader trial-context)
   (grade :initarg :grade :reader trial-grade)
   (reward :initarg :reward :reader trial-reward)
   (passed-p :initarg :passed-p :reader trial-passed-p)
   (timings :initarg :timings :reader trial-timings)
   (wall-ms :initarg :wall-ms :reader trial-wall-ms)
   (started-at :initarg :started-at :reader trial-started-at)
   (finished-at :initarg :finished-at :reader trial-finished-at)
   (trial-directory :initarg :directory :reader trial-directory)
   (failure :initarg :error :reader trial-error))
  (:documentation "One attempt of one task by one agent."))

(defmethod trial-transcript ((trial trial))
  (let ((context (trial-context trial)))
    (and context (context-transcript context))))

(defmethod print-object ((trial trial) stream)
  (print-unreadable-object (trial stream :type t)
    (format stream "~a #~a ~(~a~) reward ~a" (trial-task-id trial) (trial-attempt trial)
            (trial-status trial) (trial-reward trial))))

(defun make-trial (&key task-id (attempt 1) agent-name agent-version (status :completed)
                     context grade (reward nil reward-p) (passed-p nil passed-p-p)
                     timings wall-ms started-at finished-at directory error)
  "A TRIAL from parts; REWARD and PASSED-P default from GRADE (else 0 and NIL)."
  (unless (member status +trial-statuses+)
    (error 'agent-eval-error :message (format nil "unknown trial status ~s" status)))
  (make-instance 'trial
                 :task-id task-id :attempt attempt :agent-name agent-name
                 :agent-version agent-version :status status :context context :grade grade
                 :reward (cond (reward-p reward) (grade (grade-reward grade)) (t 0))
                 :passed-p (cond (passed-p-p (and passed-p t))
                                 (grade (grade-passed-p grade))
                                 (t nil))
                 :timings timings :wall-ms wall-ms :started-at started-at
                 :finished-at finished-at :directory directory :error error))

(defun %unsupported-reason (task)
  "Why TASK cannot run in E0 (a string), or NIL."
  (let ((gpus (task-gpus task)))
    (cond ((task-compose-p task) "docker-compose tasks are not supported")
          ((and (realp gpus) (plusp gpus)) (format nil "the task asks for ~a GPU~:p" gpus))
          (t nil))))

(defun %check-supported (task)
  (let ((reason (%unsupported-reason task)))
    (when reason
      (error 'unsupported-task :task-id (task-id task) :reason reason :message reason))))

(defun %missing-reward-handler (policy)
  (ecase policy
    (:zero (lambda (c) (invoke-restart (find-restart 'score-zero c))))
    (:exit-code (lambda (c) (invoke-restart (find-restart 'use-exit-code c))))
    (:error (lambda (c) (declare (ignore c)) nil))))

(defun run-trial (task agent &key environment-factory (attempt 1) directory
                               (on-missing-reward :zero))
  "Run one attempt of TASK by AGENT and grade it (see the header) → TRIAL."
  (%check-supported task)
  (let* ((policy (%missing-reward-handler on-missing-reward))
         (dir (if directory (%directory-truename directory) (%fresh-directory "agent-eval-trial")))
         (agent-dir (ensure-directories-exist (merge-pathnames "agent/" dir)))
         (verifier-dir (ensure-directories-exist (merge-pathnames "verifier/" dir)))
         (factory (or environment-factory
                      (lambda (task directory) (make-local-environment task :directory directory))))
         (context (make-agent-context :logs-directory agent-dir))
         (started-at (%utc-timestamp))
         (start (get-internal-real-time))
         (timings '())
         (status :completed)
         (failure nil)
         (grade nil)
         (env nil))
    (labels ((timed (phase thunk)
               (let ((t0 (get-internal-real-time)))
                 (unwind-protect (funcall thunk)
                   (setf timings (append timings (list phase (%elapsed-ms t0)))))))
             (fail (new-status condition)
               (setf status new-status failure condition)))
      (unwind-protect
           (block phases
             ;; environment and agent setup: any error → :environment-error,
             ;; not graded. UNSUPPORTED-TASK (a factory's verdict) propagates.
             (flet ((setup-phase (phase thunk)
                      (handler-case (timed phase thunk)
                        (unsupported-task (c) (error c))
                        (error (c)
                          (fail :environment-error c)
                          (return-from phases)))))
               (setup-phase :environment-setup
                            (lambda ()
                              (setf env (funcall factory task dir))
                              (start-environment env)))
               (setup-phase :agent-setup (lambda () (agent-setup agent env))))
             (timed :agent-execution
                    (lambda ()
                      (handler-case (agent-run agent (task-instruction task) env context)
                        (agent-timeout (c) (fail :agent-timeout c))
                        (error (c) (fail :agent-error c)))
                      (handler-case (agent-populate-context agent context)
                        (error (c) (when (eq status :completed) (fail :agent-error c))))))
             (handler-case
                 (timed :verifier
                        (lambda ()
                          (setf grade
                                (handler-bind ((reward-missing policy))
                                  (grade-trial task env :output-file
                                               (merge-pathnames "test-output.txt" verifier-dir))))))
               (environment-error (c)
                 (setf grade nil)
                 (fail :environment-error c))))
        (when env (stop-environment env))))
    (make-trial :task-id (task-id task) :attempt attempt
                :agent-name (agent-name agent) :agent-version (agent-version agent)
                :status status :context context :grade grade
                :reward (if grade (grade-reward grade) 0)
                :passed-p (and grade (grade-passed-p grade))
                :timings timings :wall-ms (%elapsed-ms start)
                :started-at started-at :finished-at (%utc-timestamp)
                :directory (uiop:native-namestring dir)
                :error (and failure (princ-to-string failure)))))

;;; Runs

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

(defclass agent-eval-run (ev:eval-run)
  ((trials :initarg :trials :reader run-trials :initform nil)
   (id :initarg :id :reader run-id)
   (label :initarg :label :reader run-label :initform nil)
   (agent-name :initarg :agent-name :reader run-agent-name :initform nil)
   (agent-config :initarg :agent-config :reader run-agent-config :initform nil)
   (attempts :initarg :attempts :reader run-attempts :initform 1)
   (started-at :initarg :started-at :reader run-started-at :initform nil)
   (finished-at :initarg :finished-at :reader run-finished-at :initform nil)
   (dataset-digest :initarg :dataset-digest :reader run-dataset-digest :initform nil))
  (:documentation "An eval-protocol run of agent trials: one result per attempt."))

(defmethod print-object ((run agent-eval-run) stream)
  (print-unreadable-object (run stream :type t)
    (format stream "~a ~a: ~d trial~:p" (run-id run) (run-agent-name run)
            (length (run-trials run)))))

(defmethod run-task-trials ((run agent-eval-run) task-id)
  (sort (remove-if-not (lambda (tr) (equal task-id (trial-task-id tr))) (run-trials run))
        #'< :key #'trial-attempt))

(defun %default-run-id ()
  (format nil "run-~a-~a" (%utc-timestamp (get-universal-time) t) (%random-hex 6)))

(defun make-agent-eval-run (&key dataset results trials id label agent-name agent-config
                              attempts started-at finished-at dataset-digest
                              (n nil n-p) (mean nil mean-p) (pass-count nil pass-count-p))
  "An AGENT-EVAL-RUN. :N :MEAN :PASS-COUNT are eval-protocol's aggregates of
RESULTS unless given (LOAD-RUN gives the saved ones)."
  (let ((base (ev:make-eval-run :dataset dataset :results results)))
    (make-instance 'agent-eval-run
                   :dataset dataset
                   :results (ev:eval-run-results base)
                   :mean (if mean-p mean (ev:eval-run-mean base))
                   :n (if n-p n (ev:eval-run-n base))
                   :pass-count (if pass-count-p pass-count (ev:eval-run-pass-count base))
                   :trials (copy-list trials)
                   :id (or id (%default-run-id))
                   :label label
                   :agent-name agent-name
                   :agent-config agent-config
                   :attempts (or attempts
                                 (reduce #'max trials :key #'trial-attempt :initial-value 1))
                   :started-at started-at
                   :finished-at finished-at
                   :dataset-digest (or dataset-digest (and dataset (dataset-digest dataset))))))

(defun %clamp-reward (reward)
  (max 0 (min 1 reward)))

(defun %trial-score (trial)
  (let ((grade (trial-grade trial)))
    (ev:make-eval-score :value (%clamp-reward (trial-reward trial))
                        :verdict (if (trial-passed-p trial) :pass :fail)
                        :rationale (format nil "reward ~a (~(~a~))" (trial-reward trial)
                                           (if grade (grade-source grade) (trial-status trial))))))

(defun %trial-result (task trial)
  (ev:make-eval-case-result :case task :actual trial :score (%trial-score trial)))

(defun %skipped-result (task)
  "eval-protocol's own skip shape."
  (ev:make-eval-case-result :case task :skipped-p t
                            :score (ev:make-eval-score :value 0 :verdict :skip
                                                       :rationale "skipped")))

(defun %failed-result (task rationale)
  (ev:make-eval-case-result :case task
                            :score (ev:make-eval-score :value 0 :verdict :fail
                                                       :rationale rationale)))

(defun %run-attempt (task agent attempt directory trial-keys)
  "One attempt as an eval-case result → (values result trial-or-nil). An ERROR
escaping RUN-TRIAL (other than UNSUPPORTED-TASK) is signalled, stack live, as
EV:SCORER-ERROR with eval-protocol's restarts SKIP-CASE, RETRY-CASE (this
attempt again) and SCORE-AS-FAILURE."
  (let ((cause nil))
    (loop
      (restart-case
          (return-from %run-attempt
            (handler-bind ((error
                             (lambda (c)
                               (unless (typep c '(or unsupported-task ev:scorer-error))
                                 (setf cause c)
                                 (error 'ev:scorer-error :case task :scorer agent :cause c
                                                         :message (princ-to-string c))))))
              (let ((trial (apply #'run-trial task agent :attempt attempt
                                  :directory directory trial-keys)))
                (values (%trial-result task trial) trial))))
        (ev:skip-case ()
          :report "Skip this attempt (a skipped result)."
          (return-from %run-attempt (values (%skipped-result task) nil)))
        (ev:retry-case ()
          :report "Run this attempt again."
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname directory)
                                      :validate t :if-does-not-exist :ignore))
        (ev:score-as-failure ()
          :report "Record this attempt as a failure (value 0)."
          (return-from %run-attempt
            (values (%failed-result task (format nil "failure: ~a" cause)) nil)))))))

(defun %unsupported-results (task condition policy attempts)
  "Results for an unsupported TASK under POLICY (:SKIP / :FAIL / :ERROR)."
  (ecase policy
    (:skip (list (%skipped-result task)))
    (:fail (loop repeat attempts
                 collect (%failed-result task (format nil "unsupported: ~a"
                                                      (unsupported-task-reason condition)))))
    (:error (error condition))))

(defun run-agent-eval (dataset agent &key (attempts 1) environment-factory directory
                                       (on-unsupported :skip) (on-missing-reward :zero)
                                       id label)
  "Run AGENT on every task of DATASET, ATTEMPTS times each (see the header)
→ AGENT-EVAL-RUN."
  (check-type attempts (integer 1))
  (check-type on-unsupported (member :skip :fail :error))
  (check-type on-missing-reward (member :zero :exit-code :error))
  (let* ((root (if directory (%directory-truename directory) (%fresh-directory "agent-eval-run")))
         (started-at (%utc-timestamp))
         (trial-keys (append (and environment-factory
                                  (list :environment-factory environment-factory))
                             (list :on-missing-reward on-missing-reward)))
         (results '())
         (trials '()))
    (dolist (task (ev:eval-dataset-cases dataset))
      (let ((reason (%unsupported-reason task)))
        (if reason
            (setf results (append results
                                  (%unsupported-results
                                   task (make-condition 'unsupported-task :task-id (task-id task)
                                                                          :reason reason
                                                                          :message reason)
                                   on-unsupported attempts)))
            (loop for attempt from 1 to attempts
                  for dir = (merge-pathnames (format nil "trials/~a/~d/" (task-id task) attempt) root)
                  do (multiple-value-bind (result trial)
                         (handler-case (%run-attempt task agent attempt dir trial-keys)
                           (unsupported-task (c)
                             (values (first (%unsupported-results task c on-unsupported 1)) nil)))
                       (setf results (append results (list result)))
                       (when trial (setf trials (append trials (list trial)))))))))
    (make-agent-eval-run :dataset dataset :results results :trials trials :id id :label label
                         :agent-name (agent-name agent) :agent-config (agent-config agent)
                         :attempts attempts :started-at started-at
                         :finished-at (%utc-timestamp)
                         :dataset-digest (dataset-digest dataset))))
