(in-package #:agent-eval)

;;; Grading through the task's own tests (E0), as Harbor's verifier does
;;; (src/harbor/verifier/verifier.py): the tests reach the environment only
;;; AFTER the agent, the script writes a reward file, the reward is the score.
;;; Spec: tests/grading-test.lisp.
;;;
;;; GRADE-TRIAL task environment &key timeout output-file → GRADE, in a
;;; started environment, in this order:
;;;   1. Delete /logs/verifier/reward.txt and reward.json if present (an agent
;;;      cannot pre-write its reward); make /logs/verifier.
;;;   2. UPLOAD-PATH the task's tests/ to /tests.
;;;   3. ENVIRONMENT-EXEC ("bash" CMD-PATH-of-/tests/test.sh) in the work
;;;      dir, ENV = the task's :VERIFIER-ENV, timeout = TIMEOUT (default the
;;;      task's verifier timeout). stdout then stderr go to OUTPUT-FILE when
;;;      given (RUN-TRIAL: TRIAL-DIR/verifier/test-output.txt).
;;;   4. The reward:
;;;      timed out → reward 0, source :TIMEOUT (no file is read);
;;;      reward.txt → its trimmed text read as a real (REWARDS ((:reward . R)),
;;;        source :REWARD-TXT);
;;;      else reward.json → a JSON number, or an object: its "reward" member
;;;        when present, else the mean of its numeric members (REWARDS: every
;;;        numeric member as (KEYWORD . VALUE), keys upcased, _ → -; source
;;;        :REWARD-JSON);
;;;      else, or when the file does not parse as a finite real →
;;;        REWARD-MISSING (reason names the file or "no reward file"), with
;;;        restarts USE-EXIT-CODE (reward 1 if exit 0 else 0, source
;;;        :EXIT-CODE) and SCORE-ZERO (reward 0, source :MISSING).
;;; GRADE readers: REWARD (a real, as written: not clamped), REWARDS,
;;; PASSED-P (REWARD >= 1), SOURCE, EXIT-CODE (NIL on timeout), OUTPUT (the
;;; script's stdout then stderr, capped as EXEC-RESULT), TIMED-OUT-P,
;;; DURATION-MS.

(defgeneric grade-reward (grade))
(defgeneric grade-rewards (grade))
(defgeneric grade-passed-p (grade))
(defgeneric grade-source (grade))
(defgeneric grade-exit-code (grade))
(defgeneric grade-output (grade))
(defgeneric grade-timed-out-p (grade))
(defgeneric grade-duration-ms (grade))

(defclass grade ()
  ((reward :initarg :reward :reader grade-reward :initform 0)
   (rewards :initarg :rewards :reader grade-rewards :initform nil)
   (passed-p :initarg :passed-p :reader grade-passed-p :initform nil)
   (source :initarg :source :reader grade-source :initform nil)
   (exit-code :initarg :exit-code :reader grade-exit-code :initform nil)
   (output :initarg :output :reader grade-output :initform "")
   (timed-out-p :initarg :timed-out-p :reader grade-timed-out-p :initform nil)
   (duration-ms :initarg :duration-ms :reader grade-duration-ms :initform 0))
  (:documentation "The verifier's verdict on one trial."))

(defmethod print-object ((g grade) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~a (~(~a~))" (grade-reward g) (grade-source g))))

(defun %make-grade (&key reward rewards source exit-code output timed-out-p duration-ms)
  (make-instance 'grade :reward reward :rewards rewards :passed-p (>= reward 1)
                        :source source :exit-code exit-code :output (or output "")
                        :timed-out-p timed-out-p :duration-ms (or duration-ms 0)))

(defun %parse-real (text)
  "TEXT as a JSON value when it is a real, else NIL (json-protocol's number
grammar: no reader, no inf / NaN)."
  (let ((s (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
    (and (plusp (length s))
         (let ((v (handler-case (json:decode s) (json:json-error () nil))))
           (and (realp v) v)))))

(defun %reward-key (name)
  (intern (substitute #\- #\_ (string-upcase name)) :keyword))

(defun %rewards-from-json (value)
  "A decoded reward.json → (values reward rewards), or NIL when it has no
numeric reward."
  (cond ((realp value) (values value (list (cons :reward value))))
        ((hash-table-p value)
         (let ((members (sort (loop for k being the hash-keys of value using (hash-value v)
                                    when (realp v) collect (cons k v))
                              #'string< :key #'car)))
           (when members
             (let ((own (gethash "reward" value)))
               (values (if (realp own)
                           own
                           (/ (reduce #'+ members :key #'cdr) (length members)))
                       (mapcar (lambda (m) (cons (%reward-key (car m)) (cdr m))) members))))))
        (t nil)))

(defun %read-reward (directory)
  "The reward in DIRECTORY (reward.txt, else reward.json) →
(values reward rewards source), or (values NIL NIL NIL WHY)."
  (let ((txt (probe-file (merge-pathnames "reward.txt" directory)))
        (jsn (probe-file (merge-pathnames "reward.json" directory))))
    (cond (txt
           (let ((r (%parse-real (%read-text-file txt))))
             (if r
                 (values r (list (cons :reward r)) :reward-txt)
                 (values nil nil nil "reward.txt is not a number"))))
          (jsn
           (let ((decoded (handler-case (json:decode (%read-text-file jsn))
                            (json:json-error () nil))))
             (multiple-value-bind (r rewards) (%rewards-from-json decoded)
               (if r
                   (values r rewards :reward-json)
                   (values nil nil nil "reward.json has no numeric reward")))))
          (t (values nil nil nil "no reward file")))))

(defun grade-trial (task environment &key timeout output-file)
  "Grade the trial in started ENVIRONMENT through TASK's tests (see the header)."
  (let ((verifier-dir (merge-pathnames "verifier/" (uiop:ensure-directory-pathname
                                                    (environment-trial-directory environment)))))
    ;; 1. no stale reward files, /logs/verifier exists
    (environment-exec environment
                      (list "sh" "-c" "rm -f \"$1\" \"$2\"; mkdir -p \"$3\""
                            "agent-eval"
                            (environment-command-path environment "/logs/verifier/reward.txt")
                            (environment-command-path environment "/logs/verifier/reward.json")
                            (environment-command-path environment "/logs/verifier")))
    ;; 2. the tests arrive after the agent
    (upload-path environment
                 (merge-pathnames "tests/" (uiop:ensure-directory-pathname (task-directory task)))
                 "/tests")
    ;; 3. the verifier
    (let* ((result (environment-exec environment
                                     (list "bash" (environment-command-path environment
                                                                            "/tests/test.sh"))
                                     :env (task-verifier-env task)
                                     :timeout (or timeout (task-verifier-timeout task))))
           (output (%cap-output (concatenate 'string (exec-result-stdout result)
                                             (exec-result-stderr result))))
           (exit-code (exec-result-exit-code result))
           (common (list :exit-code exit-code :output output
                         :timed-out-p (exec-result-timed-out-p result)
                         :duration-ms (exec-result-duration-ms result))))
      (when output-file
        (%write-text-file (pathname output-file) output))
      ;; 4. the reward
      (if (exec-result-timed-out-p result)
          (apply #'%make-grade :reward 0 :rewards nil :source :timeout common)
          (progn
            (ensure-directories-exist verifier-dir)
            (dolist (f '("reward.txt" "reward.json"))
              (uiop:delete-file-if-exists (merge-pathnames f verifier-dir)))
            (download-path environment "/logs/verifier" verifier-dir)
            (multiple-value-bind (reward rewards source why) (%read-reward verifier-dir)
              (if reward
                  (apply #'%make-grade :reward reward :rewards rewards :source source common)
                  (multiple-value-bind (reward source)
                      (restart-case (error 'reward-missing :task-id (task-id task) :reason why
                                                           :message why)
                        (use-exit-code ()
                          :report "Reward 1 if the test script exited 0, else 0."
                          (values (if (eql 0 exit-code) 1 0) :exit-code))
                        (score-zero ()
                          :report "Reward 0."
                          (values 0 :missing)))
                    (apply #'%make-grade :reward reward :rewards (list (cons :reward reward))
                                         :source source common)))))))))
