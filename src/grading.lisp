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

(defclass grade () ()
  (:documentation "The verifier's verdict on one trial (stub)."))

(defun grade-trial (task environment &key timeout output-file)
  (declare (ignore task environment timeout output-file))
  (%e0-pending 'grade-trial))

(defgeneric grade-reward (grade))
(defgeneric grade-rewards (grade))
(defgeneric grade-passed-p (grade))
(defgeneric grade-source (grade))
(defgeneric grade-exit-code (grade))
(defgeneric grade-output (grade))
(defgeneric grade-timed-out-p (grade))
(defgeneric grade-duration-ms (grade))
