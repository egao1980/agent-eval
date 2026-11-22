(in-package #:agent-eval)

;;; Comparing two runs (E0). Spec: tests/compare-test.lisp.
;;;
;;; COMPARE-RUNS baseline candidate → RUN-COMPARISON. Different
;;; RUN-DATASET-DIGESTs → DATASET-MISMATCH (the two digests) with restart
;;; CONTINUE: compare the task ids both runs have. Unhandled, it is an error.
;;; ROWS: one plist per task id with trials in both runs, by id (STRING<):
;;;   (:task-id ID :baseline-mean B :candidate-mean C :delta D :baseline-passed
;;;    BP :candidate-passed CP :baseline-n BN :candidate-n CN :outcome O)
;;;   B and C as RUN-TASK-STATS' :MEAN-REWARD, D = C − B, O :WIN (D > 0),
;;;   :TIE (D = 0) or :LOSS.
;;; WINS / TIES / LOSSES count rows. MEAN-DELTA: the mean D (0 with no rows).
;;; TOKEN-DELTA: candidate − baseline of (input + output) tokens from
;;; RUN-TOTALS, NIL when either side has none. COST-DELTA likewise with
;;; :COST-USD. WALL-DELTA: candidate − baseline :WALL-MS. ONLY-BASELINE /
;;; ONLY-CANDIDATE: ids with trials on one side only (STRING<).
;;; BASELINE-ID / CANDIDATE-ID: the runs' RUN-IDs.
;;;
;;; COMPARISON-REPORT comparison &key (format :markdown)
;;;   :SEXP → (:run-comparison :baseline :candidate :wins :ties :losses
;;;            :mean-delta :token-delta :cost-delta :wall-delta :only-baseline
;;;            :only-candidate :rows ROWS)
;;;   :JSON → that object as JSON (keys as EVAL-REPORT's :JSON)
;;;   :MARKDOWN → a string with, line by line:
;;;     "# agent-eval comparison: BASELINE-ID vs CANDIDATE-ID"
;;;     ""
;;;     "| task | baseline | candidate | delta |"
;;;     "|------|----------|-----------|-------|"
;;;     "| ID | B | C | D |"  per row; B, C as ~,2F; D as ~,2@F (signed)
;;;     ""
;;;     "Wins W, ties T, losses L; mean delta D"   (D as ~,2@F)
;;;     "Tokens X; cost Y; wall Z ms"   X as ~@D or "n/a", Y as ~,4@F
;;;                                     or "n/a", Z as ~@D

(defgeneric comparison-baseline-id (comparison))
(defgeneric comparison-candidate-id (comparison))
(defgeneric comparison-rows (comparison))
(defgeneric comparison-wins (comparison))
(defgeneric comparison-ties (comparison))
(defgeneric comparison-losses (comparison))
(defgeneric comparison-mean-delta (comparison))
(defgeneric comparison-token-delta (comparison))
(defgeneric comparison-cost-delta (comparison))
(defgeneric comparison-wall-delta (comparison))
(defgeneric comparison-only-baseline (comparison))
(defgeneric comparison-only-candidate (comparison))

(defclass run-comparison ()
  ((baseline-id :initarg :baseline-id :reader comparison-baseline-id)
   (candidate-id :initarg :candidate-id :reader comparison-candidate-id)
   (rows :initarg :rows :reader comparison-rows)
   (wins :initarg :wins :reader comparison-wins)
   (ties :initarg :ties :reader comparison-ties)
   (losses :initarg :losses :reader comparison-losses)
   (mean-delta :initarg :mean-delta :reader comparison-mean-delta)
   (token-delta :initarg :token-delta :reader comparison-token-delta)
   (cost-delta :initarg :cost-delta :reader comparison-cost-delta)
   (wall-delta :initarg :wall-delta :reader comparison-wall-delta)
   (only-baseline :initarg :only-baseline :reader comparison-only-baseline)
   (only-candidate :initarg :only-candidate :reader comparison-only-candidate))
  (:documentation "Two runs side by side, task by task."))

(defmethod print-object ((c run-comparison) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~a vs ~a: ~d/~d/~d" (comparison-baseline-id c) (comparison-candidate-id c)
            (comparison-wins c) (comparison-ties c) (comparison-losses c))))

(defun %stats-by-id (run)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (s (run-task-stats run) table)
      (setf (gethash (getf s :task-id) table) s))))

(defun %sorted-keys (table)
  (sort (loop for k being the hash-keys of table collect k) #'string<))

(defun %tokens (totals)
  (let ((i (getf totals :input-tokens))
        (o (getf totals :output-tokens)))
    (and (or i o) (+ (or i 0) (or o 0)))))

(defun compare-runs (baseline candidate)
  "BASELINE and CANDIDATE task by task (see the header) → RUN-COMPARISON."
  (let ((bd (run-dataset-digest baseline))
        (cd (run-dataset-digest candidate)))
    (unless (equal bd cd)
      (restart-case (error 'dataset-mismatch :baseline bd :candidate cd
                                             :message "the runs' dataset digests differ")
        (continue ()
          :report "Compare the task ids both runs have."
          nil))))
  (let* ((bs (%stats-by-id baseline))
         (cs (%stats-by-id candidate))
         (rows (loop for id in (%sorted-keys bs)
                     for b = (gethash id bs)
                     for c = (gethash id cs)
                     when c
                       collect (let* ((bm (getf b :mean-reward))
                                      (cm (getf c :mean-reward))
                                      (d (- cm bm)))
                                 (list :task-id id :baseline-mean bm :candidate-mean cm :delta d
                                       :baseline-passed (getf b :passed)
                                       :candidate-passed (getf c :passed)
                                       :baseline-n (getf b :n) :candidate-n (getf c :n)
                                       :outcome (cond ((plusp d) :win) ((zerop d) :tie) (t :loss))))))
         (bt (run-totals baseline))
         (ct (run-totals candidate)))
    (flet ((delta (a b) (and a b (- b a)))
           (count-outcome (o) (count o rows :key (lambda (r) (getf r :outcome)))))
      (make-instance 'run-comparison
                     :baseline-id (run-id baseline) :candidate-id (run-id candidate)
                     :rows rows
                     :wins (count-outcome :win) :ties (count-outcome :tie)
                     :losses (count-outcome :loss)
                     :mean-delta (if rows (%mean (mapcar (lambda (r) (getf r :delta)) rows)) 0)
                     :token-delta (delta (%tokens bt) (%tokens ct))
                     :cost-delta (delta (getf bt :cost-usd) (getf ct :cost-usd))
                     :wall-delta (- (getf ct :wall-ms) (getf bt :wall-ms))
                     :only-baseline (remove-if (lambda (id) (gethash id cs)) (%sorted-keys bs))
                     :only-candidate (remove-if (lambda (id) (gethash id bs)) (%sorted-keys cs))))))

(defun %comparison-sexp (c)
  (list :run-comparison
        :baseline (comparison-baseline-id c) :candidate (comparison-candidate-id c)
        :wins (comparison-wins c) :ties (comparison-ties c) :losses (comparison-losses c)
        :mean-delta (comparison-mean-delta c) :token-delta (comparison-token-delta c)
        :cost-delta (comparison-cost-delta c) :wall-delta (comparison-wall-delta c)
        :only-baseline (comparison-only-baseline c)
        :only-candidate (comparison-only-candidate c)
        :rows (comparison-rows c)))

(defun %f (x) (float x 1d0))

(defun %comparison-markdown (c)
  (with-output-to-string (s)
    (format s "# agent-eval comparison: ~a vs ~a~%~%"
            (comparison-baseline-id c) (comparison-candidate-id c))
    (format s "| task | baseline | candidate | delta |~%")
    (format s "|------|----------|-----------|-------|~%")
    (dolist (r (comparison-rows c))
      (format s "| ~a | ~,2F | ~,2F | ~,2@F |~%" (getf r :task-id)
              (%f (getf r :baseline-mean)) (%f (getf r :candidate-mean)) (%f (getf r :delta))))
    (format s "~%Wins ~d, ties ~d, losses ~d; mean delta ~,2@F~%"
            (comparison-wins c) (comparison-ties c) (comparison-losses c)
            (%f (comparison-mean-delta c)))
    (let ((tokens (comparison-token-delta c))
          (cost (comparison-cost-delta c)))
      (format s "Tokens ~a; cost ~a; wall ~@D ms~%"
              (if tokens (format nil "~@D" tokens) "n/a")
              (if cost (format nil "~,4@F" (%f cost)) "n/a")
              (comparison-wall-delta c)))))

(defun comparison-report (comparison &key (format :markdown))
  "COMPARISON as :MARKDOWN (default), :SEXP or :JSON (see the header)."
  (ecase format
    (:markdown (%comparison-markdown comparison))
    (:sexp (%comparison-sexp comparison))
    (:json (%json-text (rest (%comparison-sexp comparison))))))
