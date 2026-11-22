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

(defclass run-comparison () ()
  (:documentation "Two runs side by side (stub)."))

(defun compare-runs (baseline candidate)
  (declare (ignore baseline candidate))
  (%e0-pending 'compare-runs))

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

(defun comparison-report (comparison &key (format :markdown))
  (declare (ignore comparison format))
  (%e0-pending 'comparison-report))
