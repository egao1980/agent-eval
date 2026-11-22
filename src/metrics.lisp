(in-package #:agent-eval)

;;; Metrics (E0). Spec: tests/metrics-test.lisp.
;;;
;;; PASS-AT-K n c k → the unbiased pass@k estimator (Chen et al. 2021,
;;; "Evaluating Large Language Models Trained on Code"): 1 − C(n−c, k) / C(n, k),
;;; 1 when n − c < k. Exact (a rational). N, C, K integers with 0 <= C <= N and
;;; 1 <= K <= N, else AGENT-EVAL-ERROR.
;;; RUN-TASK-STATS run → one plist per task that has trials, in dataset order:
;;;   (:task-id ID :n TRIALS :passed PASSED :mean-reward MEAN :pass-at-1 P)
;;;   MEAN the mean of rewards clamped to [0, 1]; P = PASSED / N.
;;; RUN-PASS-AT-K run k → the mean of PASS-AT-K over the tasks with N >= K
;;;   (exact); NIL when there is none.
;;; RUN-ACCURACY run → the mean clamped reward over all trials (Terminal-Bench's
;;;   accuracy); NIL when there are no trials.
;;; RUN-TOTALS run → (:trials N :input-tokens I :cache-tokens C :output-tokens
;;;   O :cost-usd D :cost-complete-p B :wall-ms W :agent-ms A). I, C, O and D
;;;   sum the trials whose context has that value (NIL when none has);
;;;   COST-COMPLETE-P is T when every trial has a cost. W sums TRIAL-WALL-MS,
;;;   A the :AGENT-EXECUTION timings.

(defun pass-at-k (n c k)
  (declare (ignore n c k))
  (%e0-pending 'pass-at-k))

(defun run-task-stats (run)
  (declare (ignore run))
  (%e0-pending 'run-task-stats))

(defun run-pass-at-k (run k)
  (declare (ignore run k))
  (%e0-pending 'run-pass-at-k))

(defun run-accuracy (run)
  (declare (ignore run))
  (%e0-pending 'run-accuracy))

(defun run-totals (run)
  (declare (ignore run))
  (%e0-pending 'run-totals))
