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
;;; RUN-PASS-AT-K run k → the mean over the tasks with N >= K trials of that
;;;   task's pass@k, where C is the SUM OF ITS CLAMPED REWARDS (each reward
;;;   clamped to [0, 1]), so partial credit counts: an integer C gives
;;;   PASS-AT-K N C K; a fractional C is linear between PASS-AT-K at floor(C)
;;;   and at ceiling(C). With 0/1 rewards it is the plain estimator; pass@1 is
;;;   the task's mean clamped reward (one trial with reward 1/2 → pass@1 =
;;;   1/2, where counting passed trials would give 0). Exact; NIL when no
;;;   task has K trials.
;;; RUN-ACCURACY run → the mean clamped reward over all trials (Terminal-Bench's
;;;   accuracy); NIL when there are no trials.
;;; RUN-TOTALS run → (:trials N :input-tokens I :cache-tokens C :output-tokens
;;;   O :cost-usd D :cost-complete-p B :wall-ms W :agent-ms A). I, C, O and D
;;;   sum the trials whose context has that value (NIL when none has);
;;;   COST-COMPLETE-P is T when every trial has a cost. W sums TRIAL-WALL-MS,
;;;   A the :AGENT-EXECUTION timings.

(defun %binomial (n k)
  "C(N, K) for integers N >= 0, K >= 0 (0 when K > N)."
  (if (> k n)
      0
      (let ((k (min k (- n k)))
            (acc 1))
        (loop for i from 1 to k
              do (setf acc (/ (* acc (- n (- k i))) i)))
        acc)))

(defun pass-at-k (n c k)
  "The unbiased pass@k estimator 1 − C(n−c, k) / C(n, k) (Chen et al. 2021),
exact; 1 when n − c < k."
  (unless (and (integerp n) (integerp c) (integerp k) (<= 0 c n) (<= 1 k n))
    (error 'agent-eval-error
           :message (format nil "pass@k needs integers 0 <= c <= n and 1 <= k <= n, not n=~s c=~s k=~s"
                            n c k)))
  (if (< (- n c) k)
      1
      (- 1 (/ (%binomial (- n c) k) (%binomial n k)))))

(defun %task-pass-at-k (n c k)
  "pass@k for a task whose successes C may be fractional (the sum of clamped
rewards): linear between PASS-AT-K at floor(C) and at ceiling(C). For 0/1
rewards it is PASS-AT-K itself; pass@1 is the mean reward."
  (multiple-value-bind (lo frac) (floor c)
    (if (zerop frac)
        (pass-at-k n lo k)
        (+ (* (- 1 frac) (pass-at-k n lo k))
           (* frac (pass-at-k n (min n (1+ lo)) k))))))

(defun %mean (xs)
  (and xs (/ (reduce #'+ xs) (length xs))))

(defun %task-trials (run task)
  (remove-if-not (lambda (tr) (equal (task-id task) (trial-task-id tr))) (run-trials run)))

(defun %run-task-groups (run)
  "(TASK . TRIALS) for each task with trials, in dataset order."
  (loop for task in (ev:eval-dataset-cases (ev:eval-run-dataset run))
        for trials = (%task-trials run task)
        when trials collect (cons task trials)))

(defun run-task-stats (run)
  "One plist per task with trials, in dataset order (see the header)."
  (loop for (task . trials) in (%run-task-groups run)
        for n = (length trials)
        for passed = (count-if #'trial-passed-p trials)
        collect (list :task-id (task-id task) :n n :passed passed
                      :mean-reward (%mean (mapcar (lambda (tr) (%clamp-reward (trial-reward tr)))
                                                  trials))
                      :pass-at-1 (/ passed n))))

(defun run-pass-at-k (run k)
  "The mean pass@k over the tasks with at least K trials (exact), or NIL."
  (%mean (loop for (nil . trials) in (%run-task-groups run)
               for n = (length trials)
               when (>= n k)
                 collect (%task-pass-at-k
                          n (reduce #'+ trials :key (lambda (tr) (%clamp-reward (trial-reward tr))))
                          k))))

(defun run-accuracy (run)
  "The mean clamped reward over all trials, or NIL with none."
  (%mean (mapcar (lambda (tr) (%clamp-reward (trial-reward tr))) (run-trials run))))

(defun run-totals (run)
  "Token, cost and time totals over RUN's trials (see the header)."
  (let ((trials (run-trials run)))
    (flet ((sum (reader)
             (let ((vs (loop for tr in trials
                             for ctx = (trial-context tr)
                             for v = (and ctx (funcall reader ctx))
                             when v collect v)))
               (and vs (reduce #'+ vs)))))
      (list :trials (length trials)
            :input-tokens (sum #'context-input-tokens)
            :cache-tokens (sum #'context-cache-tokens)
            :output-tokens (sum #'context-output-tokens)
            :cost-usd (sum #'context-cost-usd)
            :cost-complete-p (and trials
                                  (every (lambda (tr)
                                           (let ((ctx (trial-context tr)))
                                             (and ctx (context-cost-usd ctx))))
                                         trials)
                                  t)
            :wall-ms (reduce #'+ trials :key (lambda (tr) (or (trial-wall-ms tr) 0)))
            :agent-ms (reduce #'+ trials
                              :key (lambda (tr)
                                     (or (getf (trial-timings tr) :agent-execution) 0)))))))
