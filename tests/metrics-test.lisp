(in-package #:agent-eval/tests)

;;; Spec for src/metrics.lisp. Synthetic runs: no environment needed.

(deftest pass-at-k-estimator
  (ok (= 0 (pass-at-k 5 0 1)))
  (ok (= 1 (pass-at-k 5 5 1)))
  (ok (= 2/5 (pass-at-k 5 2 1)) "pass@1 = c / n")
  (testing "1 − C(n−c,k)/C(n,k)"
    (ok (= (- 1 (/ 3 10)) (pass-at-k 5 2 2)))   ; C(3,2)=3, C(5,2)=10
    (ok (= (- 1 (/ 1 10)) (pass-at-k 5 2 3))))  ; C(3,3)=1, C(5,3)=10
  (ok (= 1 (pass-at-k 5 2 4)) "n − c < k")
  (ok (rationalp (pass-at-k 7 3 2)))
  (ok (signals (pass-at-k 3 4 1) 'agent-eval-error))
  (ok (signals (pass-at-k 3 1 4) 'agent-eval-error))
  (ok (signals (pass-at-k 3 1 0) 'agent-eval-error))
  (ok (signals (pass-at-k 3 -1 1) 'agent-eval-error)))

(defun two-task-run ()
  (let ((ds (fixture-dataset)))
    (fake-run ds (list (fake-trial "hello-world" 1 1 :cost 1/100 :cache 10 :wall 1000)
                       (fake-trial "hello-world" 2 0 :cost 2/100 :wall 3000)
                       (fake-trial "partial-credit" 1 1/2 :cost nil :wall 2000))
              :attempts 2)))

(deftest task-stats
  (ok (equal '((:task-id "hello-world" :n 2 :passed 1 :mean-reward 1/2 :pass-at-1 1/2)
               (:task-id "partial-credit" :n 1 :passed 0 :mean-reward 1/2 :pass-at-1 0))
             (run-task-stats (two-task-run)))))

(deftest run-level-metrics
  (let ((run (two-task-run)))
    (ok (= 1/2 (run-accuracy run)) "mean clamped reward over trials")
    (ok (= 1/2 (run-pass-at-k run 1)) "mean over tasks of pass@1")
    (ok (= 1 (run-pass-at-k run 2)) "only tasks with n >= 2 count")
    (ok (null (run-pass-at-k run 3)))
    (ok (null (run-accuracy (fake-run (fixture-dataset) '()))))))

(deftest totals
  (let ((totals (run-totals (two-task-run))))
    (ok (equal '(:trials :input-tokens :cache-tokens :output-tokens :cost-usd
                 :cost-complete-p :wall-ms :agent-ms)
               (loop for (k nil) on totals by #'cddr collect k)))
    (ok (= 3 (getf totals :trials)))
    (ok (= 300 (getf totals :input-tokens)))
    (ok (= 10 (getf totals :cache-tokens)))
    (ok (= 60 (getf totals :output-tokens)))
    (ok (= 3/100 (getf totals :cost-usd)))
    (ok (null (getf totals :cost-complete-p)))
    (ok (= 6000 (getf totals :wall-ms)))
    (ok (= 3000 (getf totals :agent-ms))))
  (let ((totals (run-totals (fake-run (fixture-dataset)
                                      (list (fake-trial "hello-world" 1 1 :input nil :output nil))))))
    (ok (null (getf totals :input-tokens)) "no trial knows → NIL")
    (ok (null (getf totals :cost-usd)))))
