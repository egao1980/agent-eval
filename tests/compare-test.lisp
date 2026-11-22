(in-package #:agent-eval/tests)

;;; Spec for src/compare.lisp: comparing two runs.

(defun baseline-run ()
  (fake-run (fixture-dataset)
            (list (fake-trial "hello-world" 1 0 :input 100 :output 20 :cost 1/100 :wall 1000)
                  (fake-trial "partial-credit" 1 1/2 :input 100 :output 20 :cost 1/100 :wall 1000))
            :id "base"))

(defun candidate-run ()
  (fake-run (fixture-dataset)
            (list (fake-trial "hello-world" 1 1 :input 150 :output 30 :cost 2/100 :wall 1500)
                  (fake-trial "partial-credit" 1 1/2 :input 50 :output 10 :cost 1/100 :wall 500))
            :id "cand"))

(deftest comparison-rows-and-counts
  (let ((c (compare-runs (baseline-run) (candidate-run))))
    (ok (typep c 'run-comparison))
    (ok (equal "base" (comparison-baseline-id c)))
    (ok (equal "cand" (comparison-candidate-id c)))
    (ok (equal '((:task-id "hello-world" :baseline-mean 0 :candidate-mean 1 :delta 1
                  :baseline-passed 0 :candidate-passed 1 :baseline-n 1 :candidate-n 1
                  :outcome :win)
                 (:task-id "partial-credit" :baseline-mean 1/2 :candidate-mean 1/2 :delta 0
                  :baseline-passed 0 :candidate-passed 0 :baseline-n 1 :candidate-n 1
                  :outcome :tie))
               (comparison-rows c)))
    (ok (= 1 (comparison-wins c)))
    (ok (= 1 (comparison-ties c)))
    (ok (= 0 (comparison-losses c)))
    (ok (= 1/2 (comparison-mean-delta c)))
    (ok (= (- 240 240) (comparison-token-delta c)))
    (ok (= 1/100 (comparison-cost-delta c)))
    (ok (= 0 (comparison-wall-delta c)))
    (ok (null (comparison-only-baseline c)))
    (ok (null (comparison-only-candidate c))))
  (testing "a loss, seen from the other side"
    (let ((c (compare-runs (candidate-run) (baseline-run))))
      (ok (= 1 (comparison-losses c)))
      (ok (= -1/2 (comparison-mean-delta c))))))

(deftest unknown-tokens-and-cost
  (let* ((ds (fixture-dataset))
         (a (fake-run ds (list (fake-trial "hello-world" 1 1 :input nil :output nil)) :id "a"))
         (b (fake-run ds (list (fake-trial "hello-world" 1 1)) :id "b"))
         (c (compare-runs a b)))
    (ok (null (comparison-token-delta c)))
    (ok (null (comparison-cost-delta c)))))

(deftest tasks-on-one-side
  (let* ((ds (fixture-dataset))
         (a (fake-run ds (list (fake-trial "hello-world" 1 1)) :id "a"))
         (b (fake-run ds (list (fake-trial "partial-credit" 1 1)) :id "b"))
         (c (compare-runs a b)))
    (ok (null (comparison-rows c)))
    (ok (= 0 (comparison-mean-delta c)))
    (ok (equal '("hello-world") (comparison-only-baseline c)))
    (ok (equal '("partial-credit") (comparison-only-candidate c)))))

(deftest dataset-mismatch-and-continue
  (with-tmpdir (root)
    (copy-tree-to (tb-fixture "hello-world") (path-in root "hello-world/"))
    (write-file root "hello-world/instruction.md" "Something else.
")
    (let* ((other (load-tb-dataset root))
           (a (baseline-run))
           (b (fake-run other (list (fake-trial "hello-world" 1 1)) :id "other")))
      (ok (signals (compare-runs a b) 'dataset-mismatch))
      (handler-case (compare-runs a b)
        (dataset-mismatch (e)
          (ok (equal (run-dataset-digest a) (dataset-mismatch-baseline e)))
          (ok (equal (run-dataset-digest b) (dataset-mismatch-candidate e)))))
      (let ((c (handler-bind ((dataset-mismatch
                                (lambda (e) (declare (ignore e)) (invoke-restart 'continue))))
                 (compare-runs a b))))
        (ok (equal '("hello-world") (mapcar (lambda (r) (getf r :task-id)) (comparison-rows c))))
        (ok (= 1 (comparison-wins c)))))))

(deftest reports
  (let ((c (compare-runs (baseline-run) (candidate-run))))
    (testing ":sexp"
      (let ((r (comparison-report c :format :sexp)))
        (ok (eq :run-comparison (first r)))
        (ok (equal '(:baseline :candidate :wins :ties :losses :mean-delta :token-delta
                     :cost-delta :wall-delta :only-baseline :only-candidate :rows)
                   (loop for (k nil) on (rest r) by #'cddr collect k)))
        (ok (equal (comparison-rows c) (getf (rest r) :rows)))))
    (testing ":json"
      (let ((o (json:decode (comparison-report c :format :json))))
        (ok (equal "base" (gethash "baseline" o)))
        (ok (= 1 (gethash "wins" o)))
        (ok (equal "win" (gethash "outcome" (aref (gethash "rows" o) 0))))))
    (testing ":markdown (the default)"
      (let* ((md (comparison-report c))
             (lines (uiop:split-string md :separator '(#\Newline))))
        (ok (equal "# agent-eval comparison: base vs cand" (first lines)))
        (ok (member "| task | baseline | candidate | delta |" lines :test #'string=))
        (ok (member "| hello-world | 0.00 | 1.00 | +1.00 |" lines :test #'string=))
        (ok (member "| partial-credit | 0.50 | 0.50 | +0.00 |" lines :test #'string=))
        (ok (member "Wins 1, ties 1, losses 0; mean delta +0.50" lines :test #'string=))
        (ok (member "Tokens +0; cost +0.0100; wall +0 ms" lines :test #'string=))))))

(deftest promotion-gate-on-agent-runs
  (testing "eval-protocol's gates work on agent runs unchanged"
    (ok (ev:gate-passes-p (ev:make-mean-improvement-gate :delta 0)
                          (baseline-run) (candidate-run)))
    (ok (not (ev:gate-passes-p (ev:make-mean-improvement-gate :delta 0)
                               (candidate-run) (baseline-run))))))
