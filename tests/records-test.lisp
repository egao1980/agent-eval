(in-package #:agent-eval/tests)

;;; Spec for src/records.lisp: eval-protocol records, saved and loaded.

(deftest trial-plist-shape
  (let ((p (trial-plist (fake-trial "hello-world" 1 1 :cost 1/100))))
    (ok (equal '(:task-id :attempt :agent :agent-version :status :reward :passed :grade
                 :input-tokens :cache-tokens :output-tokens :cost-usd :turns :result-text
                 :status-detail :wall-ms :timings :started-at :finished-at :directory :error)
               (loop for (k nil) on p by #'cddr collect k)))
    (ok (equal "hello-world" (getf p :task-id)))
    (ok (eq :completed (getf p :status)))
    (ok (eq t (getf p :passed)))
    (ok (= 1/100 (getf p :cost-usd)))
    (ok (null (getf p :grade)))))

(deftest sexp-report
  (let* ((run (two-task-run))
         (r (ev:eval-report run)))
    (ok (eq :agent-eval-run (first r)))
    (ok (equal '(:id :label :agent :agent-config :dataset :attempts :n :mean :pass-count
                 :accuracy :pass-at-k :totals :tasks :trials)
               (loop for (k nil) on (rest r) by #'cddr collect k)))
    (ok (equal "run-a" (getf (rest r) :id)))
    (ok (equal (list :name "fixtures"
                     :version (ev:eval-dataset-version (ev:eval-run-dataset run))
                     :digest (dataset-digest (ev:eval-run-dataset run)))
               (getf (rest r) :dataset)))
    (ok (equal '((1 . 1/2) (2 . 1)) (getf (rest r) :pass-at-k)))
    (ok (= 1/2 (getf (rest r) :accuracy)))
    (ok (= 3 (length (getf (rest r) :trials))))
    (ok (equal (run-totals run) (getf (rest r) :totals)))
    (ok (equal (run-task-stats run) (getf (rest r) :tasks)))))

(deftest json-report
  (let* ((run (two-task-run))
         (o (json:decode (ev:eval-report run :format :json))))
    (ok (hash-table-p o))
    (ok (equal "run-a" (gethash "id" o)))
    (ok (= 0.5 (gethash "accuracy" o)))
    (ok (= 0.5 (json-get o "pass_at_k" "1")))
    (ok (= 1 (json-get o "pass_at_k" "2")))
    (ok (= 300 (json-get o "totals" "input_tokens")))
    (testing "a NIL flag is false, other NILs are null"
      (multiple-value-bind (v present) (gethash "cost_complete_p" (gethash "totals" o))
        (ok (and present (null v))))
      (ok (eq :null (gethash "label" o))))
    (let ((trial (aref (gethash "trials" o) 0)))
      (ok (equal "hello-world" (gethash "task_id" trial)))
      (ok (equal "completed" (gethash "status" trial)))
      (ok (eq t (gethash "passed" trial))))))

(deftest save-and-load-round-trip
  (with-tmpdir (dir)
    (let* ((run (two-task-run))
           (where (save-run run (path-in dir "runs/run-a/"))))
      (ok (equal (truename (path-in dir "runs/run-a/")) where))
      (ok (probe-file (path-in dir "runs/run-a/run.sexp")))
      (ok (hash-table-p (json:decode (read-file dir "runs/run-a/result.json"))))
      (ok (probe-file (path-in dir "runs/run-a/trials/hello-world/2/trial.sexp")))
      (testing "transcripts as JSONL, one object per line"
        (let ((lines (remove "" (uiop:split-string
                                 (read-file dir "runs/run-a/trials/hello-world/1/transcript.jsonl")
                                 :separator '(#\Newline))
                             :test #'string=)))
          (ok (= 1 (length lines)))
          (ok (equal "result" (gethash "type" (json:decode (first lines)))))))
      (testing "run.sexp is plain data, readable without *read-eval*"
        (let ((form (with-open-file (s (path-in dir "runs/run-a/run.sexp"))
                      (with-standard-io-syntax
                        (let ((*read-eval* nil)) (read s))))))
          (ok (eq :agent-eval-run (first form)))
          (ok (= 2 (length (getf (rest form) :specs))))))
      (let ((back (load-run (path-in dir "runs/run-a/"))))
        (ok (typep back 'agent-eval-run))
        (ok (equal (ev:eval-report run) (ev:eval-report back)))
        (ok (equalp (trial-transcript (first (run-trials run)))
                    (trial-transcript (first (run-trials back)))))
        (ok (every #'tb-task-p (ev:eval-dataset-cases (ev:eval-run-dataset back))))
        (ok (equal (run-dataset-digest run) (run-dataset-digest back)))))))

(deftest load-run-not-found
  (with-tmpdir (dir)
    (ok (signals (load-run (path-in dir "nothing/")) 'run-not-found))))

(deftest real-run-round-trip
  (local-lane
   (with-tmpdir (dir)
     (let* ((run (run-agent-eval (fixture-dataset)
                                 (make-cl-stack-code-agent :command (fake-cl-stack-code-command))
                                 :directory (path-in dir "work/")))
            (back (load-run (save-run run (path-in dir "saved/")))))
       (ok (equal (ev:eval-report run) (ev:eval-report back)))
       (let ((g (trial-grade (first (run-trials back)))))
         (ok (typep g 'grade))
         (ok (eq :reward-txt (grade-source g))))))))
