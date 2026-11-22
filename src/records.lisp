(in-package #:agent-eval)

;;; Results as eval-protocol records (E0). Spec: tests/records-test.lisp.
;;;
;;; TRIAL-PLIST trial → (:task-id :attempt :agent :agent-version :status
;;;   :reward :passed :grade G :input-tokens :cache-tokens :output-tokens
;;;   :cost-usd :turns :result-text :status-detail :wall-ms :timings
;;;   :started-at :finished-at :directory :error), in this order; G is NIL or
;;;   (:reward :rewards :source :exit-code :timed-out-p :duration-ms).
;;;   Pure data: strings, numbers, keywords, lists.
;;;
;;; EV:EVAL-REPORT on an AGENT-EVAL-RUN (a method; eval-protocol's own GF):
;;;   :FORMAT :SEXP (default) → (:agent-eval-run :id :label :agent
;;;     :agent-config :dataset (:name :version :digest) :attempts :n :mean
;;;     :pass-count :accuracy :pass-at-k ((1 . P1) … (ATTEMPTS . Pk)) :totals
;;;     (RUN-TOTALS) :tasks (RUN-TASK-STATS) :trials (TRIAL-PLIST …)), in this
;;;     order. :N :MEAN :PASS-COUNT are EV:EVAL-RUN's.
;;;   :FORMAT :JSON → one JSON object (json-protocol ENCODE) with the same
;;;     members: keys are the keyword names downcased with - → _ ("pass_at_k"
;;;     is an object keyed "1" … "k"), keyword values downcased strings,
;;;     rationals as floats; the flags :PASSED, :COST-COMPLETE-P and :TIMED-OUT-P
;;;     as true / false, every other NIL as null.
;;;
;;; SAVE-RUN run directory → DIRECTORY's truename, after writing:
;;;   run.sexp      the :SEXP report plus :specs (each task's TASK-SPEC, in
;;;                 dataset order), printed readably (WITH-STANDARD-IO-SYNTAX)
;;;   result.json   the :JSON report
;;;   trials/<task-id>/<attempt>/trial.sexp        TRIAL-PLIST
;;;   trials/<task-id>/<attempt>/transcript.jsonl  one JSON object per line
;;;                                                (the trial's transcript)
;;; LOAD-RUN directory → an AGENT-EVAL-RUN whose :SEXP report is EQUAL to the
;;; saved one and whose trials' transcripts are EQUALP-equal JSON values.
;;; The cases are rebuilt from :specs (no task directory is read). No
;;; run.sexp → RUN-NOT-FOUND (PATH its native namestring). Read with
;;; *READ-EVAL* NIL.

(defun trial-plist (trial)
  (declare (ignore trial))
  (%e0-pending 'trial-plist))

(defun save-run (run directory)
  (declare (ignore run directory))
  (%e0-pending 'save-run))

(defun load-run (directory)
  (declare (ignore directory))
  (%e0-pending 'load-run))
