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

(defun %grade-plist (grade)
  (and grade
       (list :reward (grade-reward grade) :rewards (grade-rewards grade)
             :source (grade-source grade) :exit-code (grade-exit-code grade)
             :timed-out-p (grade-timed-out-p grade) :duration-ms (grade-duration-ms grade))))

(defun trial-plist (trial)
  "TRIAL as pure data (see the header)."
  (let ((ctx (trial-context trial)))
    (flet ((c (reader) (and ctx (funcall reader ctx))))
      (list :task-id (trial-task-id trial)
            :attempt (trial-attempt trial)
            :agent (trial-agent-name trial)
            :agent-version (trial-agent-version trial)
            :status (trial-status trial)
            :reward (trial-reward trial)
            :passed (and (trial-passed-p trial) t)
            :grade (%grade-plist (trial-grade trial))
            :input-tokens (c #'context-input-tokens)
            :cache-tokens (c #'context-cache-tokens)
            :output-tokens (c #'context-output-tokens)
            :cost-usd (c #'context-cost-usd)
            :turns (c #'context-turns)
            :result-text (c #'context-result-text)
            :status-detail (c #'context-status-detail)
            :wall-ms (trial-wall-ms trial)
            :timings (copy-list (trial-timings trial))
            :started-at (trial-started-at trial)
            :finished-at (trial-finished-at trial)
            :directory (trial-directory trial)
            :error (trial-error trial)))))

(defun %run-sexp (run)
  (let ((ds (ev:eval-run-dataset run)))
    (list :agent-eval-run
          :id (run-id run)
          :label (run-label run)
          :agent (run-agent-name run)
          :agent-config (run-agent-config run)
          :dataset (list :name (and ds (ev:eval-dataset-name ds))
                         :version (and ds (ev:eval-dataset-version ds))
                         :digest (run-dataset-digest run))
          :attempts (run-attempts run)
          :n (ev:eval-run-n run)
          :mean (ev:eval-run-mean run)
          :pass-count (ev:eval-run-pass-count run)
          :accuracy (run-accuracy run)
          :pass-at-k (loop for k from 1 to (run-attempts run)
                           collect (cons k (run-pass-at-k run k)))
          :totals (run-totals run)
          :tasks (run-task-stats run)
          :trials (mapcar #'trial-plist (run-trials run)))))

(defmethod ev:eval-report ((run agent-eval-run) &key (format :sexp))
  (ecase format
    ((:sexp :lisp) (%run-sexp run))
    (:json (%json-text (rest (%run-sexp run))))))

;;; save / load

(defun %trial-subdirectory (directory trial)
  (merge-pathnames (format nil "trials/~a/~d/" (trial-task-id trial) (trial-attempt trial))
                   directory))

(defun %write-readably (pathname form)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* t))
        (prin1 form s)
        (terpri s)))))

(defun %read-data (pathname)
  "The one form in PATHNAME, read as data (*READ-EVAL* NIL)."
  (with-open-file (s pathname :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (read s)))))

(defun save-run (run directory)
  "Write RUN under DIRECTORY (see the header) → DIRECTORY's truename."
  (let* ((dir (%directory-truename directory))
         (ds (ev:eval-run-dataset run))
         (cases (and ds (ev:eval-dataset-cases ds))))
    (%write-readably (merge-pathnames "run.sexp" dir)
                     (append (ev:eval-report run)
                             (list :specs (mapcar #'task-spec cases)
                                   :role (and cases (ev:eval-case-role (first cases)))
                                   :provenance (and ds (ev:eval-dataset-provenance ds))
                                   :started-at (run-started-at run)
                                   :finished-at (run-finished-at run))))
    (%write-text-file (merge-pathnames "result.json" dir) (ev:eval-report run :format :json))
    (dolist (trial (run-trials run))
      (let ((tdir (%trial-subdirectory dir trial)))
        (%write-readably (merge-pathnames "trial.sexp" tdir) (trial-plist trial))
        (%write-text-file (merge-pathnames "transcript.jsonl" tdir)
                          (with-output-to-string (s)
                            (dolist (event (trial-transcript trial))
                              (write-string (json:encode event) s)
                              (terpri s))))))
    dir))

(defun %task-from-spec (spec role)
  (ev:make-eval-case :input spec
                     :metadata (list :kind :tb-task :id (getf spec :id)
                                     :files-sha256 (getf spec :files-sha256))
                     :source :terminal-bench
                     :role role))

(defun %grade-from-plist (plist)
  (and plist
       (%make-grade :reward (getf plist :reward) :rewards (getf plist :rewards)
                    :source (getf plist :source) :exit-code (getf plist :exit-code)
                    :timed-out-p (getf plist :timed-out-p)
                    :duration-ms (getf plist :duration-ms))))

(defun %trial-from-plist (plist transcript)
  (let ((ctx (make-agent-context)))
    (setf (context-input-tokens ctx) (getf plist :input-tokens)
          (context-cache-tokens ctx) (getf plist :cache-tokens)
          (context-output-tokens ctx) (getf plist :output-tokens)
          (context-cost-usd ctx) (getf plist :cost-usd)
          (context-turns ctx) (getf plist :turns)
          (context-result-text ctx) (getf plist :result-text)
          (context-status-detail ctx) (getf plist :status-detail)
          (context-transcript ctx) transcript)
    (make-trial :task-id (getf plist :task-id) :attempt (getf plist :attempt)
                :agent-name (getf plist :agent) :agent-version (getf plist :agent-version)
                :status (getf plist :status) :context ctx
                :grade (%grade-from-plist (getf plist :grade))
                :reward (getf plist :reward) :passed-p (getf plist :passed)
                :timings (getf plist :timings) :wall-ms (getf plist :wall-ms)
                :started-at (getf plist :started-at) :finished-at (getf plist :finished-at)
                :directory (getf plist :directory) :error (getf plist :error))))

(defun %read-transcript (pathname)
  (if (probe-file pathname)
      (parse-stream-json (%read-text-file pathname))
      '()))

(defun load-run (directory)
  "The AGENT-EVAL-RUN saved under DIRECTORY (see the header)."
  (let* ((dir (uiop:ensure-directory-pathname directory))
         (file (merge-pathnames "run.sexp" dir)))
    (unless (probe-file file)
      (error 'run-not-found :path (uiop:native-namestring file)
                            :message "no run.sexp"))
    (let* ((form (%read-data file))
           (body (if (and (consp form) (eq (first form) :agent-eval-run))
                     (rest form)
                     (error 'run-not-found :path (uiop:native-namestring file)
                                           :message "run.sexp is not an agent-eval run")))
           (role (getf body :role))
           (dataset-info (getf body :dataset))
           (dataset (ev:make-eval-dataset
                     :name (or (getf dataset-info :name) "unnamed")
                     :cases (mapcar (lambda (spec) (%task-from-spec spec role))
                                    (getf body :specs))
                     :provenance (getf body :provenance)))
           (trials (mapcar (lambda (plist)
                             (%trial-from-plist
                              plist
                              (%read-transcript
                               (merge-pathnames "transcript.jsonl"
                                                (merge-pathnames
                                                 (format nil "trials/~a/~d/"
                                                         (getf plist :task-id) (getf plist :attempt))
                                                 dir)))))
                           (getf body :trials)))
           (results (mapcar (lambda (trial)
                              (%trial-result (find-task dataset (trial-task-id trial)) trial))
                            trials)))
      (make-agent-eval-run :dataset dataset :results results :trials trials
                           :id (getf body :id) :label (getf body :label)
                           :agent-name (getf body :agent) :agent-config (getf body :agent-config)
                           :attempts (getf body :attempts)
                           :started-at (getf body :started-at)
                           :finished-at (getf body :finished-at)
                           :dataset-digest (getf dataset-info :digest)
                           :n (getf body :n) :mean (getf body :mean)
                           :pass-count (getf body :pass-count)))))
