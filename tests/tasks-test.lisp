(in-package #:agent-eval/tests)

;;; Spec for src/tasks.lisp: the Terminal-Bench task loader.

(deftest tb-task-is-a-plain-eval-case
  (let ((task (hello-task)))
    (ok (eq (class-of task) (find-class 'ev:eval-case)))
    (ok (tb-task-p task))
    (ok (not (tb-task-p (ev:make-eval-case :input '(:id "x")))))
    (ok (eq :terminal-bench (ev:eval-case-source task)))
    (ok (equal (list :kind :tb-task :id "hello-world"
                     :files-sha256 (task-files-sha256 (tb-fixture "hello-world")))
               (ev:eval-case-metadata task)))))

(deftest spec-keys-in-canonical-order
  (ok (equal '(:id :name :instruction :directory :files-sha256 :image :dockerfile-p
               :compose-p :workdir :agent-timeout :verifier-timeout :build-timeout
               :cpus :memory-mb :storage-mb :gpus :allow-internet :env :verifier-env
               :solution-p :difficulty :category :tags)
             (loop for (k nil) on (task-spec (hello-task)) by #'cddr collect k))))

(deftest hello-world-fields
  (let ((task (hello-task)))
    (ok (equal "hello-world" (task-id task)))
    (ok (equal "agent-eval/hello-world" (task-name task)))
    (ok (search "hello.txt" (task-instruction task)))
    (ok (equal (uiop:native-namestring (truename (tb-fixture "hello-world")))
               (task-directory task)))
    (ok (equal "docker.io/library/debian:bookworm-slim" (task-image task)))
    (ok (task-dockerfile-p task))
    (ok (not (task-compose-p task)))
    (ok (equal "/app" (task-workdir task)))
    (ok (= 30 (task-agent-timeout task)))
    (ok (= 30 (task-verifier-timeout task)))
    (ok (= 120 (task-build-timeout task)))
    (ok (= 1 (task-cpus task)))
    (ok (= 512 (task-memory-mb task)))
    (ok (= 1024 (task-storage-mb task)))
    (ok (= 0 (task-gpus task)))
    (ok (null (task-allow-internet-p task)))
    (ok (equal '(("GREETING" . "Hello, world!")) (task-env task)))
    (ok (equal '(("EXPECTED" . "Hello, world!")) (task-verifier-env task)))
    (ok (task-solution-p task))
    (ok (equal "easy" (task-difficulty task)))
    (ok (equal "fixture" (task-category task)))
    (ok (equal '("fixture" "shell") (task-tags task)))))

(deftest defaults-and-dockerfile-workdir
  (let ((task (partial-task)))
    (ok (null (task-image task)))
    (ok (task-dockerfile-p task))
    (testing "the last absolute WORKDIR wins"
      (ok (equal "/work" (task-workdir task))))
    (ok (= 600 (task-agent-timeout task)))
    (ok (= 600 (task-verifier-timeout task)))
    (ok (= 600 (task-build-timeout task)))
    (ok (= 1 (task-cpus task)))
    (ok (= 2048 (task-memory-mb task)))
    (ok (= 10240 (task-storage-mb task)))
    (ok (= 0 (task-gpus task)))
    (ok (eq t (task-allow-internet-p task)))
    (ok (null (task-env task)))
    (ok (equal '("fixture") (task-tags task))))
  (with-tmpdir (root)
    (testing "no WORKDIR, a relative one, or an image only → /app"
      (ok (equal +default-workdir+
                 (task-workdir (load-tb-task (write-tb-task root "rel" :dockerfile "FROM x
WORKDIR src
")))))
      (ok (equal "/app" (task-workdir (load-tb-task (write-tb-task root "img")))))
      (ok (null (task-solution-p (load-tb-task (write-tb-task root "img2"))))))
    (testing "the id is given, else the directory name; the name defaults to the id"
      (let ((dir (write-tb-task root "named" :toml "[environment]
docker_image = \"i\"
")))
        (ok (equal "named" (task-name (load-tb-task dir))))
        (ok (equal "other" (task-id (load-tb-task dir :id "other"))))))))

(defun format-error-field (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (task-format-error (c) (task-format-error-field c))))

(deftest format-errors
  (with-tmpdir (root)
    (ok (eq :task-toml (format-error-field
                        (lambda () (load-tb-task (write-tb-task root "a" :toml nil))))))
    (ok (eq :task-toml (format-error-field
                        (lambda () (load-tb-task (write-tb-task root "b" :toml "[task
name = 1"))))))
    (ok (eq :instruction (format-error-field
                          (lambda () (load-tb-task (write-tb-task root "c" :instruction nil))))))
    (ok (eq :instruction (format-error-field
                          (lambda () (load-tb-task (write-tb-task root "d" :instruction "

"))))))
    (ok (eq :environment (format-error-field
                          (lambda () (load-tb-task (write-tb-task root "e" :toml "[task]
name = \"x\"
"))))))
    (ok (eq :tests (format-error-field
                    (lambda () (load-tb-task (write-tb-task root "f" :test-sh nil))))))
    (ok (eq :id (format-error-field
                 (lambda () (load-tb-task (write-tb-task root "g") :id "-bad")))))
    (ok (eq :id (format-error-field
                 (lambda () (load-tb-task (write-tb-task root "h") :id "a/b")))))
    (ok (eq :timeout (format-error-field
                      (lambda () (load-tb-task (write-tb-task root "i" :toml (toml-with "[agent]
timeout_sec = 0")))))))
    (ok (eq :timeout (format-error-field
                      (lambda () (load-tb-task (write-tb-task root "j" :toml (toml-with "[verifier]
timeout_sec = \"soon\"")))))))
    (testing "the condition carries the task directory"
      (let ((dir (write-tb-task root "k" :test-sh nil)))
        (handler-case (load-tb-task dir)
          (task-format-error (c)
            (ok (equal (uiop:native-namestring (truename dir))
                       (task-format-error-path c)))
            (ok (typep c 'ev:eval-error))))))))

(deftest compose-only-task-loads
  (with-tmpdir (root)
    (let ((task (load-tb-task (write-tb-task root "multi" :toml "[task]
name = \"m\"
" :compose "services: {}
"))))
      (ok (task-compose-p task))
      (ok (null (task-image task))))))

(deftest files-sha256-is-content-only
  (with-tmpdir (a)
    (with-tmpdir (b)
      (copy-tree-to (tb-fixture "hello-world") (path-in a "x/"))
      (copy-tree-to (tb-fixture "hello-world") (path-in b "y/z/"))
      (let ((ha (task-files-sha256 (path-in a "x/")))
            (hb (task-files-sha256 (path-in b "y/z/"))))
        (ok (= 64 (length ha)))
        (ok (every (lambda (ch) (find ch "0123456789abcdef")) ha))
        (ok (equal ha hb))
        (testing ".git/ is ignored"
          (write-file (path-in a "x/") ".git/HEAD" "ref: refs/heads/main
")
          (ok (equal ha (task-files-sha256 (path-in a "x/")))))
        (testing "an edit changes it"
          (write-file (path-in b "y/z/") "instruction.md" "Something else.
")
          (ok (not (equal ha (task-files-sha256 (path-in b "y/z/"))))))))))

(deftest dataset-loading
  (let ((ds (fixture-dataset)))
    (ok (ev:eval-dataset-p ds))
    (ok (equal "fixtures" (ev:eval-dataset-name ds)))
    (ok (equal '("hello-world" "partial-credit")
               (mapcar #'task-id (ev:eval-dataset-cases ds))))
    (ok (equal "partial-credit" (task-id (find-task ds "partial-credit"))))
    (ok (null (find-task ds "nope")))
    (ok (equal (list (list :terminal-bench
                           (uiop:native-namestring (truename (fixture "tb/")))))
               (ev:eval-dataset-provenance ds))))
  (testing "the name defaults to the directory's"
    (ok (equal "tb" (ev:eval-dataset-name
                     (load-tb-dataset (uiop:ensure-directory-pathname (fixture "tb/")))))))
  (testing "include and exclude"
    (let ((dir (uiop:ensure-directory-pathname (fixture "tb/"))))
      (ok (equal '("partial-credit")
                 (mapcar #'task-id (ev:eval-dataset-cases
                                    (load-tb-dataset dir :include '("partial-credit"))))))
      (ok (equal '("hello-world")
                 (mapcar #'task-id (ev:eval-dataset-cases
                                    (load-tb-dataset dir :exclude '("partial-credit")))))))))

(deftest dataset-restarts
  (with-tmpdir (root)
    (write-tb-task root "good")
    (write-tb-task root "bad" :test-sh nil)
    (ensure-directories-exist (path-in root "not-a-task/"))
    (testing "the error is signalled without a handler"
      (ok (signals (load-tb-dataset root) 'task-format-error)))
    (testing "SKIP-TASK leaves it out"
      (let ((ds (handler-bind ((task-format-error
                                 (lambda (c) (declare (ignore c)) (invoke-restart 'skip-task))))
                  (load-tb-dataset root))))
        (ok (equal '("good") (mapcar #'task-id (ev:eval-dataset-cases ds))))))
    (testing "USE-VALUE puts another task in its place"
      (let* ((other (load-tb-task (write-tb-task (path-in root "elsewhere/") "spare")))
             (ds (handler-bind ((task-format-error
                                  (lambda (c) (declare (ignore c)) (use-value other))))
                   (load-tb-dataset root))))
        (ok (equal '("spare" "good") (mapcar #'task-id (ev:eval-dataset-cases ds)))))))
  (with-tmpdir (empty)
    (ok (eq :tasks (format-error-field (lambda () (load-tb-dataset empty)))))))

(deftest dataset-digest-and-version
  (let ((a (fixture-dataset)))
    (ok (= 64 (length (dataset-digest a))))
    (ok (equal (dataset-digest a) (dataset-digest (fixture-dataset))))
    (ok (equal (ev:eval-dataset-version a) (ev:eval-dataset-version (fixture-dataset)))))
  (with-tmpdir (root)
    (copy-tree-to (fixture "tb/") (path-in root "copy/"))
    (let ((here (fixture-dataset))
          (there (load-tb-dataset (path-in root "copy/") :name "fixtures")))
      (testing "the digest ignores where the checkout lives"
        (ok (equal (dataset-digest here) (dataset-digest there))))
      (write-file (path-in root "copy/") "hello-world/tests/test.sh" "#!/bin/bash
exit 0
")
      (ok (not (equal (dataset-digest here)
                      (dataset-digest (load-tb-dataset (path-in root "copy/")))))))))

(deftest dump-and-load-dataset-round-trip
  (let* ((ds (fixture-dataset))
         (back (ev:load-dataset (ev:dump-dataset ds))))
    (ok (equal (mapcar #'task-spec (ev:eval-dataset-cases ds))
               (mapcar #'task-spec (ev:eval-dataset-cases back))))
    (ok (every #'tb-task-p (ev:eval-dataset-cases back)))))

(deftest task-spec-of-a-non-task
  (ok (eq :task (format-error-field
                 (lambda () (task-spec (ev:make-eval-case :input "hi")))))))
