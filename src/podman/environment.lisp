(defpackage #:agent-eval/podman
  (:use #:cl #:agent-eval)
  (:local-nicknames (#:compute #:compute-protocol))
  (:export #:container-environment #:make-container-environment
           #:container-environment-image #:container-environment-backend
           #:+minimum-podman-backend-version+ #:podman-backend-ready-p))

(in-package #:agent-eval/podman)

;;; The container environment (E0, system agent-eval/podman). Terminal-Bench
;;; semantics: ONE container per trial, alive from START-ENVIRONMENT to
;;; STOP-ENVIRONMENT, so what the agent leaves is what the verifier sees.
;;; Needs compute-backend-podman >= 0.3.0 (egao1980/compute-backend-podman#3:
;;; warm sessions on podman exec, :rw mounts, copy in / out, image build;
;;; egao1980/compute-protocol#10: sandbox-spec :directory / :input). Spec:
;;; tests/container-test.lisp (gated: skipped without podman, without the
;;; test image, or without that backend version).
;;;
;;; MAKE-CONTAINER-ENVIRONMENT task &key directory backend image
;;;   DIRECTORY the trial's host directory (as MAKE-LOCAL-ENVIRONMENT).
;;;   BACKEND a compute-protocol backend (default
;;;   (COMPUTE-BACKEND-PODMAN:MAKE-PODMAN-COMPUTE-BACKEND)). IMAGE overrides
;;;   the task's; else the task's :IMAGE; else, with a Dockerfile, the image
;;;   built from the task's environment/ (tag
;;;   "agent-eval/<task-id>:<first 12 of files-sha256>", built once). No image
;;;   and no build → UNSUPPORTED-TASK.
;;; START-ENVIRONMENT: COMPUTE:OPEN-SANDBOX with runtime IMAGE, :cpu-limit
;;;   the task's cpus, :memory-limit memory_mb × 2^20 bytes, network :ALLOW
;;;   when the task allows internet else :NONE, and DIRECTORY/logs/ mounted
;;;   :rw at /logs (so agent and verifier logs land on the host as they are
;;;   written); then makes the work dir, /logs/agent and /logs/verifier.
;;; ENVIRONMENT-EXEC: COMPUTE:RUN-SANDBOXED on the session (podman exec), the
;;;   rules of src/environments.lisp; ENVIRONMENT-COMMAND-PATH is the path
;;;   itself. UPLOAD-PATH / DOWNLOAD-PATH: the backend's copy. STOP: closes the
;;;   session (the container is removed); idempotent.
;;; PODMAN-BACKEND-READY-P → T when the loaded compute-backend-podman's
;;;   version is >= +MINIMUM-PODMAN-BACKEND-VERSION+ and `podman info`
;;;   succeeds; never signals.

(defparameter +minimum-podman-backend-version+ "0.3.0")

(defclass container-environment (task-environment) ()
  (:documentation "A task environment in one podman container (stub)."))

(defun make-container-environment (task &key directory backend image)
  (declare (ignore task directory backend image))
  (error "make-container-environment is not implemented yet (E0 contract)"))

(defgeneric container-environment-image (environment))
(defgeneric container-environment-backend (environment))

(defun podman-backend-ready-p ()
  nil)
