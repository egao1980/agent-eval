(defpackage #:agent-eval/tests
  (:use #:cl #:rove #:agent-eval)
  (:local-nicknames (#:ev #:eval-protocol)
                    (#:compute #:compute-protocol)
                    (#:json #:json-protocol)))

(in-package #:agent-eval/tests)
