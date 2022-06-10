(defpackage #:libcmark/test
  (:documentation "Tests for the low-level direct bindings")
  (:use #:cl #:cffi #:libcmark #:fiveam)
  (:export run! all-tests))
(in-package #:libcmark/test)

(defun test-all ()
  (run! 'all-tests))

