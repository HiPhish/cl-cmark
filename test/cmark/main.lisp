(in-package #:cmark/test)

(defun test-all ()
  (run! 'all-tests))

(def-suite all-tests
  :description "The root suite of all tests.")

(def-suite cmark
  :description "Parent suite of tests for the high-level bindings"
  :in cmark/test:all-tests)
