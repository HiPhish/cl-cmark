(in-package #:libcmark/test)

(defmacro with-document ((node-name
                           &key (document "Hello *world*")
                                (options +CMARK-OPT-DEFAULT+))
                         &body body)
  "Test fixture in which a given DOCUMENT string is parsed to generate a
  libcmark NODE with OPTIONS in its constructor. The NODE persists throught the
  BODY forms and can be referenced there.

  After the form terminates the NODE is freed automatically."
  (let ((doc-name (gensym)))
   `(let* ((,doc-name ,document)
           (,node-name (parse-document ,doc-name (length ,doc-name) ,options)))
      (unwind-protect (progn ,@body)
        (free-node ,node-name)))))


(def-suite all-tests
  :description "The root suite of all tests.")

(def-suite libcmark
  :description "Tests for the low-level bindings. We only test that the
  bindings work, not how well the bound functions work, i.e. not their return
  values or side effects. Otherwise we would just end up testing the C library
  all over again."
  :in libcmark/test:all-tests)
(in-suite libcmark)

(test simple-interface-works
  "The simple interface is a function which convert an in-memory CommonMark
  string to HTML; we only test that it works, not its return value."
  (let* ((input "Hello *world*!")
         (output (markdown-to-html input (length input) +CMARK-OPT-DEFAULT+)))
    (is (> (length output) 0))))
