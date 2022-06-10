(in-package #:cmark/test)

(def-suite cmark/parsing
  :description "Tests for parsing text into nodes."
  :in cmark)

(defun load-md-from-file (fname)
  "Load the file contents of a Markdown file into a string."
  (declare (type string fname))
  (with-open-file (in (merge-pathnames
                          fname
                          (asdf:system-source-directory "cmark/test"))
                      :direction :input)
    (let ((result (make-string (file-length in))))
      (read-sequence result in)
      result)))


;;; ---------------------------------------------------------------------------
(def-suite cmark/parsing/document
  :description "Parsing an entire document in one go"
  :in cmark/parsing)
(in-suite cmark/parsing/document)

(test parse-empty-document
  (let ((document (cmark::parse-document "")))
    (is-true (typep document 'cmark::node))))

(test parse-nonempty-document
  "Parsing a simple markup-less document"
  (let* ((document (cmark::parse-document "Hello world"))
         (paragraph (car (cmark::node-children  document)))
         (text      (car (cmark::node-children paragraph))))
    (is (= 1 (length (cmark::node-children  document))))
    (is (= 1 (length (cmark::node-children paragraph))))
    (is (zerop (length (cmark::node-children text))))
    (is (string= "Hello world" (cmark::node-literal text)))))

(test parse-complex-document
  "Parsing an entire document yields a large tree structure"
  (let* ((markdown (load-md-from-file "test/sample.md")))
    (finishes (cmark::parse-document markdown))))
