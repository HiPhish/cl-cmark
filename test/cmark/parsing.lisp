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


;;; ---------------------------------------------------------------------------
(def-suite cmark/parsing/streaming
  :description "Testing the streaming parser"
  :in cmark/parsing)
(in-suite cmark/parsing/streaming)

(test create-and-close-parser
  "We can create a new parser and close it"
  (finishes (cmark::close-streaming-parser (cmark::make-streaming-parser))))

(test with-parser-context
  "We can call the parser context macro"
  (finishes
    (cmark::with-streaming-parser (parser)
      nil)))

(test parser-context-has-parser-binding
  "We can call the parser context macro"
  (cmark::with-streaming-parser (parser)
    (is (typep parser 'cmark::streaming-parser))))

(test feed-parser
  "We can feed a string to the streaming parser"
  (cmark::with-streaming-parser (parser)
    (finishes (cmark::feed-streaming-parser parser "Hello *world*."))))

(test finish-parser
  "We can finish the streaming parser"
  (cmark::with-streaming-parser (parser)
    (cmark::feed-streaming-parser parser "Hello *world*.")
    (is (typep (cmark::finish-streaming-parser parser) 'cmark::node))))

(test open-parser-not-exhausted
  "After closing a parser it is exhausted"
  (cmark::with-streaming-parser (parser)
    (is-false (cmark::streaming-parser-exhausted-p parser))))

(test closed-parser-exhausted
  "After closing a parser it is exhausted"
  (let ((parser (cmark::make-streaming-parser)))
    (cmark::close-streaming-parser parser)
    (is-true (cmark::streaming-parser-exhausted-p parser))))

(test closing-idempotent
  "Closing a closed parser does not raise any conditions"
  (let ((parser (cmark::make-streaming-parser)))
    (finishes (cmark::close-streaming-parser parser))
    (finishes (cmark::close-streaming-parser parser))))

(test feed-closed
  "Feeding a closed parser raises a condition"
  (let ((parser (cmark::make-streaming-parser)))
    (cmark::close-streaming-parser parser)
    (signals error (cmark::feed-streaming-parser parser "Hello *world*"))))

(test finish-closed
  "Finishing a closed parser raises a condition"
  (let ((parser (cmark::make-streaming-parser)))
    (cmark::close-streaming-parser parser)
    (signals error (cmark::finish-streaming-parser parser))))
