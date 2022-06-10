(in-package #:libcmark/test)

(defmacro with-parser (parser-name options &body body)
  "Test fixture which instantiates a new parser with OPTIONS and cleans it up
  when the form exits. Within the BODY forms the parser is available as
  PARSER-NAME."
  `(let ((,parser-name (make-parser ,options)))
     (unwind-protect (progn ,@body)
       (free-parser ,parser-name))))

(def-suite libcmark/parsing
  :description "Testing document parsing"
  :in libcmark)
(in-suite libcmark/parsing)


(test parser-creation-and-destruction
  "We can create and destroy a parser object"
  (finishes
    (with-parser parser +CMARK-OPT-DEFAULT+
      nil)))

(test parser-feed-and-finish
  "We can feed a string into a parser and finish parsing"
  (with-parser parser +CMARK-OPT-DEFAULT+
    (let ((document "Hello *world*"))
      (parser-feed parser document (length document)))
    (let ((node (parser-finish parser)))
      (free-node node))))

(test parse-string
  "A string can be parsed into a node with children"
  (finishes
    (with-document (node)
      nil)))

; parse-file
