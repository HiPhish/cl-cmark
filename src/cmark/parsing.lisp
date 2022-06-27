;;;; User-facing CommonMark parsing interface

(in-package #:cmark)
;;; NOTE: CFFI gives us conversion between native strings and UTF-8 encoded
;;; foreign strings for free

;;; FIXME: Perhaps PARSE-STRING would be a better name?
(defun parse-document (document &key (smart nil))
  "Parses a CommonMark DOOCUMENT string and returns the root node of the parsed
  document tree."
  (declare (type string document))
  ;; Call into the C library
  (let* ((length (string-octet-length document))
         (opt (logior libcmark:+cmark-opt-default+
                      (if smart libcmark:+cmark-opt-smart+ 0)))
         (root (libcmark:parse-document document length opt)))
    (unwind-protect
        (parse-tree root)
      (libcmark:free-node root))))

(defstruct (streaming-parser (:constructor nil))
  (foreign-parser nil :type (or null cffi:foreign-pointer))
  (exhausted-p nil :type boolean))

(defun make-streaming-parser (&key (smart nil))
  "Creates a new streaming parser object. A streaming parser can be fed the
  CommonMark document piecewise, then once the entire document has been fed to
  it the parser can be finished, which produces the document node tree. It is
  the caller's responsibility to close the stream."
  (let ((result (make-instance 'streaming-parser))
        (opt (logior libcmark:+cmark-opt-default+
                     (if smart libcmark:+cmark-opt-smart+ 0))))
    (with-slots (foreign-parser) result
      (setf foreign-parser (libcmark:make-parser opt)))
    result))

(defun feed-streaming-parser (parser string)
  "Feed a STRING into the streaming PARSER."
  (declare (type streaming-parser parser)
           (type string string))
  (with-slots (foreign-parser exhausted-p) parser
    (when exhausted-p
      (error "Trying to feed an exhausted streaming parser"))
    (libcmark:parser-feed foreign-parser string (string-octet-length string))
    nil))

(defun close-streaming-parser (parser)
  "Closes the streaming PARSER, rendering it exhausted. Any internal state of
  the parser will be discarded. This function is idempotent: if the parser is
  already closed nothing will happen."
  (declare (type streaming-parser parser))
  (with-slots (foreign-parser exhausted-p) parser
    (when foreign-parser
      (libcmark:free-parser foreign-parser))
    (setf foreign-parser nil)
    (setf exhausted-p t)))

(defun finish-streaming-parser (parser)
  "Returns the parsed node tree. Does not exhaust the parser, feeding the
  parser or finishing it again is not an error. It is an error to finish a
  closed parser."
  (declare (type streaming-parser parser))
  (with-slots (foreign-parser exhausted-p) parser
    (when exhausted-p
      (error "Trying to finish an exhausted streaming parser"))
    (let ((foreign-node (libcmark:parser-finish foreign-parser)))
      (unwind-protect (parse-tree foreign-node)
        (libcmark:free-node foreign-node)))))

(defmacro with-streaming-parser ((parser &key (smart nil)) &body body)
  "Evaluate the BODY forms with PARSER bound to a new streaming parser
  instance. The parser will be safely closed and disposed of when the form
  terminates. Evaluates to the last BODY expression."
  `(let ((,parser (make-streaming-parser :smart ,smart)))
     (unwind-protect (progn ,@body)
       (close-streaming-parser ,parser))))

;;; TODO: Do we really want this? It cannot be built on top of PARSE-FILE since
;;; a Common Lisp FILE-STREAM and a C FILE are not interchangeable.
(defun parse-stream (stream &key (smart nil))
  "Parses a UTF-8 encoded CommonMark file STREAM and returns the root node of
  the parsed document tree."
  (declare (type file-stream stream)
           (ignore stream)
           (ignore smart))
  ;; Call into the C library
  ;; Needs the string encoded as UTF-8
  ;; Needs the length of the UTF-8 bytes
  (error "Not implemented, do not use!"))

(defun string-octet-length (string)
  "Helper function, returns the length of STRING in UTF-8 encoded bytes"
  (length (flex:string-to-octets string :external-format :UTF-8)))

(defun dump-tree (root)
  "Dumps a string of the node tree."
  (declare (type node root))
  (labels ((dump-node (node level)
             (dotimes (i level)
               (format t "  "))
             (format t "~a~%" (class-name (class-of node)))
             (dolist (child (node-children node))
               (dump-node child (1+ level)))))
    (dump-node root 0)))
