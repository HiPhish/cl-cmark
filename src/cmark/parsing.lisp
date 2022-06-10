;;;; User-facing CommonMark parsing interface

(in-package #:cmark)
;;; NOTE: CFFI gives us conversion between native strings and UTF-8 encoded
;;; foreign strings for free

(defun parse-document (document)
  "Parses a CommonMark DOOCUMENT string and returns the root node of the parsed
  document tree."
  (declare (type string document))
  ;; Call into the C library
  ;; TODO: options!
  (let* ((length (string-octet-length document))
         (root (libcmark:parse-document document length libcmark:+cmark-opt-default+)))
    (unwind-protect
        (parse-tree root)
      (libcmark:free-node root))))

;;; TODO: Do we really want this? It cannot be built on top of PARSE-FILE since
;;; a Common Lisp FILE-STREAM and a C FILE are not interchangeable.
(defun parse-stream (stream)
  "Parses a UTF-8 encoded CommonMark file STREAM and returns the root node of
  the parsed document tree."
  (declare (type file-stream stream)
           (ignore stream))
  ;; Call into the C library
  ;; Needs the string encoded as UTF-8
  ;; Needs the length of the UTF-8 bytes
  (error "Not implemented, do not use!"))

;;; There is no streaming parser because there are memory safety issues: a
;;; native streaming parser needs to be backed by a foreign streaming parser.
;;; What if the native parser goes out of scope and is garbage collected before
;;; it has finished? The foreign parser would never get freed, leading to a
;;; memory leak.


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
