;;;; Definition of conditions
(in-package #:cmark)

(define-condition parser-exhausted (error)
  ((parser :initarg :parser :type streaming-parser
           :documentation "The parser instance")
   (string :initform nil :initarg :string :type (or null string)
           :documentation "String to parse"))
  (:documentation "Trying to manipulate an exhausted streaming parser."))

(define-condition unexpected-orphan (error)
  ((node :initarg :node :type node
         :documentation "The node in question"))
  (:documentation "Orphan node in a context where a child node is expected."))

(define-condition unexpected-parent (error)
  ((node :initarg :node :type node
         :documentation "The node in question"))
  (:documentation "Child node in a context where a orphan node is expected."))
