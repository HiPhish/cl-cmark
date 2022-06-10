;;;; Helper functions which wrap around a foreign node and use it to parse the
;;;; document and return a tree of native nodes instead

(in-package #:cmark)

(defparameter *foreign/native-nodes* nil
  "Mapping of foreign node memory pointer to a native node. Bind this variable
  to a hash table while translating foreign nodes to native ones; as a new
  native node is constructed add an entry, later nodes can then refer to it,
  e.g. to get a reference to their native parent based on their foreign
  parent.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *node-type-to-class*
    '((:cmark-node-document       . (document-node))
      (:cmark-node-block-quote    . (block-quote-node))
      (:cmark-node-list           . (list-node
                                      (libcmark:node-get-list-type   . :type)
                                      (libcmark:node-get-list-delim  . :delim)
                                      (libcmark:node-get-list-start  . :start)
                                      (libcmark:node-get-list-tight  . :tightp)))
      (:cmark-node-item           . (item-node))
      (:cmark-node-code-block     . (code-block-node
                                      (libcmark:node-get-fence-info . :fence-info)
                                      (libcmark:node-get-literal . :literal)))
      (:cmark-node-html-block     . (html-block-node
                                      (libcmark:node-get-literal . :literal)))
      (:cmark-node-custom-block   . (custom-block-node
                                      (libcmark:node-get-on-enter . :on-enter)
                                      (libcmark:node-get-on-exit . :on-exit)))
      (:cmark-node-paragraph      . (paragraph-node))
      (:cmark-node-heading        . (heading-node
                                      (libcmark:node-get-heading-level . :level)))
      (:cmark-node-thematic-break . (thematic-break-node))
      (:cmark-node-text           . (text-node
                                      (libcmark:node-get-literal . :literal)))
      (:cmark-node-softbreak      . (softbreak-node))
      (:cmark-node-linebreak      . (linebreak-node))
      (:cmark-node-code           . (code-node
                                      (libcmark:node-get-literal . :literal)))
      (:cmark-node-html-inline    . (html-inline-node
                                      (libcmark:node-get-literal . :literal)))
      (:cmark-node-custom-inline  . (custom-inline-node
                                      (libcmark:node-get-on-enter . :on-enter)
                                      (libcmark:node-get-on-exit . :on-exit)))
      (:cmark-node-emph           . (emph-node))
      (:cmark-node-strong         . (strong-node))
      (:cmark-node-link           . (link-node
                                      (libcmark:node-get-url . :url)
                                      (libcmark:node-get-title . :title)))
      (:cmark-node-image          . (image-node
                                      (libcmark:node-get-url . :url)
                                      (libcmark:node-get-title . :title) )))
    "Maps the node type symbol from libcmark to a native node class name its
    slots. Each slot is (FOREIGN-GETTER . NATIVE-INITARG) pair.")

  (defparameter *node-common-fields*
    '((libcmark:node-get-user-data . :user-data)
      (libcmark:node-get-start-line . :start-line)
      (libcmark:node-get-start-column . :start-column)
      (libcmark:node-get-end-line . :end-line)
      (libcmark:node-get-end-column . :end-column))
    "List of (FOREIGN-GETTER . NATIVE-INITARG) pairs for all common node fields.")

  (defun zip (list1 list2)
    "Helper function, zips two lists into one flat list."
    (loop for i in list1 for j in list2
          collect i collect j))

  (defun thing-to-gensym (thing)
    "Helper function, ignores its argument and generates an uninterned symbol."
    (declare (ignore thing))
    (gensym)))


(defmacro define-node-parser-method (method-name type)
  "Defines a method specialised for parsing a foreign node into a native node
  based on its TYPE. All the slots, except PARENT and CHILDREN, are
  initialised based by getting the value from the foreign node."
  (destructuring-bind (class . slots) (cdr (assoc type *NODE-TYPE-TO-CLASS*))
    (let* ((specific-initargs (mapcar #'cdr slots))
           (specific-vars (mapcar #'thing-to-gensym slots))
           (common-initargs (mapcar #'cdr *node-common-fields*))
           (common-vars (mapcar #'thing-to-gensym *node-common-fields*))
           (foreign (gensym)))
      `(defmethod ,method-name (,foreign (type (eql ,type)))
         (let ,(concatenate 'list
                            (mapcar (lambda (var spec) `(,var (,(car spec) ,foreign)))
                                    specific-vars slots)
                            (mapcar (lambda (var spec) `(,var (,(car spec) ,foreign)))
                                    common-vars *node-common-fields*))
            (make-instance ',class
                           ,@(zip specific-initargs specific-vars)
                           ,@(zip common-initargs common-vars)))))))

;;; I would have liked to do
;;;   (dolist (type '(...))
;;;     (define-node-parser-method type))
;;; but then TYPE would have been passed as a symbol to the macro whereas I
;;; want the value of the variable.
(define-node-parser-method parse-node :cmark-node-document)
(define-node-parser-method parse-node :cmark-node-block-quote)
(define-node-parser-method parse-node :cmark-node-list)
(define-node-parser-method parse-node :cmark-node-item)
(define-node-parser-method parse-node :cmark-node-code-block)
(define-node-parser-method parse-node :cmark-node-html-block)
(define-node-parser-method parse-node :cmark-node-custom-block)
(define-node-parser-method parse-node :cmark-node-paragraph)
(define-node-parser-method parse-node :cmark-node-heading)
(define-node-parser-method parse-node :cmark-node-thematic-break)
(define-node-parser-method parse-node :cmark-node-text)
(define-node-parser-method parse-node :cmark-node-softbreak)
(define-node-parser-method parse-node :cmark-node-linebreak)
(define-node-parser-method parse-node :cmark-node-code)
(define-node-parser-method parse-node :cmark-node-html-inline)
(define-node-parser-method parse-node :cmark-node-custom-inline)
(define-node-parser-method parse-node :cmark-node-emph)
(define-node-parser-method parse-node :cmark-node-strong)
(define-node-parser-method parse-node :cmark-node-link)
(define-node-parser-method parse-node :cmark-node-image)

(defun node-foreign-to-native (foreign)
  "Translates one FOREIGN node to a native one. Establishes parent/child
  relationship. Registers FOREIGN and the new node in the
  *FOREIGN/NATIVE-NODES* table."
  (declare (type cffi:foreign-pointer foreign))
  (let* ((type (libcmark:node-get-type foreign))
         (node (parse-node foreign type))
         (foreign-parent (libcmark:node-parent foreign))
         (native-parent (gethash (cffi:pointer-address foreign-parent)
                                 *FOREIGN/NATIVE-NODES*)))
    (when native-parent
      (with-slots (parent) node
        (setf parent native-parent))
      (with-slots (children) native-parent
        (setf children (nconc children (list node)))))
    (setf (gethash (cffi:pointer-address foreign) *foreign/native-nodes*) node)
    node))

(defun parse-tree (foreign)
  "Parses the entire FOREIGN tree into a native tree."
  (declare (type cffi:foreign-pointer foreign))
  (let ((*FOREIGN/NATIVE-NODES* (make-hash-table)))
    (let ((root (node-foreign-to-native foreign)))
      (let ((iterator (libcmark:make-iterator foreign)))
        (unwind-protect
            (progn
              ;; This enters the root, we are not interested
              (libcmark:iterator-next iterator)
              (libcmark:iterator-next iterator)
              (do ((node (libcmark:iterator-get-node iterator)
                         (libcmark:iterator-get-node iterator))
                   (event (libcmark:iterator-get-event-type iterator)
                          (libcmark:iterator-get-event-type iterator)))
                  ((eq event :cmark-event-done) root)
                ;; Called for side effects only
                (when (eq event :cmark-event-enter)
                  (node-foreign-to-native node))
                (libcmark:iterator-next iterator)))
          (libcmark:free-iterator iterator))))))
