(in-package #:libcmark/test)

(def-suite libcmark/iterator
  :description "Testing iteration over a document tree"
  :in libcmark)
(in-suite libcmark/iterator)


(defmacro with-document-iterator (iterator-name root-form &body body)
  "Test fixture which instantiates a document iterator for the node ROOT-FORM
  and cleans it up when the form exits. Within the BODY forms the iterator is
  available as ITERATOR-NAME."
  (let ((root-name (gensym)))
    `(let* ((,root-name ,root-form)
            (,iterator-name (make-iterator ,root-name)))
       (unwind-protect (progn ,@body)
         (free-iterator ,iterator-name)))))

(test iterator-creation-and-destruction
  "We can create and destroy an iterator instance."
  (with-document (root)
    (let ((iterator (make-iterator root)))
      (free-iterator iterator))))

(test iterator-next
  "We can advance the iterator"
  (with-document (root)
    (with-document-iterator iterator root
      (iterator-next iterator))))

(test iterator-get-node
  "We can get the current node of the iterator"
  (with-document (root)
    (with-document-iterator iterator root
      (iterator-get-node iterator))))

(test iterator-get-event-type
  "We can get the current event type of the iterator"
  (with-document (root)
    (with-document-iterator iterator root
      (iterator-get-event-type iterator))))

(test iterator-get-root
  "We can get the root node of the iterator"
  (with-document (root)
    (with-document-iterator iterator root
      (iterator-get-root iterator))))

(test iterator-reset
  "We can get reset the iterator"
  (with-document (root)
    (with-document-iterator iterator root
      (iterator-reset iterator root :cmark-event-enter))))
