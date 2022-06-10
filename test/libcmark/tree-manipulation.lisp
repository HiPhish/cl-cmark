(in-package #:libcmark/test)

(def-suite libcmark/tree-manipulation
  :description "Testing tree manipulation"
  :in libcmark)
(in-suite libcmark/tree-manipulation)


(test node-unlink
  "We can unlink a node from the tree"
  (finishes
    (with-document (root)
      ;; After the child has been unlined we need to free it manually, it is no
      ;; longer managed by its parent
      (let ((child (node-first-child root)))
        (unwind-protect
            (node-unlink child)
          (free-node child))))))

(test node-insert-before
  "We can insert a node into the tree in front of a node"
  (finishes
    (with-document (root)
      (let ((sibling (make-node :CMARK-NODE-EMPH))
            (node    (node-first-child root)))
        (when (zerop (node-insert-before node sibling))
          (free-node sibling))))))

(test node-insert-after
  "We can insert a node into the tree after a node"
  (finishes
    (with-document (root)
      (let ((sibling (make-node :CMARK-NODE-EMPH))
            (node    (node-first-child root)))
        (when (zerop (node-insert-after node sibling))
          (free-node sibling))))))

(test node-replace
  "We can replace a node in the tree"
  (finishes
    (with-document (root)
      (let* ((old-node (node-first-child root))
             (new-node (make-node :cmark-node-emph))
             (orphan new-node))
        (unwind-protect
            (unless (zerop (node-replace old-node new-node))
              (setf orphan old-node))
          (free-node orphan))))))

(test node-prepend-child
  "We can insert a new child into the tree"
  (finishes
    (with-document (root)
      (let* ((child (make-node :cmark-node-emph)))
        (when (zerop (node-prepend-child root child))
          (free-node child))))))

(test node-append-child
  "We can append a new child into the tree"
  (finishes
    (with-document (root)
      (let* ((child (make-node :cmark-node-emph)))
        (when (zerop (node-append-child root child))
          (free-node child))))))

(test node-append-child
  "We can consolidate the children of the tree"
  (finishes
    (with-document (root)
      (consolidate-text-nodes root))))
