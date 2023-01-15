(in-package #:cmark/test)

(def-suite cmark/contructor
  :description "Tests for node instance construction"
  :in cmark)

(in-suite cmark/contructor)

(test orphan-node
  "Instantiate a child node instance without parent node"
  (finishes
    (let ((parent nil))
      (make-instance 'cmark:child-node :parent parent))))

(test child-node
  "Instantiate a child node instance without parent node"
  (finishes
    (let ((parent (make-instance 'cmark:child-node :parent nil)))
      (make-instance 'cmark:child-node :parent parent))))
