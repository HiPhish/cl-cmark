(in-package #:libcmark/test)

(def-suite libcmark/traversal
  :description "Testing node creation"
  :in libcmark)
(in-suite libcmark/traversal)

(test node-next
  "We can get the next sibling of a node"
  (finishes
    (with-document (node)
      (node-next node))))

(test node-previous
  "We can get the previous sibling of a node"
  (finishes
    (with-document (node)
      (node-previous node))))

(test node-parent
  "We can get the parent of a node"
  (finishes
    (with-document (node)
      (node-parent node))))

(test node-first-child
  "We can get the first child of a node"
  (finishes
    (with-document (node)
      (node-first-child node))))

(test node-last-child
  "We can get the last child of a node"
  (finishes
    (with-document (node)
      (node-last-child node))))
