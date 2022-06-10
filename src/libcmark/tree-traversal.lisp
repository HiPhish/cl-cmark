;;; Traversal of the nodes inside the tree
(in-package #:libcmark)

(defcfun ("cmark_node_next" node-next) :pointer
  "Returns the next node in the sequence after node, or NULL if there is none."
  (node :pointer))

(defcfun ("cmark_node_previous" node-previous) :pointer
  "Returns the previous node in the sequence after node, or NULL if there is none."
  (node :pointer))

(defcfun ("cmark_node_parent" node-parent) :pointer
  "Returns the parent of node, or NULL if there is none."
  (node :pointer))

(defcfun ("cmark_node_first_child" node-first-child) :pointer
  "Returns the first child of node, or NULL if node has no children."
  (node :pointer))

(defcfun ("cmark_node_last_child" node-last-child) :pointer
  "Returns the last child of node, or NULL if node has no children."
  (node :pointer))
