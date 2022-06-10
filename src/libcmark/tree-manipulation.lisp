;;;; Manipulation of the node tree
(in-package #:libcmark)

(defcfun ("cmark_node_unlink" node-unlink) :void
  "Unlinks a NODE, removing it from the tree, but not freeing its memory. (Use
  FREE-NODE for that.)"
  (node :pointer))

(defcfun ("cmark_node_insert_before" node-insert-before) :int
  "Inserts sibling before NODE. Returns 1 on success, 0 on failure."
  (node :pointer)
  (sibling :pointer))

(defcfun ("cmark_node_insert_after" node-insert-after) :int
  "Inserts sibling after NODE. Returns 1 on success, 0 on failure."
  (node :pointer)
  (sibling :pointer))

(defcfun ("cmark_node_replace" node-replace) :int
  "Replaces OLD-NODE with NEW-NODE and unlinks oldnode (but does not free its
  memory). Returns 1 on success, 0 on failure."
  (old-node :pointer)
  (new-node :pointer))

(defcfun ("cmark_node_prepend_child" node-prepend-child) :int
  "Adds CHILD to the beginning of the children of NODE.  Returns 1 on success,
  0 on failure."
  (node :pointer)
  (child :pointer))

(defcfun ("cmark_node_append_child" node-append-child) :int
  "Adds CHILD to the end of the children of NODE.  Returns 1 on success, 0 on
  failure."
  (node :pointer)
  (child :pointer))

(defcfun ("cmark_consolidate_text_nodes" consolidate-text-nodes) :void
  "Consolidates adjacent text nodes."
  (root :pointer))
