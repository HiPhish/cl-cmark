;;;; Creating and Destroying Nodes
(in-package #:libcmark)

(defcfun ("cmark_node_new" make-node) :pointer
  "Creates a new node of type TYPE. Note that the node may have other required
  properties, which it is the caller's responsibility to assign."
  (type cmark-node-type))

(defcfun ("cmark_node_new_with_mem" make-node-with-mem) :pointer
  "Same as MAKE-NODE, but explicitly listing the memory allocator used to
  allocate the node. Note: be sure to use the same allocator for every node in
  a tree, or bad things can happen."
  (type cmark-node-type)
  (mem (:pointer (:struct cmark-mem))))

(defcfun ("cmark_node_free" free-node) :void
  "Frees the memory allocated for a node and any children."
  (node :pointer))
