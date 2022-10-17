;;;; Iterating over the tree nodes. Iteration is done through a stateful
;;;; iterator object that can be moved along the tree.
(in-package #:libcmark)

(defcenum cmark-event-type
  "Kind of cmark iterator event."
  :cmark-event-none
  :cmark-event-done
  :cmark-event-enter
  :cmark-event-exit)

(defcfun ("cmark_iter_new" make-iterator) :pointer
  "Creates a new iterator starting at ROOT. The current node and event type are
  undefined until ITER-NEXT is called for the first time. The memory
  allocated for the iterator should be released using FREE-ITERATOR when it is
  no longer needed."
  (root :pointer))

(defcfun ("cmark_iter_free" free-iterator) :void
  "Frees the memory allocated for an iterator."
  (iter :pointer))

(defcfun ("cmark_iter_next" iterator-next) cmark-event-type
  "Advances to the next node and returns the event type (CMARK_EVENT_ENTER,
  CMARK_EVENT_EXIT or CMARK_EVENT_DONE)."
  (iterator :pointer))

(defcfun ("cmark_iter_get_node" iterator-get-node) :pointer
  "Returns the current node."
  (iterator :pointer))

(defcfun ("cmark_iter_get_event_type" iterator-get-event-type) cmark-event-type
  "Returns the current event type."
  (iterator :pointer))

(defcfun ("cmark_iter_get_root" iterator-get-root) :pointer
  "Returns the root node."
  (iterator :pointer))

(defcfun ("cmark_iter_reset" iterator-reset) :void
  "Resets the ITERATOR so that the current node is CURRENT-NODE and the event
  type is EVENT-TYPE. The new current node must be a descendant of the root
  node or the root node itself."
  (iterator :pointer)
  (current-node :pointer)
  (event-type cmark-event-type))
