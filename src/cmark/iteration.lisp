;;;; Iteration over the tree of nodes

(in-package #:cmark)

(deftype node-event ()
  "Type of event when iterating through a node tree"
  '(member done enter exit))

(defparameter *non-exit-nodes*
  (mapcar #'find-class '(html-block-node thematic-break-node
                         code-block-node text-node softbreak-node
                         linebreak-node code-node html-inline-node))
  "List of node classes for which no EXIT even will be generated.")

(defun walk-tree (node callback)
  "Iterate through the (sub)tree at NODE, calling CALLBACK at every event. The
  callback is a function which takes two arguments, the current node and the
  even type. Its return value is ignored.

  Usually the callback would be a generic function that can dispatch on the
  class of node and type of even, but this is just a suggestion, not a
  requirement."
  (declare (type node node))
  (funcall callback node 'enter)
  (dolist (child (node-children node))
    (walk-tree child callback))
  (when (not (member (class-of node) *non-exit-nodes*))
    (funcall callback node 'exit))
  (when (orphanp node)
    (funcall callback node 'done))
  nil)

(defstruct (iterator (:constructor make-iterator (root)))
  "An iterator will walk through a tree of nodes, starting from a ROOT node,
  returning one node at a time, together with information about whether the
  node is being entered or exited."
  (root root :type node)
  (node root :type node)
  (event nil :type (or null node-event)))

(defun iterator-reset (iterator current event-type)
  "Reset the ITERATOR so that the current node node is CURRENT and the event
  type is EVENT-TYPE. The new current node must be a descendant of the root
  node or the root node itself."
  (declare (type iterator iterator)
           (type node current)
           (type node-event event-type))
  (setf (iterator-node iterator) current)
  (setf (iterator-event iterator) event-type)
  (values current event-type))

(defun iterator-advance (iterator)
  "Advances the ITERATOR to the next node/event combination. If the iterator is
  exhausted nothing will happen. Returns the new node and event."
  (declare (type iterator iterator))
  (let ((node (iterator-node iterator))
        (event (iterator-event iterator)))
    (cond
      ((eq event 'done)
       (values node event))
      ((null event)
       (iterator-reset iterator (iterator-root iterator) 'enter))
      ((and (eq event 'exit)
            (eq node (iterator-root iterator)))
       (iterator-reset iterator node 'done))
      ((and (eq node (iterator-root iterator))
            (childlessp node))
       (iterator-reset iterator node 'exit))
      ((or (eq event 'exit)
           (childlessp node))
       (let* ((parent (node-parent node))
              (siblings (node-children parent))
              (position (position node siblings)))
         (if (= position (1- (length siblings)))
           (iterator-reset iterator parent 'exit)
           (iterator-reset iterator (elt siblings (1+ position)) 'enter))))
      ((eq event 'enter)
       (iterator-reset iterator (node-next node) 'enter))
      (t (error "Unimplemented case while iterating node tree: (~A ~A)"
                node event)))))
