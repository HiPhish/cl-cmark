;;;; Traversal of nodes across the node tree

(in-package #:cmark)
;;; Note: This file does not include tree traversal functions which are
;;; part of the NODE protocol anyway

(defun node-first-child (node)
  "Returns the first child of NODE, or NIL if NODE has no children."
  (declare (type node node))
  (first (slot-value node 'children)))

(defun node-last-child (node)
  "Returns the last child of NODE, or NIL if NODE has no children."
  (declare (type node node))
  (car (last (slot-value node 'children))))

(defun node-next (node &optional (child 0))
  "Returns the next NODE in the tree, or NIL if there is none. The nodes
  are traversed in a depth-first manner. The CHILD parameter determines into
  which child node to descend."
  (declare (type node node)
           (type (integer 0) child))
  (let ((parent (node-parent node))
        (children (node-children node))
        (position (which-child node)))
    (cond
      ((and children (< child (length children)))
       (elt children child))
      (parent
       ;; NOTE: this uses recursion, it might blow the stack on Lisps without
       ;; tail call optimisation
       (node-next parent (1+ position)))
      (t nil))))

(defun node-previous (node)
  "Returns the previous NODE in the sequence after NODE, or NIL if there is
  none."
  (declare (type node node))
  (let ((parent (node-parent node))
        (position (which-child node)))
    (cond
      ((orphanp node) nil)
      ((zerop position) parent)
      (t
       ;; Start at the previous sibling of NODE, then keep descending through
       ;; the last child each until hitting a leaf node
       (do ((current (elt (node-children parent) (1- position)) (car (last (node-children current)))))
           ((childlessp current) current))))))

(defun node-next-sibling (node)
  "Returns the next sibling NODE, or NIL if NODE is the last child."
  (declare (type node node))
  (let ((position (which-child node)))
    (when position
      (let ((siblings (node-children (node-parent node))))
        (when (< position (1- (length siblings)))
          (elt siblings (1+ position)))))))

(defun node-previous-sibling (node)
  "Returns the next sibling NODE, or NIL if NODE is the last child."
  (declare (type node node))
  (let ((position (which-child node)))
    (when (and position (> position 0))
      (elt (node-children (node-parent node)) (1+ position)))))

(defun which-child (node)
  "Position of NODE among its siblings, or NIL if NODE is an orphan."
  (declare (type node node))
  (let ((parent (node-parent node)))
    (and parent
         (position node (node-children parent)))))
