(in-package #:cmark/test)

(def-suite cmark/tree-manipulation
  :description "Tests of node tree manipulation routines."
  :in cmark)


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/prepend-child-node
  :description "Prepending a new child node to a parent"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/prepend-child-node)

(test prepend-child-to-childless-node
  "Prepending a child node establishes a parent-child relationshipt"
  (let* ((child (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:prepend-child-node parent child)
    (is (eq (cmark:node-parent child) parent))
    (is (equal (list child)
               (cmark:node-children parent)))))

(test prepend-child-order
  "Prepending a child respects ordering"
  (let* ((child1 (make-instance 'cmark:block-quote-node))
         (child2 (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:prepend-child-node parent child2)
    (cmark:prepend-child-node parent child1)
    (is (equal (list child1 child2)
               (cmark:node-children parent)))))

(test prepend-non-orphan
  "Prepending a non-orphan node signals a condition"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:block-quote-node))
        (root   (make-instance 'cmark:document-node)))
    (cmark:prepend-child-node parent child)
    (signals cmark:child-node (cmark:prepend-child-node root child))))

(test prepend-non-orphan/detach-from-parent
  "A restart lets us detach the node from its parent"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:block-quote-node))
        (root   (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (handler-bind
        ((cmark:child-node (lambda (c)
                             (declare (ignore c))
                             (invoke-restart 'cmark:detach-from-parent))))
      (cmark:prepend-child-node root child))
    (is (equal (list child)
               (cmark:node-children root)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/append-child-node
  :description "Appending a new child node to a parent"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/append-child-node)

(test append-child-to-childless-node
  "Appending a child node establishes a parent-child relationshipt"
  (let* ((child (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (is (eq (cmark:node-parent child) parent))
    (is (equal (list child)
               (cmark:node-children parent)))))

(test append-child-order
  "Appending a child respects ordering"
  (let* ((child1 (make-instance 'cmark:block-quote-node))
         (child2 (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child1)
    (cmark:append-child-node parent child2)
    (is (equal (list child1 child2)
               (cmark:node-children parent)))))

(test append-non-orphan
  "Appending a non-orphan node signals a condition"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:block-quote-node))
        (root   (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (signals cmark:child-node (cmark:append-child-node root child))))

(test append-non-orphan/detach-from-parent
  "A restart lets us detach the node from its parent"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:block-quote-node))
        (root   (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (handler-bind
        ((cmark:child-node (lambda (c)
                             (declare (ignore c))
                             (invoke-restart 'cmark:detach-from-parent))))
      (cmark:append-child-node root child))
    (is (equal (list child)
               (cmark:node-children root)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/insert-before-node
  :description "Inserting a new node in front of an existing node"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/insert-before-node)

(test insert-before-existing
  "Inserting before an existing node in a tree"
  (let* ((child1 (make-instance 'cmark:block-quote-node))
         (child2 (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child2)
    (cmark:insert-node-before child2 child1)
    (is (equal (list child1 child2)
               (cmark:node-children parent)))))

(test insert-before-orphan
  "Inserting a node before an orphan node signals an error"
  (let* ((child  (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (signals cmark:orphan-node
      (cmark:insert-node-before parent child))))

(test insert-before-non-orphan-node
  "Inserting a non-orphan node signals an error"
  (let* ((child1  (make-instance 'cmark:block-quote-node))
         (child2  (make-instance 'cmark:block-quote-node))
         (parent1 (make-instance 'cmark:document-node))
         (parent2 (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent1 child1)
    (cmark:append-child-node parent2 child2)
    (signals cmark:child-node
      (cmark:insert-node-before child1 child2))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/insert-after-node
  :description "Inserting a new node behind an existing node"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/insert-after-node)

(test insert-after-existing
  "Inserting after an existing node in a tree"
  (let* ((child1 (make-instance 'cmark:block-quote-node))
         (child2 (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child1)
    (cmark:insert-node-after child1 child2)
    (is (equal (list child1 child2)
               (cmark:node-children parent)))))

(test insert-after-orphan
  "Inserting a node after an orphan node signals an error"
  (let* ((child  (make-instance 'cmark:block-quote-node))
         (parent (make-instance 'cmark:document-node)))
    (signals cmark:orphan-node
      (cmark:insert-node-after parent child))))

(test insert-after-non-orphan-node
  "Inserting a non-orphan node signals an error"
  (let* ((child1  (make-instance 'cmark:block-quote-node))
         (child2  (make-instance 'cmark:block-quote-node))
         (parent1 (make-instance 'cmark:document-node))
         (parent2 (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent1 child1)
    (cmark:append-child-node parent2 child2)
    (signals cmark:child-node
      (cmark:insert-node-after child1 child2))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/unlink
  :description "Unlinking a node from a tree"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/unlink)

(test unlinking-side-effects
  "Unlinking destroys the parent/child relationship"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (cmark:unlink-node child)
    (is-false (cmark:node-parent child))
    (is-false (cmark:node-children parent))))

(test unlinking-return-value
  "Unlinking returns the unlinked node"
  (let ((child  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child)
    (is (eq child (cmark:unlink-node child)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/replace
  :description "Replacement of a node within a tree"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/replace)

(test replace-side-effects
  "Replacement changes parent/child relationships"
  (let ((child1  (make-instance 'cmark:block-quote-node))
        (child2  (make-instance 'cmark:block-quote-node))
        (parent (make-instance 'cmark:document-node)))
    (cmark:append-child-node parent child1)
    (cmark:replace-node child1 child2)
    (is-false (cmark:node-parent child1))
    (is (eq parent (cmark:node-parent child2)))
    (is (equal (list child2) (cmark:node-children parent)))))

(test replace-orphan-node
  "Replacing an orphan node signals an error"
  (let ((old-node (make-instance 'cmark:block-quote-node))
        (new-node (make-instance 'cmark:block-quote-node)))
    (signals cmark:orphan-node
      (cmark:replace-node old-node new-node))))

(test replace-orphan-node/prepend-to-parent
  "It is possible to prepend an orphan node to a parent in a restart"
  (let ((root     (make-instance 'cmark:document-node))
        (child    (make-instance 'cmark:block-quote-node))
        (old-node (make-instance 'cmark:block-quote-node))
        (new-node (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child)
    (handler-bind
        ((cmark:orphan-node
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cmark:prepend-to-parent root))))
      (cmark:replace-node old-node new-node))
    (is (equal (list new-node child)
               (cmark:node-children root)))
    (is-false (cmark:node-parent old-node))))

(test replace-orphan-node/append-to-parent
  "It is possible to append an orphan node to a parent in a restart"
  (let ((root     (make-instance 'cmark:document-node))
        (child    (make-instance 'cmark:block-quote-node))
        (old-node (make-instance 'cmark:block-quote-node))
        (new-node (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child)
    (handler-bind
        ((cmark:orphan-node
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cmark:append-to-parent root))))
      (cmark:replace-node old-node new-node))
    (is (equal (list child new-node)
               (cmark:node-children root)))
    (is-false (cmark:node-parent old-node))))

(test replace-orphan-node/insert-before-sibling
  "It is possible to insert an orphan node before another node in a restart"
  (let ((root     (make-instance 'cmark:document-node))
        (child    (make-instance 'cmark:block-quote-node))
        (old-node (make-instance 'cmark:block-quote-node))
        (new-node (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child)
    (handler-bind
        ((cmark:orphan-node
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cmark:insert-before-sibling child))))
      (cmark:replace-node old-node new-node))
    (is (equal (list new-node child)
               (cmark:node-children root)))
    (is-false (cmark:node-parent old-node))))

(test replace-orphan-node/insert-after-sibling
  "It is possible to insert an orphan node after another node in a restart"
  (let ((root     (make-instance 'cmark:document-node))
        (child    (make-instance 'cmark:block-quote-node))
        (old-node (make-instance 'cmark:block-quote-node))
        (new-node (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child)
    (handler-bind
        ((cmark:orphan-node
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cmark:insert-after-sibling child))))
      (cmark:replace-node old-node new-node))
    (is (equal (list child new-node)
               (cmark:node-children root)))
    (is-false (cmark:node-parent old-node))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-manipulation/consolidate
  :description "Merging adjacent text nodes in a tree"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-manipulation/consolidate)

(test consolidate-return-value
  "Consolidation returns the root of the tree"
  (let ((root (make-instance 'cmark:document-node)))
    (is (eq root(cmark:consolidate-text-nodes root)))))


(test consolidate-empty-tree
  "Consolidation of an empty tree does nothing"
  (let ((root (make-instance 'cmark:document-node)))
    (cmark:consolidate-text-nodes root)
    (is (null (cmark:node-children root)))
    (is (null (cmark:node-parent root)))))

(test consolidate-siblings
  "Adjacent child nodes are merged"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:text-node :literal "aaa"))
        (child2 (make-instance 'cmark:text-node :literal "bbb"))
        (child3 (make-instance 'cmark:text-node :literal "ccc")))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (cmark:consolidate-text-nodes root)
    (is (= 1 (length (cmark:node-children root))))
    (is (string= "aaabbbccc" (cmark:node-literal child3)))))

(test no-consolidation
  "If there is a non-text node in between there is no consolidation"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:text-node :literal "aaa"))
        (child2 (make-instance 'cmark:code-node :literal "bbb"))
        (child3 (make-instance 'cmark:text-node :literal "ccc")))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (cmark:consolidate-text-nodes root)
    (is (= 3 (length (cmark:node-children root))))
    (is (string= "aaa" (cmark:node-literal child1)))
    (is (string= "bbb" (cmark:node-literal child2)))
    (is (string= "ccc" (cmark:node-literal child3)))))
