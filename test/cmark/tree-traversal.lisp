(in-package #:cmark/test)

(def-suite cmark/tree-traversal
  :description "Tests of node tree traversal routines."
  :in cmark)


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-traversal/first-child
  :description "From one node to the next in the sequence"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-traversal/first-child)

(test first-child-of-childless
  "A node without children has no first child"
  (let ((root (make-instance 'cmark:document-node)))
    (is-false (cmark:node-first-child root))))

(test first-child-of-multiple
  "A node multiple children has a first child"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (child3 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (is (eq child1
            (cmark:node-first-child root)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-traversal/last-child
  :description "From one node to the next in the sequence"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-traversal/last-child)

(test last-child-of-childless
  "A node without children has no last child"
  (let ((root (make-instance 'cmark:document-node)))
    (is-false (cmark:node-last-child root))))

(test last-child-of-multiple
  "A node multiple children has a last child"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (child3 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (is (eq child3
            (cmark:node-last-child root)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-traversal/next
  :description "From one node to the next in the sequence"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-traversal/next)

(test next-of-singular
  "A childless orphan has no next node"
  (let ((root (make-instance 'cmark:document-node)))
    (is-false (cmark:node-next root))))

(test next-is-first-child
  "The next node of a node with children is its first child"
  (let ((root (make-instance 'cmark:document-node))
        (child (make-instance 'cmark:block-quote-node)))
    (cmark:prepend-child-node root child)
    (is (eq child
            (cmark:node-next root)))))

(test next-is-sibling/two-siblings
  "The next node of a childless node is its next sibling"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (is (eq child2
            (cmark:node-next child1)))))

(test next-is-sibling/three-siblings
  "The position among siblings is taken into account"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (child3 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (is (eq child3
            (cmark:node-next child2)))))

(test next-of-last-child
  "The next node of a last child is its uncle"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (grandchild (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node child1 grandchild)
    (is (eq child2
            (cmark:node-next grandchild)))))

(test next-of-lastchild/multi-generation
  "The next node of a last child is its generation-wise closest uncle

  (root (gen1/1 (gen2/1 (gen3/1 (gen4/1)))
                (gen2/2)))"
  (let ((root (make-instance 'cmark:document-node))
        (gen1/1 (make-instance 'cmark:block-quote-node))
        (gen2/1 (make-instance 'cmark:block-quote-node))
        (gen2/2 (make-instance 'cmark:block-quote-node))
        (gen3/1 (make-instance 'cmark:block-quote-node))
        (gen4/1 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root gen1/1)
    (cmark:append-child-node gen1/1 gen2/1)
    (cmark:append-child-node gen1/1 gen2/2)
    (cmark:append-child-node gen2/1 gen3/1)
    (cmark:append-child-node gen3/1 gen4/1)
    (is (eq gen2/2
            (cmark:node-next gen4/1)))))

(test next-of-last-child/no-descent
  "The next node is the closest uncle, not one if its descentents

  (root (gen1/1 (gen2/1))
        (gen1/2 (gen2/2 (gen3/1))))"
  (let ((root (make-instance 'cmark:document-node))
        (gen1/1 (make-instance 'cmark:block-quote-node))
        (gen1/2 (make-instance 'cmark:block-quote-node))
        (gen2/1 (make-instance 'cmark:block-quote-node))
        (gen2/2 (make-instance 'cmark:block-quote-node))
        (gen3/1 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root gen1/1)
    (cmark:append-child-node root gen1/2)
    (cmark:append-child-node gen1/1 gen2/1)
    (cmark:append-child-node gen1/2 gen2/2)
    (cmark:append-child-node gen2/2 gen3/1)
    (is (eq gen1/2
            (cmark:node-next gen2/1)))))


;;; ---------------------------------------------------------------------------
(def-suite cmark/tree-traversal/previous
  :description "From one node to the previous one in the sequence"
  :in cmark/tree-manipulation)
(in-suite cmark/tree-traversal/previous)

(test previous-of-singular
  "A childless orphan has no previous node"
  (let ((root (make-instance 'cmark:document-node)))
    (is-false (cmark:node-previous root))))

(test previous-is-orphan-parent/only-child
  "The previous node of an only child is the orphan parent"
  (let ((root (make-instance 'cmark:document-node))
        (child (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child)
    (is (eq root
            (cmark:node-previous child)))))

(test previous-is-orphan-parent/first-child
  "The previous node of an only child with orphan parent is the parent"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (is (eq root
            (cmark:node-previous child1)))))

(test previous-is-sibling/childless-sibling
  "The previous node of a child is its childless sibling"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (is (eq child1
            (cmark:node-previous child2)))))

(test previous-is-sibling/three-siblings
  "The previous node of an only child is the parent"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (child3 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node root child3)
    (is (eq child2
            (cmark:node-previous child3)))))

(test previous-of-first-child
  "The next node of a last child is its uncle"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (grandchild (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node child1 grandchild)
    (is (eq child2
            (cmark:node-next grandchild)))))

(test previous-is-nephew
  "The previous node is the last child of its sibling"
  (let ((root (make-instance 'cmark:document-node))
        (child1 (make-instance 'cmark:block-quote-node))
        (child2 (make-instance 'cmark:block-quote-node))
        (grandchild (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root child1)
    (cmark:append-child-node root child2)
    (cmark:append-child-node child1 grandchild)
    (is (eq grandchild
            (cmark:node-previous child2)))))

;;; TODO: more test scenarios, similar to he above
(test previous-of-last-child/multi-generation
  "The previous node of a last child is its generation-wise closest nephew

  (root (gen1/1 (gen2/1 (gen3/1 (gen4/1)))
                (gen2/2)))"
  (let ((root (make-instance 'cmark:document-node))
        (gen1/1 (make-instance 'cmark:block-quote-node))
        (gen2/1 (make-instance 'cmark:block-quote-node))
        (gen2/2 (make-instance 'cmark:block-quote-node))
        (gen3/1 (make-instance 'cmark:block-quote-node))
        (gen4/1 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root gen1/1)
    (cmark:append-child-node gen1/1 gen2/1)
    (cmark:append-child-node gen1/1 gen2/2)
    (cmark:append-child-node gen2/1 gen3/1)
    (cmark:append-child-node gen3/1 gen4/1)
    (is (eq gen4/1
            (cmark:node-previous gen2/2)))))

(test previous-of-last-child/no-ascent
  "The next node is the closest nephew, not one if its ancestors

  (root (gen1/1 (gen2/1))
        (gen1/2 (gen2/2 (gen3/1))))"
  (let ((root (make-instance 'cmark:document-node))
        (gen1/1 (make-instance 'cmark:block-quote-node))
        (gen1/2 (make-instance 'cmark:block-quote-node))
        (gen2/1 (make-instance 'cmark:block-quote-node))
        (gen2/2 (make-instance 'cmark:block-quote-node))
        (gen3/1 (make-instance 'cmark:block-quote-node)))
    (cmark:append-child-node root gen1/1)
    (cmark:append-child-node root gen1/2)
    (cmark:append-child-node gen1/1 gen2/1)
    (cmark:append-child-node gen1/2 gen2/2)
    (cmark:append-child-node gen2/2 gen3/1)
    (is (eq gen2/2
            (cmark:node-previous gen3/1)))))
