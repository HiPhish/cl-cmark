(in-package #:cmark/test)

(def-suite cmark/iteration
  :description "Tests for iterating over a node tree."
  :in cmark)

(def-suite cmark/iteration/iterator
  :description "Tests for the iterator object"
  :in cmark/iteration)

(defmacro is-iterator-state (iterator node event)
  "Check whether the current state of the ITERATOR matches the expected NODE
  and EVENT."
  (let ((iterator-var (gensym)))
    `(let ((,iterator-var ,iterator))
       (is (eq ,node (cmark::iterator-node ,iterator-var)))
       (is (eq ,event (cmark::iterator-event ,iterator-var))))))

(defmacro is-iterator-states (iterator &body states)
  "Consecutive iterator advancement of ITERATOR yields the following STATES,
  where each state is a pair (NODE EVENT). Does not include the initial state
  of the iterator."
  (let ((iterator-var (gensym))
        (node/event   (gensym)))
    `(let ((,iterator-var ,iterator))
       (dolist (,node/event (list ,@(mapcar (lambda (state) `(cons ,@state))
                                            states)))
         (cmark:iterator-advance ,iterator-var)
         (is-iterator-state ,iterator-var (car ,node/event) (cdr ,node/event))))))


;;; ---------------------------------------------------------------------------
(in-suite cmark/iteration/iterator)

(test initial-state
  "Initially the iterator is at the root without event"
  (let* ((root (make-instance 'cmark:document-node))
         (iterator (cmark:make-iterator root)))
    (is-iterator-state iterator root nil)))

(test idempotency
  "Advancing an exhausted iterator is idempotent"
  (let* ((root (make-instance 'cmark:document-node))
         (iterator (cmark:make-iterator root)))
    (cmark:iterator-reset iterator root 'cmark:done)
    (cmark:iterator-advance iterator)
    (is-iterator-state iterator root 'cmark:done)))

(test singular-node
  "Iterate over a tree consisting of the root only"
  (let* ((root (make-instance 'cmark:document-node))
         (iterator (cmark:make-iterator root)))
    (is-iterator-states iterator
      (root 'cmark:enter)
      (root 'cmark:exit)
      (root 'cmark:done))))

(test complex-tree
  "Iterate over a complex tree of many nodes

                A    
                |    
        +-------+-------+
        |       |       |
        B       G       L
        |       |
      +-+-+  +--+--+
      |   |  |  |  |
      C   D  H  I  J
          |        |
        +-+-+      |
        |   |      |
        E   F      K"
  (let* ((a (make-instance 'cmark:document-node))
         (b (make-instance 'cmark:document-node))
         (c (make-instance 'cmark:document-node))
         (d (make-instance 'cmark:document-node))
         (e (make-instance 'cmark:document-node))
         (f (make-instance 'cmark:document-node))
         (g (make-instance 'cmark:document-node))
         (h (make-instance 'cmark:document-node))
         (i (make-instance 'cmark:document-node))
         (j (make-instance 'cmark:document-node))
         (k (make-instance 'cmark:document-node))
         (l (make-instance 'cmark:document-node))
         (iterator (cmark:make-iterator a)))
    (cmark:append-child-node a b)
    (cmark:append-child-node a g)
    (cmark:append-child-node a l)
    (cmark:append-child-node b c)
    (cmark:append-child-node b d)
    (cmark:append-child-node d e)
    (cmark:append-child-node d f)
    (cmark:append-child-node g h)
    (cmark:append-child-node g i)
    (cmark:append-child-node g j)
    (cmark:append-child-node j k)
    (is-iterator-states iterator
      (a 'cmark:enter)
      (b 'cmark:enter)
      (c 'cmark:enter)
      (d 'cmark:enter)
      (e 'cmark:enter)
      (f 'cmark:enter)
      (d 'cmark:exit)
      (b 'cmark:exit)
      (g 'cmark:enter)
      (h 'cmark:enter)
      (i 'cmark:enter)
      (j 'cmark:enter)
      (k 'cmark:enter)
      (j 'cmark:exit)
      (g 'cmark:exit)
      (l 'cmark:enter)
      (a 'cmark:exit)
      (a 'cmark:done))))
