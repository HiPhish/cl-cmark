(in-package #:libcmark/test)

(def-suite libcmark/node
  :description "Testing node creation"
  :in libcmark)
(in-suite libcmark/node)


(defvar *node-types*
    '(:cmark-node-document
      :cmark-node-block-quote
      :cmark-node-list
      :cmark-node-item
      :cmark-node-code-block
      :cmark-node-html-block
      :cmark-node-custom-block
      :cmark-node-paragraph
      :cmark-node-heading
      :cmark-node-thematic-break
      :cmark-node-text
      :cmark-node-softbreak
      :cmark-node-linebreak
      :cmark-node-code
      :cmark-node-html-inline
      :cmark-node-custom-inline
      :cmark-node-emph
      :cmark-node-strong
      :cmark-node-link
      :cmark-node-image)
  "All the possible CMARK-NODE-TYPE keys because CFFI has no way of getting the
  keys from the type definition.")

;;; Creating and Destroying Nodes
(test default-memory-allocator-available
  "Whether the default memory allocator getter works; does not test the value
  of the result though."
  (finishes
    (let ((memory (foreign-alloc '(:struct cmark-mem))))
      (unwind-protect
          (progn
            (setf (mem-ref memory '(:pointer (:struct cmark-mem)))
                  (default-mem-allocator))
        (foreign-free memory))))))

(test node-creation-and-destruction
  "Create a node and safely destroy it."
  (finishes
    (dolist (type *node-types*)
      (let ((node (make-node type)))
        (free-node node)))))

