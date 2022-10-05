(defpackage #:cmark
  (:documentation "High-level interface to libcmark")
  (:use #:cl)
  (:export 
    ;; Parsing
    parse-document
    parse-stream
    make-streaming-parser
    feed-streaming-parser
    close-streaming-parser
    finish-streaming-parser
    with-streaming-parser
    ;; Node
    node document-node block-quote-node list-node item-node code-block-node
    html-block-node custom-block-node paragraph-node heading-node
    thematic-break-node text-node softbreak-node linebreak-node code-node
    html-inline-node custom-inline-node emph-node strong-node link-node
    image-node
    ;; Node accessors
    node-parent node-children node-literal
    node-list-type node-list-delim node-list-start
    node-heading-level
    node-title node-url
    ;; Node predicates
    orphanp childlessp inline-node-p block-node-p leaf-node-p
    ;; Tree traversal
    node-first-child node-last-child
    node-next node-previous
    node-next-sibling node-previous-sibling
    which-child
    ;; Tree manipulation
    unlink-node insert-node-before insert-node-after
    replace-node prepend-child-node append-child-node
    consolidate-text-nodes
    ;; Iteration
    done enter exit
    walk-tree
    make-iterator iterator-reset iterator-advance))
