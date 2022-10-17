(defpackage #:libcmark
  (:documentation
    "Low-level Common Lisp bindings to the libcmark C library. See also the cmark(3) man page.")
  (:use #:cl #:cffi)
  (:export #:markdown-to-html
           #:cmark-node-type #:cmark-list-type #:cmark-delim-type
           #:cmark-mem #:default-mem-allocator
           #:make-node #:make-node-with-mem #:free-node
           #:node-next #:node-previous #:node-parent #:node-first-child #:node-last-child
           #:cmark-event-type
           #:make-iterator #:free-iterator
           #:iterator-next #:iterator-get-node #:iterator-get-event-type
           #:iterator-get-root #:iterator-reset
           #:node-get-user-data #:node-set-user-data #:node-get-type #:node-get-type-string
           #:node-get-literal #:node-set-literal
           #:node-get-heading-level #:node-set-heading-level
           #:node-get-list-type #:node-set-list-type
           #:node-get-list-delim #:node-set-list-delim
           #:node-get-list-start #:node-set-list-start
           #:node-get-list-tight #:node-set-list-tight
           #:node-get-fence-info #:node-set-fence-info
           #:node-get-url #:node-set-url #:node-get-title #:node-set-title
           #:node-get-on-enter #:node-set-on-enter #:node-get-on-exit #:node-set-on-exit
           #:node-get-start-line #:node-get-start-column
           #:node-get-end-line #:node-get-end-column
           #:node-unlink #:node-insert-before #:node-insert-after #:node-replace
           #:node-prepend-child #:node-append-child #:consolidate-text-nodes
           #:make-parser #:make-parser-with-mem #:free-parser
           #:parser-feed #:parser-finish #:parse-document #:parse-file
           #:render-xml #:render-html #:render-man #:render-commonmark #:render-latex
           #:+cmark-opt-default+
           #:+cmark-opt-sourcepos+ #:+cmark-opt-hardbreaks+ #:+cmark-opt-safe+
           #:+cmark-opt-unsafe+ #:+cmark-opt-nobreaks+
           #:+cmark-opt-normalize+ #:+cmark-opt-validate-utf8+ #:+cmark-opt-smart+
           #:version #:version-string))
