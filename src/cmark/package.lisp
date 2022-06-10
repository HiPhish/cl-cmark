(defpackage #:cmark
  (:documentation "High-level interface to libcmark")
  (:use #:cl)
  (:export node
           document-node
           block-quote-node
           list-node))
