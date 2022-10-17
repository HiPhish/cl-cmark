;;;; Various definitions used in different parts of the library. This file
;;;; contains all constants and type declarations from the man page, even if
;;;; their definitions are scattered throughout the man page otherwise. It is
;;;; easier to keep them all in one place.
(in-package #:libcmark)

(define-foreign-library libcmark
  (:unix (:or "libcmark.so.0.30.2" "libcmark.so.0" "libcmark"))
  (:t (:default "libcmark")))
(use-foreign-library libcmark)


(defcfun ("cmark_markdown_to_html" markdown-to-html) :string
  "Convert TEXT (assumed to be a UTF-8 encoded string with length LEN) from
  CommonMark Markdown to HTML, returning a null-terminated, UTF-8-encoded
  string. It is the caller's responsibility to free the returned buffer."
  (text :string)
  (len :size)
  (options :int))

;;; ENUMERATIONS
;;; Various enumerations
(defcenum cmark-node-type
  "Kind of Markdown node"
  :cmark_node_none
  ;; Block
  :cmark-node-document
  :cmark-node-block-quote
  :cmark-node-list
  :cmark-node-item
  :cmark-node-code-block
  :cmark-node-html-block
  :cmark-node-custom-block
  :cmark-node-paragraph
  :cmark-node-heading
  :cmark-node-thematic-break
  ;; Inline
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

(defcenum cmark-list-type
  "Kind of Markdown list"
  :cmark-no-list
  :cmark-bullet-list
  :cmark-ordered-list)

(defcenum cmark-delim-type
  "Kind of Markdown delimiter"
  :cmark-no-delim
  :cmark-period-delim
  :cmark-paren-delim)
