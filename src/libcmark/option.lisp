;;;; Option constants; these are meant to be ORed together
(in-package #:libcmark)

(defconstant +cmark-opt-default+ 0
  "Default options.")

;;; Options affecting rendering
(defconstant +cmark-opt-sourcepos+ (ash 1 1)
  "Include a data-sourcepos attribute on all block elements.")

(defconstant +cmark-opt-hardbreaks+ (ash 1 2)
  "Render softbreak elements as hard line breaks.")

(defconstant +cmark-opt-safe+ (ash 1 3)
  "+CMARK-OPT-SAFE+ is defined here for API compatibility, but it no longer has
  any effect. \"Safe\" mode is now the default: set +CMARK-OPT-UNSAFE+ to
  disable it.")

(defconstant +cmark-opt-unsafe+ (ash 1 17)
  "Render raw HTML and unsafe links (javascript:, vbscript:, file:, and
  data:, except for image/png, image/gif, image/jpeg, or image/webp mime
  types). By default, raw HTML is replaced by a placeholder HTML comment.
  Unsafe links are replaced by empty strings.")

(defconstant +cmark-opt-nobreaks+ (ash 1 4)
  "Render softbreak elements as spaces.")


;;; Options affecting parsing
(defconstant +cmark-opt-normalize+ (ash 1 8)
  "Legacy option (no effect).")

(defconstant +cmark-opt-validate-utf8+ (ash 1 9)
  "Validate UTF-8 in the input before parsing, replacing illegal sequences with
  the replacement character U+FFFD.")

(defconstant +cmark-opt-smart+ (ash 1 10)
  "Convert straight quotes to curly, --- to em dashes, -- to en dashes.")
