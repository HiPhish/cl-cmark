;;;; Rendering a tree of nodes a target language
(in-package #:libcmark)

(defcfun ("cmark_render_xml" render-xml) :string
  "Render a node tree as XML. It is the caller's responsibility to free the
  returned buffer."
  (root :pointer)
  (options :int))

(defcfun ("cmark_render_html" render-html) :string
  "Render a node tree as an HTML fragment. It is up to the user to add an
appropriate header and footer. It is the caller's responsibility to
free the returned buffer."
  (root :pointer)
  (options :int))

(defcfun ("cmark_render_man" render-man) :string
  "Render a node tree as a groff man page, without the header. It is the
  caller's responsibility to free the returned buffer."
  (root :pointer)
  (options :int)
  (width :int))

(defcfun ("cmark_render_commonmark" render-commonmark) :string
  "Render a node tree as a commonmark document. It is the caller's
  responsibility to free the returned buffer."
  (root :pointer)
  (options :int)
  (width :int))

(defcfun ("cmark_render_latex" render-latex) :string
  "Render a node tree as a LaTeX document. It is the caller's responsibility to
  free the returned buffer."
  (root :pointer)
  (options :int)
  (width :int))
