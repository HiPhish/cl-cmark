(in-package #:libcmark/test)

(def-suite libcmark/rendering
  :description "Testing document rendering"
  :in libcmark)
(in-suite libcmark/rendering)


(test render-xml
  "We can render a document as XML"
  (is (stringp (with-document (root)
                 (render-xml root +CMARK-OPT-DEFAULT+)))))

(test render-html
  "We can render a document as HTML"
  (is (stringp (with-document (root)
                 (render-html root +CMARK-OPT-DEFAULT+)))))

(test render-man
  "We can render a document as a man page"
  (is (stringp (with-document (root)
                 (render-man root +CMARK-OPT-DEFAULT+ 80)))))

(test render-commonmark
  "We can render a document as CommonMark"
  (is (stringp (with-document (root)
                 (render-commonmark root +CMARK-OPT-DEFAULT+ 80)))))

(test render-latex
  "We can render a document as LaTeX"
  (is (stringp (with-document (root)
                 (render-latex root +CMARK-OPT-DEFAULT+ 80)))))
