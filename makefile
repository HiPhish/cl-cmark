doc: doc/cl-cmark.info doc/cl-cmark.html

doc/cl-cmark.info: doc/cl-cmark.texi
	texi2any doc/cl-cmark.texi -o doc/cl-cmark.info

doc/cl-cmark.html: doc/cl-cmark.texi
	texi2any --html -c TOP_NODE_UP_URL=https://example.com doc/cl-cmark.texi -o doc/cl-cmark.html

doc/cl-cmark.pdf: doc/cl-cmark.texi
	texi2pdf --pdf --build=tidy --build-dir=doc doc/cl-cmark.texi -o doc/cl-cmark.pdf
