(asdf:defsystem #:cmark
  :description "Common Lisp bindings to libcmark, the CommonMark reference implementation"
  :author "HiPhish <hiphish@posteo.de>"
  :license "BSD-2-Clause"
  :version (:read-file-line "version.txt")
  :serial t
  :depends-on ("libcmark" "cffi" "flexi-streams")
  :components ((:module "src"
                :components ((:module "cmark"
                              :components ((:file "package")
                                           (:file "conditions")
                                           (:file "node")
                                           (:file "iteration")
                                           (:file "tree-manipulation")
                                           (:file "tree-traversal")
                                           (:file "from-foreign")
                                           (:file "parsing"))))))
  :in-order-to ((test-op (test-op "cmark/test"))))

(asdf:defsystem #:cmark/test
  :description "Tests for cl-cmark"
  :author "HiPhish <hiphish@posteo.de>"
  :license "BSD-2-Clause"
  :version (:read-file-line "version.txt")
  :serial t
  :depends-on (#:cmark "fiveam" "cl-fad")
  :serial t
  :components ((:module "test"
                :components ((:module "cmark"
                              :components ((:file "package")
                                           (:file "main")
                                           (:file "tree-manipulation")
                                           (:file "tree-traversal")
                                           (:file "iteration")
                                           (:file "parsing"))))))
  :perform (test-op (o s)
             (symbol-call :cmark/test :test-all)))
