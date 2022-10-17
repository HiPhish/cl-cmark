(asdf:defsystem "libcmark"
  :description "Common Lisp bindings to libcmark, the CommonMark reference implementation"
  :author "HiPhish <hiphish@posteo.de>"
  :license "BSD-2-Clause"
  :version (:read-file-line "version.txt")
  :serial t
  :depends-on ("cffi")
  :components ((:module "src"
                :components ((:module "libcmark"
                              :components ((:file "package")
                                           (:file "main")
                                           (:file "option")
                                           (:file "memory-allocator")
                                           (:file "node-constructor")
                                           (:file "tree-traversal")
                                           (:file "node-accessor")
                                           (:file "iterator")
                                           (:file "tree-manipulation")
                                           (:file "parsing")
                                           (:file "rendering")
                                           (:file "version")
                                           )))))
  :in-order-to ((test-op (test-op "libcmark/test"))))

(asdf:defsystem #:libcmark/test
  :description "Tests for the low-level libcmark bindings"
  :author "HiPhish <hiphish@posteo.de>"
  :license "BSD-2-Clause"
  :version (:read-file-line "version.txt")
  :serial t
  :depends-on ("libcmark" "fiveam")
  :serial t
  :components ((:module "test"
                :components ((:module "libcmark"
                              :components ((:file "package")
                                           (:file "main")
                                           (:file "node")
                                           (:file "tree-traversal")
                                           (:file "iterator")
                                           (:file "accessor")
                                           (:file "tree-manipulation")
                                           (:file "parsing")
                                           (:file "rendering")
                                           (:file "version"))))))
  :perform (test-op (o s)
             (symbol-call :libcmark/test :test-all)))
