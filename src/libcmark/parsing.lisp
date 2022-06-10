;;;; Parsing text into a tree of nodes
(in-package #:libcmark)

(defcfun ("cmark_parser_new" make-parser) :pointer
  "Creates a new parser object."
  (options :int))

(defcfun ("cmark_parser_new_with_mem" make-parser-with-mem) :pointer
  "Creates a new parser object with the given MEMory allocator"
  (options :int)
  (mem (:pointer (:struct cmark-mem))))

(defcfun ("cmark_parser_free" free-parser) :void
  "Frees memory allocated for a parser object."
  (parser :pointer))

(defcfun ("cmark_parser_feed" parser-feed) :void
  "Feeds a STRING of length LEN to parser."
  (parser :pointer)
  (buffer :string)
  (len :size))

(defcfun ("cmark_parser_finish" parser-finish) :pointer
  "Finish parsing and return a pointer to a tree of nodes."
  (parser :pointer))

(defcfun ("cmark_parse_document" parse-document) :pointer
  "Parse a CommonMark document in BUFFER of length LEN.  Returns a pointer to a
  tree of nodes. The memory allocated for the node tree should be released
  using cmark_node_free when it is no longer needed."
  (buffer :string)
  (len :size)
  (options :int))

(defcfun ("cmark_parse_file" parse-file) :pointer
  "Parse a CommonMark document in file FILE, returning a pointer to a tree of
  nodes. The memory allocated for the node tree should be released using
  cmark_node_free when it is no longer needed."
  (file :pointer)
  (options :int))
