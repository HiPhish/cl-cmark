;;;; Node accessor functions
(in-package #:libcmark)

(defcfun ("cmark_node_get_user_data" node-get-user-data) :pointer
  "Returns the user data of node."
  (node :pointer))

(defcfun ("cmark_node_set_user_data" node-set-user-data) :int
  "Sets arbitrary user data for node. Returns 1 on success, 0 on failure."
  (node :pointer)
  (user-data :pointer))

(defcfun ("cmark_node_get_type" node-get-type) cmark-node-type
  "Returns the type of node, or CMARK_NODE_NONE on error."
  (node :pointer))

(defcfun ("cmark_node_get_type_string" node-get-type-string) :string
  "Like NODE-GET-TYPE, but returns a string representation of the type, or
  \"<unknown>\"."
  (node :pointer))

(defcfun ("cmark_node_get_literal" node-get-literal) :string
  "Returns the string contents of NODE, or an empty string if none is set.
  Returns NULL if called on a node that does not have string content."
  (node :pointer))

(defcfun ("cmark_node_set_literal" node-set-literal) :int
  "Sets the string CONTENTS of NODE. Returns 1 on success, 0 on failure."
  (node :pointer)
  (content :string))

(defcfun ("cmark_node_get_heading_level" node-get-heading-level) :int
  "Returns the heading level of NODE, or 0 if node is not a heading."
  (node :pointer))

(defcfun ("cmark_node_set_heading_level" node-set-heading-level) :int
  "Sets the heading LEVEL of NODE, returning 1 on success and 0 on error."
  (node :pointer)
  (level :int))

(defcfun ("cmark_node_get_list_type" node-get-list-type) cmark-list-type
  "Returns the list type of NODE, or :CMARK_NO_LIST if node is not a list."
  (node :pointer))

(defcfun ("cmark_node_set_list_type" node-set-list-type) :int
  "Sets the list type of node, returning 1 on success and 0 on error."
  (node :pointer)
  (type cmark-list-type))

(defcfun ("cmark_node_get_list_delim" node-get-list-delim) cmark-delim-type
  "Returns the list delimiter type of NODE, or CMARK-NO-DELIM if node is not a
  list."
  (node :pointer))

(defcfun ("cmark_node_set_list_delim" node-set-list-delim) :int
  "Sets the list delimiter TYPE of NODE, returning 1 on success and 0 on
  error."
  (node :pointer)
  (delim cmark-delim-type))

(defcfun ("cmark_node_get_list_start" node-get-list-start) :int
  "Returns starting number of NODE, if it is an ordered list, otherwise 0."
  (node :pointer))

(defcfun ("cmark_node_set_list_start" node-set-list-start) :int
  "Sets STARTing number of NODE, if it is an ordered list.  Returns 1 on
  success, 0 on failure."
  (node :pointer)
  (start :int))

(defcfun ("cmark_node_get_list_tight" node-get-list-tight) :int
  "Returns 1 if NODE is a tight list, 0 otherwise."
  (node :pointer))

(defcfun ("cmark_node_set_list_tight" node-set-list-tight) :int
  "Sets the \"TIGHTness\" of a list. Returns 1 on success, 0 on failure."
  (node :pointer)
  (tight :int))

(defcfun ("cmark_node_get_fence_info" node-get-fence-info) :string
  "Returns the info string from a fenced code block."
  (node :pointer))

(defcfun ("cmark_node_set_fence_info" node-set-fence-info) :int
  "Sets the info string in a fenced code block, returning 1 on success and
; 0 on failure."
  (node :pointer)
  (info :string))

(defcfun ("cmark_node_get_url" node-get-url) :string
  "Returns the URL of a link or image NODE, or an empty string if no URL is
  set. Returns NULL if called on a node that is not a link or image."
  (node :pointer))

(defcfun ("cmark_node_set_url" node-set-url) :int
  "Sets the URL of a link or image NODE. Returns 1 on success, 0 on failure."
  (node :pointer)
  (url :string))

(defcfun ("cmark_node_get_title" node-get-title) :string
  "Returns the title of a link or image NODE, or an empty string if no title is
  set. Returns NULL if called on a node that is not a link or image."
  (node :pointer))

(defcfun ("cmark_node_set_title" node-set-title) :int
  "Sets the TITLE of a link or image NODE. Returns 1 on success, 0 on failure."
  (node :pointer)
  (title :string))

(defcfun ("cmark_node_get_on_enter" node-get-on-enter) :string
  "Returns the literal \"on enter\" text for a custom NODE, or an empty string
  if no on_enter is set. Returns NULL if called on a non-custom node."
  (node :pointer))

(defcfun ("cmark_node_set_on_enter" node-set-on-enter) :int
  "Sets the literal text to render ON-ENTER for a custom NODE.  Any children of
  the node will be rendered after this text. Returns 1 on success 0 on
  failure."
  (node :pointer)
  (on-enter :string))

(defcfun ("cmark_node_get_on_exit" node-get-on-exit) :string
  "Returns the literal \"on exit\" text for a custom node, or an empty string
  if no on_exit is set. Returns NULL if called on a non-custom node."
  (node :pointer))

(defcfun ("cmark_node_set_on_exit" node-set-on-exit) :int
  "Sets the literal text to render ON-EXIT for a custom NODE.  Any children of
  the node will be rendered before this text. Returns 1 on success 0 on
  failure."
  (node :pointer)
  (on-exit :string))

(defcfun ("cmark_node_get_start_line" node-get-start-line) :int
  "Returns the line on which NODE begins."
  (node :pointer))

(defcfun ("cmark_node_get_start_column" node-get-start-column) :int
  "Returns the column at which NODE begins."
  (node :pointer))

(defcfun ("cmark_node_get_end_line" node-get-end-line) :int
  "Returns the line on which NODE ends."
  (node :pointer))

(defcfun ("cmark_node_get_end_column" node-get-end-column) :int
  "Returns the column at which NODE ends."
  (node :pointer))
