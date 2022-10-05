;;;; The node classes and functions which operate on nodes alone

(in-package #:cmark)

(defclass node ()
  ((parent :initarg parent :reader node-parent :initform nil :type (or node null)
           :documentation "Parent node of this node, or NIL if this is a root node")
   (children :initarg :children :reader node-children :initform nil :type list
             :documentation "All child nodes of this node")
   (user-data :initarg :user-data :accessor node-user-data :type integer
              :documentation "User data of the node")
   (start-line :initarg :start-line :reader node-start-line :type (integer 0 *)
               :documentation "Line at which the node begins")
   (start-column :initarg :start-column :reader node-start-column :type (integer 0 *)
                 :documentation "Column at which the node begins")
   (end-line :initarg :end-line :reader node-end-line :type (integer 0 *)
               :documentation "Line at which the node ends")
   (end-column :initarg :end-column :reader node-end-column :type (integer 0 *)
                 :documentation "Column at which the node ends"))
  (:documentation "Base class of all cmark nodes."))

(defclass document-node (node)
  ()
  (:documentation "Root node of the entire document"))

(defclass block-quote-node (node)
  ()
  (:documentation "Represents a block quotation."))

(defclass list-node (node)
  ((type :initarg :type :accessor node-list-type :type (member :cmark-bullet-list :cmark-ordered-list)
         :documentation "The type of list")
   (delim :initarg :delim :accessor node-list-delim :type (member :period :paren)
          :documentation "The delimiter type of the list")
   (start :initarg :start :accessor node-list-start :type (integer 0 *)
          :documentation "Starting number of the node (zero for unordered lists)")
   (tightp :initarg :tightp :accessor node-list-tight-p :type boolean
           :documentation "Whether the node is a tight list"))
  (:documentation
   "A list node, it can be either an unordered list (:BULLET-LIST) or an :ORDERED-LIST."))

(defclass item-node (node)
  ()
  (:documentation "Represents and item inside a list (bullet or unordered)."))

(defclass code-block-node (node)
  ((fence-info :initarg :fence-info :accessor node-fence-info :type string
               :documentation "Info string from a fenced code block")
   (literal :initarg :literal :accessor node-literal :type (or string null)
            :documentation "Text of the node (might be empty)."))
  (:documentation "A block of code to present verbatim."))

(defclass html-block-node (node)
  ((literal :initarg :literal :accessor node-literal :type (or string null)
            :documentation "Text of the node (might be empty)."))
  (:documentation "Block-level embedded HTML code."))

(defclass custom-block-node (node)
  ((on-enter :initarg :on-enter :accessor node-on-enter :type string
             :documentation "Literal text to render on entering a custom node")
   (on-exit :initarg :on-exit :accessor node-on-exit :type string
            :documentation "Literal text to render on exiting a custom node"))
  (:documentation "A user-defined node which produces custom text."))

(defclass paragraph-node (node)
  ()
  (:documentation "A paragraph of the document."))

(defclass heading-node (node)
  ((level :initarg :level :accessor node-heading-level :type (integer 1 6)
          :documentation "Level of the heading"))
  (:documentation "A heading with variable level."))

(defclass thematic-break-node (node)
  ()
  (:documentation "Represents a thematic break within the content of the document."))

; /* inline */
(defclass text-node (node)
  ((literal :initarg :literal :accessor node-literal :type (or string null)
            :documentation "Text of the node (might be empty)."))
  (:documentation "Plain text content."))

(defclass softbreak-node (node)
  ()
  (:documentation "A line break which may be rendered differently depending on the rendering options."))

(defclass linebreak-node (node)
  ()
  (:documentation "A hard line break which does not terminate the current block level element."))

(defclass code-node (node)
  ((literal :initarg :literal :accessor node-literal :type (or string null)
            :documentation "Text of the node (might be empty)."))
  (:documentation "Inline verbatim code."))

(defclass html-inline-node (node)
  ((literal :initarg :literal :accessor node-literal :type (or string null)
            :documentation "Text of the node (might be empty)."))
  (:documentation "Inline embedded HTML code."))

(defclass custom-inline-node (node)
  ((on-enter :initarg :on-enter :accessor node-on-enter :type string
             :documentation "Literal text to render on entering a custom node")
   (on-exit :initarg :on-exit :accessor node-on-exit :type string
            :documentation "Literal text to render on exiting a custom node"))
  (:documentation "A user-defined node which produces custom text."))

(defclass emph-node (node)
  ()
  (:documentation "Container for emphatic text."))

(defclass strong-node (node)
  ()
  (:documentation "Container for strong text."))

(defclass link-node (node)
  ((url :initarg :url :accessor node-url :type string
        :documentation "URL of a link node")
   (title :initarg :title :accessor node-title :type string
          :documentation "Title of the node (empty if there is no title)"))
  (:documentation "Hyperlink with target URL and title."))

(defclass image-node (node)
  ((url :initarg :url :accessor node-url :type string
        :documentation "URL of an image node")
   (title :initarg :title :accessor node-title :type string
          :documentation "Title of the node (empty if there is no title)"))
  (:documentation "Hyperlink with target URL and alt-text."))

(defun orphanp (node)
  "Whether NODE is an orphan, meaning it has no parent node."
  (declare (type node node))
  (null (node-parent node)))

(defun childlessp (node)
  "Whether NODE has no children"
  (declare (type node node))
  (null (node-children node)))

(defgeneric inline-node-p (node)
  (:documentation
  "Whether a node is an inline node (as opposed to a block-level node).")
  (:method ((node               node)) nil)
  (:method ((node          text-node))   t)
  (:method ((node     softbreak-node))   t)
  (:method ((node     linebreak-node))   t)
  (:method ((node          code-node))   t)
  (:method ((node   html-inline-node))   t)
  (:method ((node custom-inline-node))   t)
  (:method ((node          emph-node))   t)
  (:method ((node        strong-node))   t)
  (:method ((node          link-node))   t)
  (:method ((node         image-node))   t))

(defgeneric block-node-p (node)
  (:documentation
  "Whether a node is a block-level node (as opposed to an inline node).")
  (:method ((node                node)) nil)
  (:method ((node       document-node))   t)
  (:method ((node    block-quote-node))   t)
  (:method ((node           list-node))   t)
  (:method ((node           item-node))   t)
  (:method ((node     code-block-node))   t)
  (:method ((node     html-block-node))   t)
  (:method ((node   custom-block-node))   t)
  (:method ((node      paragraph-node))   t)
  (:method ((node        heading-node))   t)
  (:method ((node thematic-break-node))   t))

(defgeneric leaf-node-p (node)
  (:documentation
   "Whether a node is a leaf-type node or not.")
  (:method ((node                node)) nil)
  (:method ((node     html-block-node))   t)
  (:method ((node thematic-break-node))   t)
  (:method ((node     code-block-node))   t)
  (:method ((node           text-node))   t)
  (:method ((node      softbreak-node))   t)
  (:method ((node      linebreak-node))   t)
  (:method ((node           code-node))   t)
  (:method ((node    html-inline-node))   t))
