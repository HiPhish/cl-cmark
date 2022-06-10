(in-package #:libcmark/test)

(def-suite libcmark/accessor
  :description "Testing node accessor functions"
  :in libcmark)
(in-suite libcmark/accessor)


(test node-get/set-user-data
  "We can get and set the user data of a node."
  (finishes
    (with-document (node)
      (let ((user-data (node-get-user-data node)))
        (node-set-user-data node user-data)))))

(test node-get-node-type
  "We can get the type of a node as a keyword"
  (finishes
    (with-document (node)
      (node-get-type node))))

(test node-get-node-type-string
  "We can get the type of a node as a string"
  (finishes
    (with-document (node)
      (node-get-type-string node))))

(test node-get/set-node-literal
  "We can get and set the string contents of a node"
  (finishes
    (with-document (node)
      (node-get-literal node)
      (node-set-literal node "xxx"))))

(test node-get/set-node-heading-level
  "We can get and set the heading level of a node"
  (finishes
    (with-document (node)
      (node-get-heading-level node)
      (node-set-heading-level node 3))))

(test node-get/set-list-type
  "We can get and set the list type of a node"
  (finishes
    (with-document (node)
      (node-get-list-type node)
      (node-set-list-type node :cmark-bullet-list))))

(test node-get/set-list-delim
  "We can get and set the list delimiter of a node"
  (finishes
    (with-document (node)
      (node-get-list-delim node)
      (node-set-list-delim node :cmark-period-delim))))

(test node-get/set-list-start
  "We can get and set the list start of a node"
  (finishes
    (with-document (node)
      (node-get-list-start node)
      (node-set-list-start node 123))))

(test node-get/set-list-tight
  "We can get and set the tightness of a node"
  (finishes
    (with-document (node)
      (node-get-list-tight node)
      (node-set-list-tight node 1))))

(test node-get/set-fence-info
  "We can get and set the fence-info of a node"
  (finishes
    (with-document (node)
      (node-get-fence-info node)
      (node-set-fence-info node "xxx"))))

(test node-get/set-url
  "We can get and set the url of a node"
  (finishes
    (with-document (node :document "[example.com](https://www.example.com)")
      (node-get-url node)
      (node-set-url node "https://www.example.com"))))

(test node-get/set-title
  "We can get and set the title of a node"
  (finishes
    (with-document (node :document "[example.com](https://www.example.com)")
      (node-get-title node)
      (node-set-title node "xxx"))))

(test node-get/set-on-enter
  "We can get and set the on-enter text of a node"
  (finishes
    (with-document (node)
      (node-get-on-enter node)
      (node-set-on-enter node "xxx"))))

(test node-get/set-on-exit
  "We can get and set the on-exit text of a node"
  (finishes
    (with-document (node)
      (node-get-on-exit node)
      (node-set-on-exit node "xxx"))))

(test node-get-start-line
  "We can get the first line number of a node"
  (finishes
    (with-document (node)
      (node-get-start-line node))))

(test node-get-start-column
  "We can get the first column number of a node"
  (finishes
    (with-document (node)
      (node-get-start-column node))))

(test node-get-end-column
  "We can get the last line number of a node"
  (finishes
    (with-document (node)
      (node-get-end-line node))))

(test node-get-end-column
  "We can get the last column number of a node"
  (finishes
    (with-document (node)
      (node-get-end-column node))))

