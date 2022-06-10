(in-package #:libcmark/test)

(def-suite libcmark/version
  :description "Testing version information"
  :in libcmark)
(in-suite libcmark/version)


(test get-version-int
  "We can get the version number"
  (is (integerp (version))))

(test get-version-string
  "We can get the version string"
  (is (stringp (version-string))))
