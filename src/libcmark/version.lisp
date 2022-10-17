;;;; Version information.
(in-package #:libcmark)

(defcfun ("cmark_version" version) :int
  "The library version as integer for runtime checks.

  • Bits 16-23 contain the major version.
  • Bits 8-15 contain the minor version.
  • Bits 0-7 contain the patchlevel.

  In hexadecimal format, the number 0x010203 represents version 1.2.3.")

(defcfun ("cmark_version_string" version-string) :string
  "The library version string for runtime checks.")
