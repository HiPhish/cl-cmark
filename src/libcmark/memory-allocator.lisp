;;;; Custom memory allocator support
(in-package #:libcmark)

(defcstruct cmark-mem
  "Defines the memory allocation functions to be used by CMark when
  parsing and allocating a document tree"
  (calloc :pointer)   ; void *(*calloc)(size_t, size_t);
  (realloc :pointer)  ; void *(*realloc)(void *, size_t);
  (free :pointer))    ; void (*free)(void *);

(defcfun ("cmark_get_default_mem_allocator" default-mem-allocator) (:pointer (:struct cmark-mem))
  "Returns a pointer to the default memory allocator")
