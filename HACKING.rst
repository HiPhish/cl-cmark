.. default-role:: code


#####################################
 Hacking on cl-cmark and cl-libcmark
#####################################


The libcmark system
###################

This system is a very thin wrapper around the C library, it's mostly a 1:1
translation. The project organization reflects the structure of the official
man page (`cmark(3)`). All bindings were created manually by going through the
man page and converting the C definitions (data types and functions) to the
corresponding Common Lisp.

If you wish to extend cl-cmark please do likewise. Find where in the man page
the new additions are placed, find the corresponding places in the Lisp code,
and add the new code in between the already existing code.


Project organization
====================

See the ASDF definition file for the order the files are sourced in. The first
comment of each file explains what part of the man page it corresponds to. We
try to wrap everything for the sake of completeness, even if it might not be
particularly useful, such as the "simple interface".


Docstrings
==========

The docstrings are copied word for word from the man page. Only make changes
when necessary or if it better fits with the way Lisp works (e.g. upcase names
because all symbols are upper-case in Lisp).

If there is no description in the man page (e.g. for the structure definitions)
make up a docstring. If in the future the man page does get a description
replace the custom docstring with the official description.


On resources
============

C has no resource management facilities, you are expected to explicitly
allocate and release resources. In Lisp it is customary to provide some sort of
`WITH-...` macro. Do not do this in the cl-libcmark system, this system is only
a thin wrapper. A safe interface should be implemented on top of these
primitives by a higher-level system such as cl-cmark.


Testing
=======

We only test the interface, not the functionality. Assume that the C
implementation of is correct and test that the bindings actually bind the
correct function. One test per function should suffice.


The cmark system
================

This is a lispy system which is built on top of cl-libcmark. The user should
ideally not even be aware that it is built on top of a C library. Write it as
if you were writing a pure Common Lisp from scratch. Examples:

- Use CLOS classes to represent nodes
- Provide safe `WITH-...` macros for code that manages resources
- Signal conditions instead of using error codes to communicate failure
- Do add additional functionality not present in libcmark if it makes sense
- Only use foreign (C) objects internally, such as creating an intermediate
  foreign node to construct a native (Lisp) node
- Avoid native objects holding on to foreign objects where feasible. Example
  and counterexample:
   - native nodes to not wrap foreign nodes because native nodes can do
     everything foreign ones do
   - native parsers do map foreign parsers because they need to delegate to
     their parser throughout their lifetime

If at some point in the future one decides to replace cl-libcmark with a pure
native Common Lisp implementation the public API of cl-cmark should not have to
change.


Making a new release
####################

This project follows `semantic versioning`_.

- Add pre-release changes to the changelog under the heading of `Unreleased`
- When creating a new release change `Unreleased` to the actual version number
- Always write the friendly manual (WFTM) after each change
- Bump the version number in `version.txt`
- Bump the version number in the manual (`doc/cl-cmark.texi`)
- Tag the release commit with `v<x>.<y>.<z>`, where `x`, `y` and `z` are the
  major, minor and patch version respectively.

Whenever a feature is ready (implemented and tested) it should be merged into
the `master` branch. The `master` branch should always be in a working state.
It does not matter if not all features are complete as long as the features
which are are not broken. If a feature is not documented or if it is marked as
"experimental" in the manual it is OK to break it during a minor or patch
release. But ideally there should be nothing incomplete in `master` anyway.



.. _semantic versioning: https://semver.org/
