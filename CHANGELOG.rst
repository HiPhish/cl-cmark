.. default-role:: code

###########
 Changelog
###########

All notable changes to this project will be documented in this file.

The format is based on `Keep a Changelog`_, and this project adheres to
`Semantic Versioning`_.


[0.5.1] - 2023-01-15
####################

Added
=====

- HACKING file

Fixed
=====

- Initarg for slot `PARENT` of class `CMARK:CHILD-NODE` is now a keyword


Changed
=======

- All exports are now denoted by uninterned keyword symbols
- Condition `PARSER-EXHAUSTED` is now of type `SIMPLE-ERROR`
- Condition `UNEXPECTED-ORPHAN` is now of type `SIMPLE-ERROR`
- Condition `UNEXPECTED-PARENT` is now of type `SIMPLE-ERROR`


[0.5.1] - 2022-10-16
####################

Fixed
=====

- Wrong version number in ASDF definition files
- Wrong version number in user manual


[0.5.0] - 2022-10-14
####################

Added
=====

- Class `PARENT-NODE`
- Class `CHILD-NODE`
- Class `BLOCK-NODE`
- Class `INLINE-NODE`


Changed
=======

- Renamed condition `ORPHAN-NODE` to `UNEXPECTED-ORPHAN`
- Renamed condition `CHILD-NODE` to `UNEXPECTED-CHILD`
- Concrete node classes now inherit from `PARENT-NODE` and/or `CHILD-NODE`
- Concrete node classes now inherit from either `BLOCK-NODE` or `INLINE-NODE`


[0.4.0] - 2022-10-13
####################

Fixed
=====

- Child node getting dropped when calling `INSERT-NODE-BEFORE`


Added
=====
- Restart `DETACH-FROM-PARENT` in `APPEND-CHILD-NODE`
- Restart `DETACH-FROM-PARENT` in `PREPEND-CHILD-NODE`
- Restart `PREPEND-TO-PARENT` in `REPLACE-NODE`
- Restart `APPEND-TO-PARENT` in `REPLACE-NODE`
- Restart `INSERT-BEFORE-SIBLING` in `REPLACE-NODE`
- Restart `INSERT-AFTER-SIBLING` in `REPLACE-NODE`
- Restart `DETACH-FROM-PARENT` in `INSERT-NODE-BEFORE`
- Restart `PREPEND-TO-PARENT` in `INSERT-NODE-BEFORE`
- Restart `APPEND-TO-PARENT` in `INSERT-NODE-BEFORE`
- Restart `INSERT-BEFORE-SIBLING` in `INSERT-NODE-BEFORE`
- Restart `INSERT-AFTER-SIBLING` in `INSERT-NODE-BEFORE`
- Restart `DETACH-FROM-PARENT` in `INSERT-NODE-AFTER`
- Restart `PREPEND-TO-PARENT` in `INSERT-NODE-AFTER`
- Restart `APPEND-TO-PARENT` in `INSERT-NODE-AFTER`
- Restart `INSERT-BEFORE-SIBLING` in `INSERT-NODE-AFTER`
- Restart `INSERT-AFTER-SIBLING` in `INSERT-NODE-AFTER`


Changed
=======

- Minor corrections and clarifications in the manual


[0.3.0] - 2022-10-08
####################

Changed
=======
- `LIBCMARK:NODE-GET-TITLE` returns a string instead of a pointer
- Type of `CMARK:NODE-LIST-TYPE` is now either `:CMARK-BULLET-LIST` or
  `:CMARK-ORDERED-LIST`, rather than `:BULLET-LIST` or `:ORDERED-LIST`
- Functions which used to signal `ERROR` now signal a more specific condition


Added
=====

- Preliminary manual
- Stream parser
- Proper export of symbols in the `cmark` package in accord with the manual
- Filled in missing docstrings
- Generic function `cmark:leaf-node-p`
- Conditions `parser-exhausted`, `orphan-node` and `child-node`


[0.2.0] - 2022-06-17
####################

Added
=====

- Streaming parser
- Parser keyword option `:smart` for smart text conversion, e.g. straight
  double quotes to proper English quotation marks


[0.1.0] - 2022-06-10
####################

Initial release.

.. _Keep a Changelog: https://keepachangelog.com/en/1.0.0/
.. _Semantic Versioning: https://semver.org/spec/v2.0.0.html
