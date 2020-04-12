# Revision history for ghctags

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## 0.1.1.0 -- 2020-03-03

* Added support for tag's kinds.
* Added various file headers

## 0.1.2.0 -- 2020-03-05

* Preserve tag information in ctags generated files
* Support for `file:` tags (exported / not exported terms)
* Added a test-suite (golden tests and property tests)

## 0.1.3.0 -- 2020-03-08

* Change order of tags: type classes, type families and data type families are
  sorted before their instances.  If one is using multipe tags (the default),
  the order of them also matters (i.e. in the vim `tags` option).

## 0.1.4.0 -- 2020-03-11

* Tags for default instances of associated (data) type familes.
* Added path argument, can be passed using `-fplugin-opt=Plugin.GhcTags:../tags`.
* Wrapped `IOExceptions`, so when it will happen it will be obvious that the
  plugin failed not `ghc`
* Fixed the tag ordering function to be fullfil the transitivness property.

## 0.1.5.0 -- 2020-03-13

* concurrency safety - protection `tags` file using a file lock

## 0.1.6.0 -- 2020-03-24

* support for etags files
* various bug fixes
* type level information (not type checked!), from the parsed tree, including:
  type of instances (instance context & instance head), types of `GADTs`
  constructors, rhs of type synonyms, kinds of type or data families.
* expanded ctags pseudo tags with descriptions of fields and tag kinds

## 0.2.0.0 -- 2020-04-12

* Fixed bug #37
* Added `--debug` flag
