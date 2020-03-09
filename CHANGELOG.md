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

## 0.1.3.0 -- 2020-03.08

* Change order of tags: type classes, type families and data type families are
  sorted before their instances.  If one is using multipe tags (the default),
  the order of them also matters (i.e. in the vim `tags` option).

## 0.1.3.0 -- ?

* Added path argument, can be passed using `-fplugin-opt=Plugin.GhcTags:../tags`.
* Wrapped `IOExceptions`, so when it will happen it will be obvious that the
  plugin failed not `ghc`
