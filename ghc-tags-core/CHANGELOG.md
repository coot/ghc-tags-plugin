# Revision history for ghc-tags-core

## 0.1.0.0 -- 2020-03-24

* Normalise 'tagFilePath' for tags which are returned by the parsers.
* Added `GhcTag`, some of the constructor contains type level information which
  is used to form `CTagFields`.
* Added ctag [pseudo header](https://docs.ctags.io/en/latest/man/ctags-client-tools.7.html#pseudo-tags) parser

## 0.2.0.0 -- 2020-04-12

* Use `Text` to represent file names; Parsers are using `ByteString` as input
  rather than `Text`, which allows to normalise `FilePath` when parsing data
  using `filepath-bytestring` library.

## 0.2.3.0 -- 2020-08-07

* Added `hsDeclsToGhcTags`.
* Type signatures for class method.

## next version -- not published

* Type family and data type family kind contains all bound type variables (with their kind if they are given)
* Type family instances: include the resulting type
* Data type family instances: include the resulting kind
