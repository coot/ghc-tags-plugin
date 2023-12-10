# Revision history for ghc-tags-core

## 0.6.1.0 -- 2023-12-10

- `GHC-9.8` support; droppped support of `GHC-{8.10, 9.0, 9.2}`.

## 0.6.0.0 -- 2023-05-01

- `GHC-9.6` support; dropped support of `GHC-8.8`.

## 0.5.0.0

- Removed dependency on `pipes` package, some functionality moved to
  `ghc-tags-pipes` package.

## 0.4.2.2

- Added `Ord` instances: `TagDefinition`, `TagFields`, `Tag`

## 0.4.2.1

- Changed the `ghc` cabal flag in `ghc-tags-core` into `ghc-lib` flag.

## 0.4.2.0

* Added `ghc` cabal flag, when not set, `ghc-tags-core` will be build with
  `ghc-lib` instead of `ghc` library.  By default `ghc-tags-core` is built with
  `ghc` library.

## 0.4.0.0

* Record tags for instance methods.
* Added instance method tag field which records instance head
* Changed `Semigroup` instance for `TagFields` (a left biased monoid isomorphic
  with `Map` monoid instance, rather than the free monoid)
* Added `Semigroup` instance for `Tag` type (a left biased monoid)
* Changed `TagKind` type: not indexed by `TAG_KIND`
* Added `CTagMap` and `ETagMap` with parsers & formatters (for used by
  `ghc-tag`)

## 0.3.1.1 -- 2022-08-05

* Support `GHC-9.4`

## 0.2.4.0 -- 2020-09-08

* Type family and data type family kind contains all bound type variables (with
  their kind if they are given)
* Type family instances: include the resulting type
* Data type family instances: include the resulting kind
* Data constructor tags cary the contstructor declaration 

## 0.2.3.0 -- 2020-08-07

* Added `hsDeclsToGhcTags`.
* Type signatures for class method.

## 0.2.0.0 -- 2020-04-12

* Use `Text` to represent file names; Parsers are using `ByteString` as input
  rather than `Text`, which allows to normalise `FilePath` when parsing data
  using `filepath-bytestring` library.

## 0.1.0.0 -- 2020-03-24

* Normalise 'tagFilePath' for tags which are returned by the parsers.
* Added `GhcTag`, some of the constructor contains type level information which
  is used to form `CTagFields`.
* Added ctag [pseudo header](https://docs.ctags.io/en/latest/man/ctags-client-tools.7.html#pseudo-tags) parser

## 0.2.4.1 -- 2021-03-15

* Support `GHC-9.0`
* vim-plugin: better parser of ghc-pkg output
