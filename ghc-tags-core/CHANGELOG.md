# Revision history for ghc-tags-core

## 0.1.0.0 -- 2020-03-24

* First version. Released on an unsuspecting world.
* Normalise 'tagFilePath' for tags which are returned by the parsers.
* Added `GhcTag`, some of the constructor contains type level information which
  is used to form `CTagFields`.
