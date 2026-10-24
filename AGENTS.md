* when writing tests:
  - use QuickCheck
  - use Arbtrary instances rather than forAll* functions
  - one property at a time
  - if genrated data needs an invariant to satisfy write a fix function, which
    can be re-used in the shrinker.
