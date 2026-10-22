# Revision history for ghc-tags-pipes

## Unreleased

- Recover streamed tags after malformed lines. Add `tagParserWithWarnings` to
  report skipped records without stopping tag updates.

- Add a complete streaming merge that removes duplicates from old and new tags,
  retaining distinct tags with equal sorting keys.

- Match complete module paths when merging streamed tags, preserving files with
  matching filename or directory suffixes.

## 0.1.1.0 -- 2023-05-01

* `GHC-9.6` support.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
