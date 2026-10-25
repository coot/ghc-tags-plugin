# Revision history for ghctags

## Unreleased

- Correct export flags for associated type/data family instances and type defaults.
  Tag defaults using the family name rather than the right-hand-side type, and
  keep data-instance constructors private unless their exports include them.

- Omit `file:` on default and instance methods exported through their class,
  including instance signatures, while retaining it on nested local bindings.

- Emit pattern synonym signatures with the signature kind (`⊢`) and a `type:`
  field, suppressing the corresponding pattern definition tags. Unsigned pattern
  definitions retain the pattern kind (`p`).

- Add `--qualified` to emit defining-module-qualified names alongside ordinary
  names, including Template Haskell declarations, in ctags and etags output.

- Extend `gtp-check` with duplicate declaration reports, source file and line
  checks, and comparison of ctags and etags declaration locations, preserving
  duplicate counts and resolving paths relative to each tags file.


- Resolve relative tags paths from the nearest `cabal.project` or `stack.yaml`,
  honoring `STACK_YAML`. Warn and fall back to the package directory when no
  project configuration is found.

- Extend `gtp-check` with `--etags`/`-e`, parsed tag counts, ctags program/version
  metadata, parser diagnostics and warnings for unsorted ctags files.

- Refresh ctags metadata during Template Haskell updates, replacing stale or
  missing headers. Correct signature and type/data-family kind descriptions.

- Keep local bindings when an unrelated binding with the same name has a signature.

- Filter etags declarations before applying file/line ordering, consistently
  removing redundant definitions and type constructors in both output formats.

- Apply signature filtering to Template Haskell updates, removing redundant
  function tags even when generated declarations share the same source location.

- Preserve source filenames when Template Haskell updates an etags file
  containing tags from multiple modules.

- Recover valid tags before and after malformed records, report skipped input,
  and continue updating damaged tags files, including streaming and TH updates.

- Remove duplicate tags consistently in streaming and non-streaming updates,
  including when the existing tags file is empty.

- Identify compiled modules relative to the tags directory, keeping modules with
  the same source path in different packages separate in shared tags files.

## 0.6.2.0 -- 2026-03-08

- `GHC-9.14` support.
- `GHC-9.12` support.

## 0.6.1.1 -- 2024-07-21

- `GHC-9.10` support.

## 0.6.0.0 -- 2023-05-01

- `GHC-9.6` support; dropped support of `GHC-8.8`.

## 0.5.1.0

- tags for local bindings (from `where` clauses)
- tags for modules

## 0.5.0.0

- tags of instance methods & signatures
- fixed a case in which generated ctags where missing headers

## 0.4.0.2

* Fixed a bug: no headers for ctag file when `--stream` option was not set.

## 0.4.0.1

* `ghc-tags-plugin` is now compatible with `GHC-9.4`.
  **NOTE**: `GHC-9.4` includes a [fix][ghc-issue-20417], which makes `cabal`
  cache mechanism work properly when plugins are enabled.

## 0.4.0.0 -- 2022-01-09

* `ghc-tags-plugin` is now compatible with `GHC-9.2`
* `--stream` option, only effective for `ctags`; When enabled,
  `ghc-tags-plugin` streams existing tags when adding the tags found in a new
  module.  Without this option the tags file is read at once into memory.

## 0.3.0.0 -- 2021-05-01

* filter adjacents tags: preserve only type signatures (filter out adjacent
  terms) or data constructors (filter out adjacent type constructors).
* fix emacs support: ghc-tags-plugin can now correctly display multiple tags
  (e.g. instance declarations).  Thanks to @nfrisby for finding out how to do
  that.

## 0.2.4.0 -- 2020-09-08

* `ghc-tags-vim` a vim plugin which helps to maintain a `cabal.project.local` file.
* better tag info

## 0.2.3.0 -- 2020-08-07

* Generate tags for template haskell splices (requires at least `GHC-8.10`).
* Include types of class methods.

## 0.2.0.0 -- 2020-04-12

* Fixed bug [#37][issue-37]
* Added `--debug` flag

## 0.1.6.0 -- 2020-03-24

* support etags files
* various bug fixes
* type level information (not type checked!), from the parsed tree, including:
  type of instances (instance context & instance head), types of `GADTs`
  constructors, rhs of type synonyms, kinds of type or data families.
* expanded ctags pseudo tags with descriptions of fields and tag kinds

## 0.1.5.0 -- 2020-03-13

* concurrency safety - protection `tags` file using a file lock

## 0.1.4.0 -- 2020-03-11

* Tags for default instances of associated (data) type familes.
* Added path argument, can be passed using `-fplugin-opt=Plugin.GhcTags:../tags`.
* Wrapped `IOExceptions`, so when it will happen it will be obvious that the
  plugin failed not `ghc`
* Fixed the tag ordering function to be fullfil the transitivness property.

## 0.1.3.0 -- 2020-03-08

* Change order of tags: type classes, type families and data type families are
  sorted before their instances.  If one is using multipe tags (the default),
  the order of them also matters (i.e. in the vim `tags` option).

## 0.1.2.0 -- 2020-03-05

* Preserve tag information in ctags generated files
* Support `file:` tags (exported / not exported terms)
* Added a test-suite (golden tests and property tests)

## 0.1.1.0 -- 2020-03-03

* Added support for tag's kinds.
* Added various file headers

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

[ghc-issue-20417]: https://gitlab.haskell.org/ghc/ghc/-/issues/20417
[issue-37]: https://github.com/coot/ghc-tags-plugin/issues/37
