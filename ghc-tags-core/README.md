Create and work with CTAGS and ETAGS files
==========================================
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-8D82AC.svg?style=for-the-badge)
![MPL-2.0 License](http://img.shields.io/badge/license-MPL20-brightgreen.svg?style=for-the-badge)
[![Haskell CI](https://img.shields.io/github/actions/workflow/status/coot/ghc-tags-plugin/ci.yml?branch=master&label=Build&style=for-the-badge)](https://github.com/coot/ghc-tags-plugin/actions/workflows/ci.yml)
[![](https://matrix.hackage.haskell.org/api/v2/packages/ghc-tags-core/badge)](https://matrix.hackage.haskell.org/#/package/ghc-tags-core)

Library scope
-------------

`ghc-tags-core` library provides:

  * a function to extract /tag/ information from @'HsModule' 'GhcPs'@ parsed tree representation of Haskell code,
  * parsers for __ctag__ and __etag__ style tag files (/vim/ \/ /emacs/),
  * formatting tags into __ctag__ and __etag__ files,
  * tries to be compatible with [universal-ctags](https://github.com/universal-ctags/ctags).

Projects using this library
---------------------------
Check out these projects:

  * [ghc-tags] or its [fork][ghc-tags-fork] which is using the most recent
    `ghc-tags-core`;
  * [ghc-tags-plugin] - a ghc [compiler plugin] which extracts tags during
    @GHC@'s parser pass and also from TH splices.

[ghc-tags-fork]: https://github.com/coot/ghc-tags
[ghc-tags]: https://hackage.haskell.org/package/ghc-tags
[ghc-tags-plugin]: https://hackage.haskell.org/package/ghc-tags-plugin
[compiler plugin]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/extending_ghc.html?highlight=compiler%20plugin#compiler-plugins
