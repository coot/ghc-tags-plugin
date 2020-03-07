# Ghc Tags Compiler Plugin ![](https://github.com/coot/ghc-tags-plugin/workflows/GhcTagsPlugin%20CI%20Workflow/badge.svg)

A [Ghc Compiler Plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.

# Requirements

```
ghc >= 8.6
```

# vim configuration

Each generated tags file is put next to the corresponding `*.cabal` file.  If
you just have a repo with a cabal file in the main directory `vim` default
`tags` setting will work, if you have some modules in subdirectories you will
need to set:
```
:set tags+=*/tags
```

# Plugin usage

Configuration of this plugin requires some familiarity with `ghc` packages.
Check out
[documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#packages)
how to use `-plugin-package` or `-plugin-package-id`.  In the examples below we
us `-plugin-package=ghc-tags-plugin` but specifying version
`-package=ghc-tags-plugin-0.0.0.0` (where `0.0.0.0` is the version you
installed), might work better.  You can use `ghc-pkg latest`  (likely with
appropriate `--package-db` flag) to check which version is available.

## ghc

```
ghc -plugin-package=ghc-tags-plugin -fplugin=Plugin.GhcTags
```

## cabal

Install the `ghc-tags-plugin` to cabal store with:
```
cabal install --lib ghc-tags-plugin
```

In `cabal.project.local` file add `package` stanza for every local package :
```
project some-project
    ghc-options: -package-db=PACKAGE_DB
                 -plugin-package=ghc-tags-plugin
                 -fplugin=Plugin.GhcTags
```

`PACKAGE_DB` is likely to be something like (for `ghc-8.6.5`)
'${HOME}/.cabal/store/ghc-8.6.5/package.db' (all environment variables must be
expanded).

## stack

Install `ghc-tags-plugin` 

```
stack install ghc-tags-plugin
```

In `stack.yaml` file add:
```
ghc-options:
    some-project: -package-db=PACKAGE_DB
                  -plugin-package=ghc-tags-plugin
                  -fplugin=Plugin.GhcTags
```

where `PACKAGE_DB` is the package db where `ghc-tags-plugin` was installed by
`stack`.

## modifying `cabal` files

You can always add `ghc-tags-plugin` as a build dependency in a cabal file (for
each component).  You should hide it behind a flag and then use `cabal` or `stack`
to enable it (or `cabal.project.local` or `stack.yaml` files for that purpose).

# Security implications of compiler plugins

Such plugins can:

* run arbitrary `IO`;
* modify abstract syntax tree in some way;  a malicious plugin could change
  some security parameter in your code exposing a security hole.

This plugin only reads & writes to `tags` file (and updates a shared mutable
state) as of `IO`, and does not
[modify/](https://github.com/coot/ghc-tags-plugin/blob/master/lib/Plugin/GhcTags.hs#L79)
the syntax tree.
