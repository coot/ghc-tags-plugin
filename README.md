# Ghc Tags Compiler Plugin
A [Ghc Compiler Plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.

# Usage

## ghc

```
ghc -plugin-package=ghc-tags-plugin -fplugin=Plugin.GhcTags
```

## cabal

Install the `ghc-tags-plugin` to cabal store with:
```
cabal install ghc-tags-plugin
```

In `cabal.project.local` file add `package` stanza for every local package :
```
project someproject
    ghc-options: -package-db=PACKAGE_DB -plugin-package=ghc-tags-plugin-0.1.0.1 -fplugin=Plugin.GhcTags
```

`PACKAGE_DB` is likely something to be something like (this example is for
`ghc-8.6.5`) '${HOME}/.cabal/store/ghc-8.6.5/package.db' (it must be provided
with with expanded environment variables)

A `tags` file will be created (or destructively updated) in each project
directory (the same as its `cabal` file).

## stack

Install `ghc-tags-plugin` 

```
stack install ghc-tags-plugin
```

In `stack.yaml` file add:
```
ghc-options:
    some-package: -package-db=PACKAGE_DB -plugin-package=ghc-tags-plugin-0.1.0.1 -fplugin=Plugin.GhcTags
```

where `PACKAGE_DB` is the package db where `ghc-tags-plugin` was installed by
`stack`.

## modifying `cabal` files

You can always add `ghc-tags-plugin` as a build dependency in a cabal file (for
each component).  You can hide it behind a flag and then use `cabal` or `stack`
to enable it (or `cabal.project.local` or `stack.yaml` files for that purpose).

# Security implications of compiler plugins

Such plugins can

* run arbitrary `IO`
* modify abstract syntax tree in some way.  A malicious plugin could change
  some security parameter in your code exposing a security hole.

This plugin only reads & writes to `tags` file (and updates a shared mutable
state), and does not
[modify/](https://github.com/coot/ghc-tags-plugin/blob/master/lib/Plugin/GhcTags.hs#L79)
the syntax tree.


