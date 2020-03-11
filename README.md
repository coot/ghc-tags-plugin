# Ghc Tags Compiler Plugin
![](https://github.com/coot/ghc-tags-plugin/workflows/GHC-8.8.3/badge.svg)
![](https://github.com/coot/ghc-tags-plugin/workflows/GHC-8.6.5/badge.svg)

A [Ghc Compiler Plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.


# ● Requirements

```
ghc >= 8.6
```

# ● Vim configuration

By default each generated tags file is put next to the corresponding `*.cabal`
package file.  If you just have a repo with a cabal file in the main directory
`vim` default `tags` setting will work, if you have some modules in
subdirectories you will either need to set:
```
:set tags+=*/tags
```
or pass an option to modify where tags are written, see below.


# ● Plugin options

The plugin accepts an only one option, which is a file path to the tags file.
It can be an absolute path or relative (to the `*.cabal` package file rather
than `cabal.project` file), for example:
```
-fplugin-opt=Plugin.GhcTags:../tags
```
This is useful if for *cabal packages* which are located in subdirectories.


# ● Configuration: Ghc / Cabal / Stack

Configuration of this plugin requires some familiarity with `ghc` packages.
Check out
[documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html#packages)
to use `-plugin-package` or `-plugin-package-id`.  In the examples below we
use `-plugin-package=ghc-tags-plugin` but specifying version
`-package=ghc-tags-plugin-0.0.0.0` (where `0.0.0.0` is the version you
installed), might work better.  You can use `ghc-pkg latest ghc-tags-plugin`
(likely with appropriate `--package-db` flag) to check which version is
available.

## ● Ghc

```
ghc -plugin-package=ghc-tags-plugin -fplugin=Plugin.GhcTags
```

You might also need to pass `-package-db` in which you installed the plugin.

## ● Cabal

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
(all environment variables must be expanded):
```
${HOME}/.cabal/store/ghc-8.6.5/package.db
```
or on Windows (note the `""` syntax)
```
"C:\\Users\\USER_NAME\\AppData\\Roaming\\cabal\\store\\ghc-8.6.5\\package.db
```

Note that you can also configure in this way non-local projects.  You will
likely want to pass `-fplugin-opt=Plugin.GhcTags=PATH` where `PATH` is *an
absolute* path to your `tags` file.


## ● Stack

This is alternative method, which also could be modified for `cabal` (but it is
not as nice as the previous method where you don't need to modify any files
checked in vcs).

Add `ghc-tags-plugin` to  `build-depends` in your `*.cabal` files. (You should
hide it behind a cabal flag).  And add these lines to `stack.yaml` file:

```
extra-deps:
  - git: https://github.com/coot/ghc-tags-plugin
    commit: a841dae7fb9c335101f7fa4187d02687d306f972

test-project: -plugin-package=ghc-tags-plugin
              -fplugin=Plugin.GhcTags
```

Check out an example
[here](https://github.com/coot/ghc-tags-plugin/tree/stack-setup/test-project).


## ● Ghcid

If you follow the cabal configuration as above (using `stack` should work too)
```
ghcid --comaand "cabal repl project"
```
will update `tags` file as you modify your project.


## ● Makefile

The [Makefile](https://github.com/coot/ghc-tags-plugin/blob/master/Makefile)
contains some useful commands, e.g. `install`,  `uninstall` or `reinstall` the
package in a `package.db` (by default into `cabal` store).  This is mostly for
development, but it could be useful in other scenarios as well.

# ● Exceptions

If a `GHC` plugin throws an exception, `GHC` stops.  This plugin wraps
`IOException`s, to make it obvious that it filed rather than `GHC`.  This
might mean you misconfigured the plugin (by passing wrong options).  The
result might look like this:

```
ghc: panic! (the 'impossible' happened)
  (GHC version 8.6.5 for x86_64-unknown-linux):
        GhcTagsPluginIOException ../: openFile: inappropriate type (Is a directory)

```


# ● Security implications of compiler plugins

Such plugins can:

* run arbitrary `IO`;
* modify abstract syntax tree in some way;  a malicious plugin could change
  some security parameter in your code exposing a security hole.

This plugin only reads & writes to `tags` file (and updates a shared mutable
state) as of `IO`, and does not
[modify/](https://github.com/coot/ghc-tags-plugin/blob/master/src/Plugin/GhcTags.hs#L95)
the syntax tree.
