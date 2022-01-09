Ghc Tags Compiler Plugin
========================
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-8D82AC.svg)
![MPL-2.0 License](http://img.shields.io/badge/license-MPL20-brightgreen.svg)

[![Haskell CI](https://github.com/coot/ghc-tags-plugin/actions/workflows/ci.yml/badge.svg)](https://github.com/coot/ghc-tags-plugin/actions/workflows/ci.yml)
[![](https://matrix.hackage.haskell.org/api/v2/packages/ghc-tags-plugin/badge)](https://matrix.hackage.haskell.org/#/package/ghc-tags-plugin)

A [Ghc Compiler Plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.


● Requirements
--------------

The plugin requires at least: `ghc >= 8.6`; `ghc-tags-plugin` can generate tags
for template-haskell splices, but it requires: `ghc >= 8.10`.


● Plugin options
----------------

```
Usage: <program> [-e|--etags] [--stream] [--debug] [file_path]
  write tags from ghc abstract syntax tree

Available options:
  -e,--etags               produce emacs etags file
  --stream                 stream existing tags (ctags only)
  file_path                tags file: default tags or TAGS (when --etags is
                           specified)
  --debug                  debugging output
```

It can be an absolute path or relative (to the `*.cabal` package file rather
than `cabal.project` file), for example:
```
-fplugin-opt=Plugin.GhcTags:../tags
```
This is useful if for *cabal packages* which are located in subdirectories.

## ● Emacs support

To produce `etags` file you will need to pass the follwing option
```
-fplugin-opt=Plugin.GhcTags:--etags
```

## ● Editor configuration

By default each generated tags file is put next to the corresponding `*.cabal`
package file.  If you just have a repo with a cabal file in the main directory
`vim` default `tags` setting will work, if you have some modules in
subdirectories you will either need to set:
```
:set tags+=*/tags
```
or pass an option to modify where tags are written, see below.

● Configuration: Ghc / Cabal / Stack
------------------------------------

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

● Exceptions
------------

If a `GHC` plugin throws an exception, `GHC` stops.  This plugin wraps
`IOException`s, to make it obvious that it filed rather than `GHC`.  This
might mean you misconfigured the plugin (by passing wrong options).  The
result might look like this:

```
ghc: panic! (the 'impossible' happened)
  (GHC version 8.6.5 for x86_64-unknown-linux):
        GhcTagsPluginIOException ../: openFile: inappropriate type (Is a directory)

```

● Tips
------

- If you're getting installation problems when running
  `cabal install --lib ghc-tags-plugin`; you may need to

  * remove the installed version from
    `~/.ghc/x86_64-linux-8.6.5/environments/default`
  (or whatever is your default environment)

  * unregister the installeld version from cabal store (you can check what is
  installed in your store with `ghc-pkg --package=PACKAGE_DB list | grep ghc-tags`
  for the following command):

  ```
  ghc-pkg --package-db=PACKAGE_DB unregister z-ghc-tags-plugin-z-ghc-tags-library ghc-tags-plugin
  ```

- The plugin is safe for concurrent compilation, i.e. setting `jobs: $ncpus` is
  safe.  The plugin holds an exclusive (advisory) lock on a lock file.  This
  will create synchronisation between threads / process which are using
  the same `tags` file.

- If you are working on a larger project, it might be better to not collect all
  tags in a single `tags` file, since at every compilation step one will need
  to parse a large `tags` file.  Working with tag files of size 10000 tags (or
  ~1.5MB) is ok - though this will depend on the hardware.

- If you're working on a project that is using `safe-haskell`, you will likely
  need to pass
  [-fplugin-trustworthy](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html?highlight=plugin#ghc-flag--fplugin-trustworthy)
  `ghc` flag.


● Security implications of compiler plugins
-------------------------------------------

Such plugins can:

* run arbitrary `IO`;
* modify abstract syntax tree in some way;  a malicious plugin could change
  some security parameter in your code exposing a security hole.

This plugin only reads & writes to `tags` file (and updates a shared mutable
state) as of `IO`, and does not
[modify/](https://github.com/coot/ghc-tags-plugin/blob/master/src/Plugin/GhcTags.hs#L95)
the syntax tree.
