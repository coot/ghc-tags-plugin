Ghc Tags Compiler Plugin
========================
[![Haskell](https://img.shields.io/badge/language-Haskell-8D82AC.svg?style=for-the-badge)](https://haskell.org)
[![MPL-2.0 License](http://img.shields.io/badge/license-MPL20-brightgreen.svg?style=for-the-badge)](https://github.com/coot/ghc-tags-plugin/blob/master/ghc-tags-core/LICENSE)
[![Haskell CI](https://img.shields.io/github/actions/workflow/status/coot/ghc-tags-plugin/ci.yml?branch=master&label=Build&style=for-the-badge)](https://github.com/coot/ghc-tags-plugin/actions/workflows/ci.yml)

A library and a [GHC compiler
plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.


● Requirements
--------------

The plugin requires at least: `ghc >= 9.6`.

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

To produce `etags` file you will need to pass the following option
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

## ● Cabal: quick start with GHCi

To generate tags in a REPL session without installing the plugin separately or
editing any `.cabal` files, run:

```sh
cabal repl TARGET \
  --build-depends=ghc-tags-plugin \
  --repl-options=-fplugin=Plugin.GhcTags \
  --repl-options=-fplugin-opt=Plugin.GhcTags:../tags
```

Replace `TARGET` with a component such as `lib:my-package` or `exe:my-program`.
Cabal builds the plugin and makes it available to that REPL session. This
command was tested with Cabal 3.16.1.0 and GHC 9.10.3 using a local plugin
checkout and this repository's project configuration. See Cabal's
[`--build-depends` documentation](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cmdoption-repl-build-depends).

The plugin's dependencies are solved together with the target project's
dependencies, using that project's `index-state`, constraints, and
`allow-newer` settings.  You might need to modify `allow-newer` via
[`--allow-newer` documentation](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cmdoption-allow-newer).


This configures the REPL session only. For normal builds and HLS, use the
persistent configuration below.

## ● Cabal: persistent configuration

This setup enables the plugin through `cabal.project.local`, without changing
any `.cabal` files.

First, install the plugin into Cabal's store:

```sh
cabal install --lib ghc-tags-plugin --package-env=ghc-tags
```

If installation encounters the GHC 9.14 dependency bounds described above,
add `--allow-newer=pipes-safe:base,ghc-tags-pipes:base` to this command too.

The named `ghc-tags` environment keeps this installation separate from the
default GHC package environment. The configuration below loads the plugin
through its package database, so it does not require selecting that environment.
See Cabal's [package environment documentation](https://cabal.readthedocs.io/en/stable/cabal-commands.html#adding-libraries-to-ghc-package-environments).

Use the same GHC to install the plugin and build the target project. If the
project selects a particular compiler, pass `-w /absolute/path/to/ghc` to the
installation command as well. Reinstall the plugin when changing compilers.

With Cabal 3.16.1.0 or newer, run this in the target project to find its compiler
and the corresponding store directory:

```sh
cabal path --compiler-info
```

Append `/package.db` to the reported `compiler-store-path` to obtain
`PACKAGE_DB`. For example, the resulting path might be:

```text
/home/alice/.cabal/store/ghc-9.10.3-1be3/package.db
```

Use the reported path rather than constructing it from the GHC version:
compiler ABI suffixes and custom store locations can change the directory name.
With older Cabal versions, locate the compiler's `package.db` in the Cabal store
(the store location can be queried with `cabal path --store-dir` where available).

Add the following [`program-options`] stanza to `cabal.project.local`:

```cabal
program-options
    ghc-options: -package-db=PACKAGE_DB
                 -plugin-package=ghc-tags-plugin
                 -fplugin=Plugin.GhcTags
                 -fplugin-opt=Plugin.GhcTags:../tags
```

Replace `PACKAGE_DB` with the absolute path found above. Environment variables
such as `$HOME` are not expanded in this file. If the path contains spaces,
quote the entire argument, for example `"-package-db=C:/Users/My Name/cabal/store/ghc-VERSION/package.db"`.

The stanza applies to all local packages. You can now run `cabal build` or
`cabal repl` normally. When HLS uses a Cabal cradle, restart it after changing
the configuration so it reloads the compiler options.

For a shared tags file, add an option such as:

```text
-fplugin-opt=Plugin.GhcTags:/absolute/path/to/tags
```

Note that the relative path of tags file as in the examples above with
`-fplugin-opt=Plugin.GhcTags:../tags` is relative to the `*.cabal` file, not
`cabal.project` file.

To apply the plugin to dependencies as well, use a `package *` stanza instead
of `program-options`. This can rebuild dependencies and applies the options to
the plugin's own dependencies too; install the plugin before enabling it.

## ● GHC directly

After installing the plugin, pass its package database and enable it:

```sh
ghc -package-db=PACKAGE_DB \
    -plugin-package=ghc-tags-plugin \
    -fplugin=Plugin.GhcTags Main.hs
```

Replace `PACKAGE_DB` as described above. If multiple plugin versions are
installed, select one with `-plugin-package=ghc-tags-plugin-VERSION`, or use
`-plugin-package-id=UNIT_ID` to select a particular installed instance. Inspect
the database with:

```sh
ghc-pkg --package-db=PACKAGE_DB list ghc-tags-plugin
```

See GHC's [package documentation](https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html)
and [compiler plugin documentation](https://downloads.haskell.org/ghc/latest/docs/users_guide/extending_ghc.html#using-compiler-plugins).

## ● Stack

Add `ghc-tags-plugin` to `build-depends` in the relevant components of your
`.cabal` files, optionally behind a development flag. If the plugin is not in
your snapshot, add a release compatible with the snapshot's GHC to `extra-deps`
in `stack.yaml`.

Configure the target package in `stack.yaml` under `ghc-options`:

```yaml
ghc-options:
  my-package: -plugin-package=ghc-tags-plugin
              -fplugin=Plugin.GhcTags
              -fplugin-opt=Plugin.GhcTags:../tags
```

Replace `my-package` with your package name. This Stack setup uses a declared
build dependency; the Cabal setups above avoid changing `.cabal` files.

## ● Ghcid

For automatic tag updates while editing, use the REPL quick start with ghcid:

```sh
ghcid --command="cabal repl TARGET --build-depends=ghc-tags-plugin --repl-options=-fplugin=Plugin.GhcTags --repl-options=-fplugin-opt=Plugin.GhcTags:../tags"
```

If you have already enabled the plugin in `cabal.project.local`, use:

```sh
ghcid --command="cabal repl TARGET"
```

Replace `TARGET` with the component you want ghcid to load.


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

- If GHC cannot find the installed plugin, check that installation and
  compilation use the same compiler and package database:

  ```sh
  ghc-pkg --package-db=PACKAGE_DB list ghc-tags-plugin
  ```

  After changing GHC versions, reinstall the plugin with the new compiler and
  update `PACKAGE_DB` in `cabal.project.local`.

- The plugin is safe for concurrent compilation, i.e. setting `jobs: $ncpus` is
  safe.  The plugin holds an exclusive (advisory) lock on a lock file.  This
  will create synchronisation between threads / process which are using
  the same `tags` file.

- Tag updates are written to a temporary file and renamed over the destination
  only after a successful write. Interrupted compilation (including editor
  cancellation) leaves the previous tags file intact. If tags were lost with an
  older plugin version, rebuild the affected modules with `-fforce-recomp` to
  regenerate them; an incremental build may skip those modules.

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

● ghc-tags - standalone program
-------------------------------

[`ghc-tags-fork`] is a fork of [`ghc-tags-hackage`]. Both provide a standalone
`ghc-tags` command.  Unlike the `hackage` version [ghc-tags-fork] is using the
latest [`ghc-tags-core`] version.

[ghc-issue-20417]: https://gitlab.haskell.org/ghc/ghc/-/issues/20417
[`ghc-tags-fork`]: https://github.com/coot/ghc-tags
[`ghc-tags-hackage`]: https://hackage.haskell.org/package/ghc-tags
[`ghc-tags-core`]: https//hackage.haskell.org/package/ghc-tags-core
[`program-options`]: https://cabal.readthedocs.io/en/stable/cabal-project-description-file.html#program-options
