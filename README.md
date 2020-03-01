# Ghc Tags Compiler Plugin
A [Ghc Compiler Plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#compiler-plugins)
which generates tags for each compiled module or component.  The source tree is
[left unmodified](https://github.com/coot/ghc-tags-plugin/blob/master/lib/Plugin/GhcTags.hs#L53).

# Usage
```
ghc -plugin-package=ghc-tags-plugin -fplugin=Plugin.GhcTags
```

If you're using `cabal`, at this point you will need to add `ghctags` to every
'build-depends' in every `*.cabal` file.  That's unfortunate state of the
eco-system right now.  You can add `ghc-options` to your `cabal.project.local`
file for each cabal project, e.g. (note that you'll need to update the
`CURRENT_GIT_TAG` below)

```
project someproject
    ghc-options: -fplugin=Plugin.GhcTags


source-repository-package
  type: git
  location: /home/coot/repos/haskell/ghc-tags-plugin
  tag: CURRENT_GIT_TAG
  subdir: .

```

A `tags` file will be created (or destructively updated) in each project
directory (the same as its `cabal` file).
