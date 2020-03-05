#
# install, uninstall and friends ghc-tags-plugin in cabal store
#

CABAL_STORE = ${HOME}/.cabal/store/ghc-$(shell ghc --numeric-version)/package.db

uninstall:
	ghc-pkg unregister \
	  --package-db=${CABAL_STORE} \
	  --force \
	  z-ghc-tags-plugin-z-ghc-tags-library \
	  ghc-tags-plugin

install:
	cabal install --package-db=${CABAL_STORE} --lib ghc-tags-plugin

reinstall: uninstall install

list:
	ghc-pkg list --package-db=${CABAL_STORE} | grep ghc-tags

latest:
	ghc-pkg latest --package-db=${CABAL_STORE} ghc-tags-plugin

recache:
	ghc-pkg recache --package-db=${CABAL_STORE}

.PHONY: install, uninstall, reinstall, show, latest
