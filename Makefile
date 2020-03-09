#
# install, uninstall and friends ghc-tags-plugin in cabal store
#

PACKAGE_DB = ${HOME}/.cabal/store/ghc-$(shell ghc --numeric-version)/package.db

uninstall:
	ghc-pkg unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  z-ghc-tags-plugin-z-ghc-tags-library \
	  ghc-tags-plugin

install:
	cabal install --package-db=${PACKAGE_DB} --lib ghc-tags-plugin

reinstall: uninstall install

list:
	ghc-pkg list --package-db=${PACKAGE_DB} | grep ghc-tags

latest:
	ghc-pkg latest --package-db=${PACKAGE_DB} ghc-tags-plugin

recache:
	ghc-pkg recache --package-db=${PACKAGE_DB}

.PHONY: install, uninstall, reinstall, show, latest
