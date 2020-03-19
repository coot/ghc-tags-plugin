#
# install, uninstall and friends ghc-tags-plugin in cabal store
#

PACKAGE_DB = ${HOME}/.cabal/store/ghc-$(shell ghc --numeric-version)/package.db
# this avoids changing the default environment:
# ~/.ghc/x86_64-linux-8.6.5/environments/default
# file; Unfortunatelly `/dev/null` is not accepted.
#
# THIS FILE WILL BE REMOVED!
ENV=.ghc-tags-plugin.env

uninstall:
	ghc-pkg unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  z-ghc-tags-plugin-z-ghc-tags-library \
	  ghc-tags-plugin

install:
	# avoid changing the default environment
	cabal install --package-db=${PACKAGE_DB} \
	  	      --package-env=${ENV} \
		      --lib \
		      ghc-tags-plugin
	rm ${ENV}

reinstall: uninstall install

list:
	ghc-pkg list --package-db=${PACKAGE_DB} | grep ghc-tags

latest:
	ghc-pkg latest --package-db=${PACKAGE_DB} ghc-tags-plugin

recache:
	ghc-pkg recache --package-db=${PACKAGE_DB}

check:
	ghc-pkg check --package-db=${PACKAGE_DB} 2>&1 | grep ghc-tags

describe:
	ghc-pkg describe --package-db=${PACKAGE_DB} ghc-tags-plugin

.PHONY: install, uninstall, reinstall, latest, check
