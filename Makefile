#
# install, uninstall and friends ghc-tags-plugin in cabal store
#

GHC_VERSION ?= 8.10.3
GHC=ghc-${GHC_VERSION}
GHC_PKG=ghc-pkg-${GHC_VERSION}

PACKAGE_DB = ${HOME}/.cabal/store/ghc-${GHC_VERSION}/package.db
# this avoids changing the default environment:
# ~/.ghc/x86_64-linux-8.6.5/environments/default
# file; Unfortunatelly `/dev/null` is not accepted.
#
# THIS FILE WILL BE REMOVED!
ENV=.ghc-tags-plugin.env

uninstall:
	${GHC_PKG} unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  ghc-tags-plugin
	${GHC_PKG} unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  ghc-tags-core

install:
	# avoid changing the default environment
	cabal install --package-db=${PACKAGE_DB} \
	  	      --package-env=${ENV} \
		      --with-compiler=${GHC} \
		      --disable-documentation \
		      --lib \
		      ghc-tags-plugin
	rm ${ENV}
	${GHC_PKG} describe --package-db=${PACKAGE_DB} ghc-tags-plugin | grep -A1 ^id

prof-install:
	# avoid changing the default environment
	cabal install --package-db=${PACKAGE_DB} \
	  	      --package-env=${ENV} \
		      --with-compiler=${GHC} \
		      --lib \
		      --disable-documentation \
		      --enable-profiling \
		      ghc-tags-plugin
	rm ${ENV}
	${GHC_PKG} describe --package-db=${PACKAGE_DB} ghc-tags-plugin | grep -A1 ^id

reinstall: uninstall install

list:
	${GHC_PKG} list --package-db=${PACKAGE_DB} | grep ghc-tags

latest:
	${GHC_PKG} latest --package-db=${PACKAGE_DB} ghc-tags-plugin

recache:
	${GHC_PKG} recache --package-db=${PACKAGE_DB}

check:
	${GHC_PKG} check --package-db=${PACKAGE_DB} 2>&1 | grep ghc-tags

describe:
	${GHC_PKG} describe --package-db=${PACKAGE_DB} ghc-tags-plugin

describe-core:
	${GHC_PKG} describe --package-db=${PACKAGE_DB} ghc-tags-core

.PHONY: install, uninstall, reinstall, latest, check
