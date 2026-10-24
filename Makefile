#
# install, uninstall and friends ghc-tags-plugin in cabal store
#

GHC_VERSION ?= $(shell ghc --numeric-version)

GHC_PKG=ghc-pkg-${GHC_VERSION}
GHC=ghc-${GHC_VERSION}

# cabal path --compiler-info requires cabal >= 3.12.  Capture the output once
# and defer a clear error to the point where PACKAGE_DB is actually used, so
# unrelated targets (e.g. help) still work with older cabal.
_COMPILER_STORE := $(shell cabal path --with-compiler="${GHC}" --compiler-info 2>/dev/null | sed -n 's/^compiler-store-path: //p')
ifeq ($(_COMPILER_STORE),)
PACKAGE_DB = $(error 'cabal path --compiler-info' failed -- cabal >= 3.12 is required. Upgrade with: cabal install cabal-install)
else
PACKAGE_DB := $(_COMPILER_STORE)/package.db
endif

# this avoids changing the default environment:
# ~/.ghc/x86_64-linux-8.6.5/environments/default
# file; Unfortunatelly `/dev/null` is not accepted.
#
# THIS FILE WILL BE REMOVED!
ENV=.ghc-tags-plugin.env

install:
	# avoid changing the default environment
	cabal install --package-db=${PACKAGE_DB} \
	  	      --package-env=${ENV} \
		      --with-compiler=${GHC} \
		      --disable-documentation \
		      --reinstall \
		      --lib \
	              ghc-tags-plugin
	rm ${ENV}
	${GHC_PKG} describe --package-db=${PACKAGE_DB} ghc-tags-plugin | grep -A1 ^id

# Install for every native GHC version installed with GHCup.
install-all:
	@set -eu; \
	installed=$$(ghcup --offline list --tool ghc --show-criteria installed --raw-format); \
	versions=$$(printf '%s\n' "$$installed" | awk '$$1 == "ghc" && $$2 !~ /javascript|wasm32/ { print $$2 }'); \
	if [ -z "$$versions" ]; then \
	    echo "No GHC versions are installed with ghcup." >&2; \
	    exit 1; \
	fi; \
	for version in $$versions; do \
	    printf 'Installing ghc-tags-plugin for GHC %s\n' "$$version"; \
	    $(MAKE) install GHC_VERSION="$$version"; \
	done

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

uninstall:
	${GHC_PKG} unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  ghc-tags-plugin

uninstall-core:
	${GHC_PKG} unregister \
	  --package-db=${PACKAGE_DB} \
	  --force \
	  ghc-tags-core

# reinstall ghc-tags-core and ghc-tags-plugin
reinstall-core: uninstall uninstall-core install

# reinstall only ghc-tags-plugin
reinstall: uninstall install

gtp-check:
	cabal install -f+gtp-check -w ${GHC} ghc-tags-plugin:gtp-check

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

.PHONY: install install-all prof-install uninstall uninstall-core reinstall reinstall-core \
        list latest recache check describe describe-core
