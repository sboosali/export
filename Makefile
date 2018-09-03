##################################################
# Makefile Variables
##################################################

BaseDirectory=$(CURDIR)

DefaultTarget="lib:export"

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `cabal` wrapper targets
##################################################

check:
	cabal new-build -fno-code -O0 all

.PHONY: check

##################################################
build: build-default

.PHONY: build

##################################################
build-default:
	cabal new-build $(DefaultTarget)

.PHONY: build-default

##################################################
repl:
	cabal new-repl $(DefaultTarget)

.PHONY: repl

##################################################
test:
	cabal new-test $(DefaultTarget)

.PHONY: test

##################################################
clean:
	rm -rf "dist/" "dist-newstyle/"
	rm -f *.project.local .ghc.environment.*

.PHONY: clean

##################################################
cabal-compile:
	cabal new-build all

.PHONY: cabal-compile

##################################################
stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
build-examples:
	cabal new-build xmlrpc-examples

.PHONY: build-examples

##################################################
examples: build-examples
	@echo '=================================================='
	cabal new-run xmlrpc-example-time
	@echo '=================================================='
	cabal new-run xmlrpc-example-validator
	@echo '=================================================='
	cabal new-run xmlrpc-example-introspect
	@echo '=================================================='
	cabal new-run xmlrpc-example-simple
	@echo '=================================================='
	cabal new-run xmlrpc-example-person
	@echo '=================================================='

.PHONY: examples

##################################################
watch:
	ghcid -c 'cabal --ghc-option="-fdiagnostics-color=always" new-repl export'

.PHONY: watch

##################################################
sdist: build
	cabal sdist

.PHONY: sdist

##################################################