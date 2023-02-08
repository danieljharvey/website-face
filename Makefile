.PHONY: build
build:
	cabal update
	cabal run site build

.PHONY: watch
watch:
	cabal update
	cabal run site watch
