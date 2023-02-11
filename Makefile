.PHONY: build
build: clean
	cabal run site build

.PHONY: watch
watch:
	cabal run site watch

.PHONY: clean
clean:
	cabal run site clean
