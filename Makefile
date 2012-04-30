
HC=ghc

DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

build: conf
	cabal build

rebuild: clean build

install : build doc
	cabal install

doc : build
	cabal haddock
