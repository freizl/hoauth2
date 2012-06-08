
DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

build: conf
	cabal build
	hlint src

rebuild: clean build

install: build
	cabal install	

doc: build
	cabal haddock
