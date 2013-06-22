
DIST=dist
CBD=cabal-dev
STYLE=stylish-haskell

default: build

init:
	cabal update
	$(CBD) install
clean:
	rm -rf $(DIST)

conf:
	$(CBD) configure

build: conf
	$(CBD) build

rebuild: clean build

install: build
	$(CBD) install	

hlint:
	$(STYLE) -i src/Network/OAuth/**/*.hs
	$(STYLE) -i src/Network/OAuth/*.hs
	hlint src/ --report=$(DIST)/hlint.html

doc: build
	$(CBD) haddock

dist: build
	$(CBD) sdist

test-weibo:
	cd example && sh run.sh Weibo/test.hs

test-github:
	cd example && sh run.sh Github/test.hs
