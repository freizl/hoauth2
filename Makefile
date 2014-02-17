
DIST=dist
CBD=cabal
STYLE=stylish-haskell

default: build

init:
	$(CBD) sandbox init
	$(CBD) install --job=2 --only-dependencies --enable-tests

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
	$(STYLE) -i example/*.hs
	$(STYLE) -i example/*.hs.sample
	$(STYLE) -i example/**/*.hs
	hlint src/ example --report=$(DIST)/hlint.html

doc: build
	$(CBD) haddock

dist: build
	$(CBD) sdist

####################
### Tests
####################

test-weibo:
	cd example && sh run.sh Weibo/test.hs

test-github:
	cd example && sh run.sh Github/test.hs

test-google:
	cd example && sh run.sh Google/test.hs

test-fb:
	cd example && sh run.sh Facebook/test.hs
