DIST=dist
CBD=stack
STYLE=stylish-haskell

default: build

clean:
	rm -rf $(DIST)

create-keys:
	test -e example/Keys.hs || cp example/Keys.hs.sample example/Keys.hs

build:
	$(CBD) build

test:
	$(CBD) test

rebuild: clean build


ci-stack: create-keys
	stack build --test

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
	$(CBD) exec test-weibo

test-douban:
	$(CBD) exec test-douban

test-google:
	$(CBD) exec test-google

test-facebook:
	$(CBD) exec test-facebook

test-github:
	$(CBD) exec test-github

test-fitbit:
	$(CBD) exec test-fitbit

test-stackexchange:
	$(CBD) exec test-stackexchange

test-dropbox:
	$(CBD) exec test-dropbox
