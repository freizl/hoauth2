DIST=dist
CBD=stack
STYLE=stylish-haskell
HLINT=hlint

default: build

clean:
	cabal clean
	stack clean

create-keys:
	test -e example/Keys.hs || cp example/Keys.hs.sample example/Keys.hs

build:
	$(CBD) build --test

watch:
	$(CBD) build --test --file-watch

rebuild: clean build

nightly: clean
	$(CBD) --stack-yaml stack-nightly.yaml build --test


stylish:
	$(CBD) exec stylish-haskell --  -i src/Network/OAuth/**/*.hs
	$(CBD) exec stylish-haskell --  -i src/Network/OAuth/*.hs
	$(CBD) exec stylish-haskell --  -i example/*.hs
	$(CBD) exec stylish-haskell --  -i example/*.hs.sample
	$(CBD) exec stylish-haskell --  -i example/**/*.hs

hlint:
	$(CBD) exec hlint -- src/ example --report=$(DIST)/hlint.html

doc: build
	$(CBD) haddock

dist: build
	$(CBD) sdist

####################
### CI
####################

ci-build: create-keys
	$(CBD) +RTS -N2 -RTS build --no-terminal --skip-ghc-check --fast --test

ci-lint: create-keys
	$(CBD) install hlint
	$(CBD) exec hlint -- src example

ci-nightly: create-keys 
	$(CBD) --stack-yaml stack-nightly.yaml +RTS -N2 -RTS build --no-terminal --skip-ghc-check --fast --test

####################
### Tests
####################

test-weibo:
	$(CBD) exec test-weibo

test-douban:
	$(CBD) exec test-douban

test-google:
	$(CBD) exec test-google

test-google-offline:
	$(CBD) exec test-google offline

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

test-okta:
	$(CBD) exec test-okta
