
default: build

clean:
	cabal v2-clean

create-keys:
	test -e example/Keys.hs || cp example/Keys.hs.sample example/Keys.hs

build:
	cabal v2-build --flag=test

### TODO
watch:
	cabal v2-build --file-watch

build-demo:
	cabal v2-build --flag=test demo-server
start-demo:
	cabal v2-exec --flag=test demo-server

rebuild: clean build

stylish:
	cabal v2-exec stylish-haskell --  -i src/Network/OAuth/**/*.hs
	cabal v2-exec stylish-haskell --  -i src/Network/OAuth/*.hs
	cabal v2-exec stylish-haskell --  -i example/*.hs
	cabal v2-exec stylish-haskell --  -i example/*.hs.sample
	cabal v2-exec stylish-haskell --  -i example/**/*.hs

hlint:
	cabal v2-exec hlint -- src/ example --report=dist-newstyle/hlint.html

doc: build
	cabal v2-haddock

dist: build
	cabal v2-sdist

install-hlint:
	cabal install hlint

####################
### CI
####################

ci-build: build

ci-lint: create-keys install-hlint hlint
