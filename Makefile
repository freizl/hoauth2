
default: build

clean:
	cabal v2-clean

create-keys:
	test -e example/Keys.hs || cp example/Keys.hs.sample example/Keys.hs

build:
	cabal v2-build --flag=test

watch:
	find src example -name '*.hs' | entr -s 'make build'

build-demo:
	cabal v2-build --flag=test demo-server

start-demo:
	cabal v2-exec --flag=test demo-server

rebuild: clean build

stylish:
	find src example -name '*.hs' | xargs stylish-haskell -i

hlint:
	hlint src/ example --report=dist-newstyle/hlint.html

doc: build
	cabal v2-haddock

dist: build
	cabal v2-sdist

## Maybe use hpack?
cabal2nix:
	cabal2nix -ftest . > hoauth2.nix

####################
### CI
####################

ci-build: create-keys
	nix-build

ci-lint:
	nix-shell --command 'make hlint'
