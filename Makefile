default: build

clean:
	cabal v2-clean

create-keys:
	test -e example/Keys.hs || cp example/Keys.hs.sample example/Keys.hs

build:
	cabal v2-build --flag=test

## install ghcid globally: `cabal install ghcid`
watch:
	ghcid --command="cabal v2-repl ."

watch-demo:
	ghcid --command="cabal v2-repl --flag=test demo-server"

build-demo:
	cabal v2-build --flag=test demo-server

repl-demo:
	cabal v2-repl --flag=test demo-server

start-demo: build-demo
	cabal v2-exec --flag=test demo-server

rebuild: clean build

hlint:
	hlint . --report

doc: build
	cabal v2-haddock

dist: rebuild
	cabal v2-sdist

cabal2nix:
	cabal2nix -ftest . > hoauth2.nix

####################
### CI
####################

ci-build: create-keys
	nix-build

ci-lint:
	nix-shell --command 'make hlint'
