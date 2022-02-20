default: build

clean:
	cabal v2-clean

build:
	cabal v2-build all

## install ghcid globally: `cabal install ghcid`
watch:
	ghcid --command="cabal v2-repl ."

watch-demo:
	ghcid --command="cabal v2-repl demo-server"

repl-demo:
	cabal v2-repl demo-server

start-demo:
	cabal v2-run demo-server

rebuild: clean build

hlint:
	hlint hoauth2/src hoauth2-example/src

doc: build
	cabal v2-haddock

dist: rebuild
	cabal v2-sdist

####################
### CI - nix build
####################

cabal2nix:
	cabal2nix -ftest . > hoauth2.nix

ci-build:
	nix-build

ci-lint:
	nix-shell --command 'make hlint'
