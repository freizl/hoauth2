default: build

clean:
	cabal v2-clean

build:
	cabal v2-build

## install ghcid globally: `cabal install ghcid`
watch:
	ghcid --command="cabal v2-repl ."

watch-demo:
	ghcid --command="cabal v2-repl demo-server"

build-demo:
	cabal v2-build demo-server

repl-demo:
	cabal v2-repl demo-server

start-demo: build-demo
	cabal v2-exec demo-server

rebuild: clean build

hlint:
	hlint . --report

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
