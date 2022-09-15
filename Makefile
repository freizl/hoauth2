default: build

clean:
	cabal clean

build:
	cabal build all

rebuild: clean build

hlint:
	hlint hoauth2/src hoauth2-demo/src hoauth2-tutorial/src

doc: build
	cabal haddock all

dist: rebuild
	cabal sdist all

## install ghcid globally: `cabal install ghcid`
watch-lib:
	ghcid --command="cabal repl hoauth2" --restart=hoauth2/hoauth2.cabal

####################
### CI - nix build
####################

ci-build:
	nix-build

ci-lint:
	nix-shell --command 'make hlint'
