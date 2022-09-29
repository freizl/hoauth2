default: build

clean:
	cabal clean

build:
	cabal build -j --run-tests all

rebuild: clean build

hlint:
	hlint hoauth2/src hoauth2-demo/src hoauth2-tutorial/src hoauth2-providers/src hoauth2-providers-tutorial/src

doc: build
	cabal haddock all

dist: rebuild
	cabal sdist all

format-cabal:
	cabal-fmt -i hoauth2/hoauth2.cabal
	cabal-fmt -i hoauth2-tutorial/hoauth2-tutorial.cabal
	cabal-fmt -i hoauth2-providers/hoauth2-providers.cabal
	cabal-fmt -i hoauth2-providers-tutorial/hoauth2-providers-tutorial.cabal
	cabal-fmt -i hoauth2-demo/hoauth2-demo.cabal

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
