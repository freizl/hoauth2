default: build

clean:
	cabal clean

build:
	cabal build -j --run-tests all

build-ide:
	cabal build -j --run-tests all --ghc-options="-fwrite-ide-info"

rebuild: clean build

hlint-only:
	hlint .

hlint:
	hlint .

hlint-fix:
	hlint --refactor --refactor-options="--inplace" .

doc: build
	cabal haddock all

dist: rebuild
	cabal sdist all

format-cabal:
	cabal-fmt -i hoauth2/hoauth2.cabal
	cabal-fmt -i hoauth2-tutorial/hoauth2-tutorial.cabal
	cabal-fmt -i hoauth2-providers/hoauth2-providers.cabal
	cabal-fmt -i hoauth2-providers-tutorial/hoauth2-providers-tutorial.cabal

## install ghcid globally: `cabal install ghcid`
watch-lib:
	ghcid --command="cabal repl hoauth2" --restart=hoauth2/hoauth2.cabal

publish:
	cabal upload $(echo ./dist-newstyle/sdist/*.tar.gz)

####################
### CI - nix build
####################

ci-build:
	nix-build

ci-lint:
	nix-shell --command 'make hlint'

###############################################################################
#                                    HIEDB                                    #
###############################################################################
#
# mk-html:
# 	hiedb -D .hiedb html t:GrantTypeFlow:Network.OAuth2.Experiment.Types:hoauth2-2.5.0-inplace
#
# mk-graph:
# 	hiedb index -D .hiedb
# 	hiedb ref-graph -D .hiedb
# 	dot -Tsvg refs.dot > /tmp/hoauth2.svg
