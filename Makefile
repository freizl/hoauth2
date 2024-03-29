default: b

c:
	cabal clean

b:
	cabal build --enable-tests all --ghc-options="-fwrite-ide-info"

rb: c b

l:
	hlint .

hlint-fix:
	hlint --refactor --refactor-options="--inplace" .

doc: b
	cabal haddock all

dist: rb
	cabal sdist all

format-cabal:
	cabal-fmt -i hoauth2/hoauth2.cabal
	cabal-fmt -i hoauth2-tutorial/hoauth2-tutorial.cabal
	cabal-fmt -i hoauth2-providers/hoauth2-providers.cabal
	cabal-fmt -i hoauth2-providers-tutorial/hoauth2-providers-tutorial.cabal
	cabal-fmt -i hoauth2-demo/hoauth2-demo.cabal

## TODO: can run directly from cli but 'make format'
format-hs:
	# no luck either: fourmolu -i "$$(fd -e hs)"
	fourmolu -i $(fd -e hs)

## install ghcid globally: `cabal install ghcid`
watch-lib:
	ghcid --command="cabal repl hoauth2" --restart=hoauth2/hoauth2.cabal

publish: dist
	cabal upload $(echo ./dist-newstyle/sdist/*.tar.gz)

####################
### CI - nix build
####################

ci: rb l
	cabal test all


###############################################################################
#                                    HIEDB                                    #
###############################################################################
clean-hindex:
	rm -f .hiedb

mk-hindex:
	hiedb -D .hiedb index dist-newstyle/
#
# mk-html:
# 	hiedb -D .hiedb html t:GrantTypeFlow:Network.OAuth2.Experiment.Types:hoauth2-2.5.0-inplace
#
# mk-graph:
# 	hiedb index -D .hiedb
# 	hiedb ref-graph -D .hiedb
# 	dot -Tsvg refs.dot > /tmp/hoauth2.svg
