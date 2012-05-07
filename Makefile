
test-weibo-snap=dist/build/test-weibo-snap/test-weibo-snap

HC=ghc

DIST=dist

default: build

clean:
	rm -rf $(DIST)

conf:
	cabal configure

build: conf
	cabal build

rebuild: clean build

install : build
	cabal install

preview:
	$(test-weibo-snap) -b 127.0.0.1 -p 9988
test-demo:
	cd test && runghc snap.hs -b 127.0.0.1 -p 9988
doc : build
	cabal haddock
