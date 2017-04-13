test: install
	haskell-chess 3000 127.0.0.1 8000 white

t2:
	haskell-chess 3000 127.0.0.1 8000 white

build:
	@cabal install

install: build
	@cp dist/build/Haskell-Chess/Haskell-Chess /usr/local/bin/haskell-chess

uninstall:
	rm -f /usr/local/bin/Haskell-Chess

clean:
	@rm -rf dist/