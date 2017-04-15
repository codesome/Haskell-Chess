build:
	@cabal install

install: build
	@cp dist/build/Haskell-Chess/Haskell-Chess /usr/local/bin/haskell-chess

uninstall:
	rm -f /usr/local/bin/Haskell-Chess

clean:
	@rm -rf dist/