build:
	cabal install

play:
	./dist/build/Haskell-Chess/Haskell-Chess
	
build_play: build play

install: build
	cp dist/build/Haskell-Chess/Haskell-Chess /usr/local/bin/haskell-chess

uninstall:
	rm -f /usr/local/bin/haskell-chess

clean:
	rm -rf dist/