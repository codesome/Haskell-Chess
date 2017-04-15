# Haskell Chess

## Install
`cabal` is required to install.

```
$ git clone https://github.com/thecodesome/Haskell-Chess.git
$ cd Haskell-Chess/
$ sudo make install
```

### Library dependencies (taken care in install)
* GLUT
* OpenGL
* network
* split

## Start

### Command structure
`haskell-chess {myport} {host} {port} {color}`

Where
* `myport`: port where you want to start the game server.
* `host`: Opponent host id to connect to.
* `port`: Port where the opponent has started `haskell-chess` server
* `color`: `white` or `black` (You are required to mutually decide with the opponent and select the color. Anything other than `white` will be considered as black)

```
$ haskell-chess 3000 127.0.0.1 8000 white
Starting server at port 3000 ...
Press [Enter] when other player is ready
```

> Note: The chat server for the game will be started at port `{myport}+1`. Hence make sure that `{myport}` and `{myport}+1` are not in use before starting the game.

## Docs

* [Game instructions](https://github.com/thecodesome/Haskell-Chess/blob/master/docs/Instructions.md)

* [How to use chat](https://github.com/thecodesome/Haskell-Chess/blob/master/docs/Chat.md)

* [About Code](https://github.com/thecodesome/Haskell-Chess/blob/master/docs/Code.md)

## Uninstall

```
$ sudo make uninstall
```
