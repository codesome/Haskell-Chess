# Code

### Directory structure

* `docs/`
  * `docs/Instructions.md`
    * Instructions on how to use haskell-chess
  * `docs/Chat.md`
    * About how to use chat in game
  * `docs/Code.md`
    * Code/File structure
* `src/`
  * `src/UI/`
    * `src/UI/Display.hs`
      * Display callbacks of main display loop.
    * `src/UI/Figures.hs`
      * Vertices and settings of all the pieces on board.
    * `src/UI/Bindings.hs`
      * Lefty click, right click, own moves, opponent move (game play)
    * `src/UI/ConsoleDisplay.hs`
      * Game messages displayed on console
  * `src/VerifyMove/`
    * Each file has function to verify the moves of respective pieces.
    * `src/VerifyMove/Bishop.hs`
    * `src/VerifyMove/King.hs`
    * `src/VerifyMove/Knight.hs`
    * `src/VerifyMove/Pawn.hs`
    * `src/VerifyMove/Queen.hs`
    * `src/VerifyMove/Rook.hs`
  * `src/Types.hs`
    * All the data types used across the game.
  * `src/BoardUtils.hs`
    * All the functions needed to access and modify the data types that we defined.
  * `src/Defaults.hs`
    * Initial setting of game for black and white.
  * `src/GameUtils.hs`
    * Logic for check and checkmate
  * `src/MoveUtils.hs`
    * Functions to verfy moves and modify game state and display points according to move.
  * `src/SocketHandlers.hs`
    * Function for chat server and game socket handlers.
  * `src/Main.hs`
    * main function to initiate and start the game.
* `Readme.md`
  * Instruction on how to install haskell-chess.
