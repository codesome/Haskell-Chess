module UI.ConsoleDisplay where 

import System.IO

-- To update with current game state
updateConsole :: Bool -> Bool -> Bool -> Bool -> IO ()
updateConsole myMove check lost won =
    (if myMove
        then putStrLn "\x1b[36mgame>\x1b[37m It's your move now"
        else putStrLn "\x1b[36mgame>\x1b[37m Waiting for opponent") >>=
    (\_ -> if check
        then putStrLn "\x1b[31mgame> You are on check!\x1b[37m"
        else putStr "") >>=
    (\_ -> if lost
        then putStrLn "\x1b[31mgame> Its a mate! You lost\x1b[37m"
        else putStr "") >>=
    (\_ -> if won
        then putStrLn "\x1b[32mgame> Its a mate! You Won\x1b[37m"
        else putStr "") >>=
    (\_ -> hFlush stdout)

-- To add message to console
addMessage :: String -> IO ()
addMessage msg =
    putStrLn ("\x1b[33mgame> " ++ msg ++ "\x1b[37m") >>=
        (\_ -> hFlush stdout)