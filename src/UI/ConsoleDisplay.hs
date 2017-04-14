module UI.ConsoleDisplay where 

import System.IO

-- To update with current game state
updateConsole :: Bool -> Bool -> Bool -> IO ()
updateConsole myMove check mate =
    (if myMove
        then putStrLn "\x1b[36mgame>\x1b[37m It's your move now"
        else putStrLn "\x1b[36mgame>\x1b[37m Waiting for opponent") >>=
    (\_ -> if check
        then putStrLn "\x1b[31mgame> You are on check!\x1b[37m"
        else putStr "") >>=
    (\_ -> if mate
        then putStrLn "\x1b[31mgame> Its a mate!\x1b[37m"
        else putStr "") >>=
    (\_ -> hFlush stdout)

-- To add message to console
addMessage :: String -> IO ()
addMessage msg =
    putStrLn ("\x1b[33mgame> " ++ msg ++ "\x1b[37m") >>=
        (\_ -> hFlush stdout)