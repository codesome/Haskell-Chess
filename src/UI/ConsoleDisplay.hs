module UI.ConsoleDisplay where 

import System.IO

updateConsole :: Bool -> Bool -> IO ()
updateConsole myMove iAmOnCheck = do
    if myMove
        then putStrLn "\x1b[36mgame>\x1b[37m It's your move now"
        else putStrLn "\x1b[36mgame>\x1b[37m Waiting for opponent"
    if iAmOnCheck
        then putStrLn "\x1b[31m> You are on check!\x1b[37m"
        else putStr ""
    hFlush stdout

addMessage :: String -> IO ()
addMessage msg = do
    putStrLn ("\x1b[33m> " ++ msg ++ "\x1b[37m")
    hFlush stdout