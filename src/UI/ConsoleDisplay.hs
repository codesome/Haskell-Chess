module UI.ConsoleDisplay where 

import System.IO

updateConsole :: Bool -> Bool -> IO ()
updateConsole myMove iAmOnCheck = do
	putStr "\ESC[2J"
	if myMove
		then putStrLn "Its your move now"
		else putStrLn "Waiting for opponent"
	if iAmOnCheck
		then putStrLn "You are on check!"
		else putStr ""
	hFlush stdout

addMessage :: String -> IO ()
addMessage msg = do
	putStrLn msg
	hFlush stdout