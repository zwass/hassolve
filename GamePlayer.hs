{-# OPTIONS -Wall #-}

module GamePlayer where

menu :: IO ()
menu = putStr $ unlines
       ["You can use any of these options at any time:",
        "n - Start a new game",
        "? - Show this help message",
        "q - Quit"]

handleInput :: IO Bool
handleInput = do
  putStr ">> "
  input <- getLine
  case input of
    "q" -> do return False
    "n" -> do putStrLn "Starting new game"
              return True
    "?" -> do menu
              return True
    _ -> do putStrLn "Unrecognized option"
            return True

mainLoop :: IO ()
mainLoop = do
  continue <- handleInput
  if continue
    then do mainLoop
    else do return ()

main :: IO ()
main = do
  putStrLn "Welcome to the game player!"
  menu
  mainLoop