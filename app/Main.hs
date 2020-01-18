module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Lib

parseRow :: String -> Row
parseRow _ = (Red, Red, Red, Red)

doGuess :: StateT (Row, Int) IO ()
doGuess = do
    input <- liftIO getLine
    let guess = parseRow input
    liftIO $ putStrLn $ show guess
    (solution, numTries) <- get
    case (check guess solution) of
        (4, 0) -> do
            liftIO $ putStrLn $ show solution
        _ -> case numTries < 10 of
            True -> do
                modify $ fmap (+1)
                doGuess
            False -> do
                liftIO $ putStrLn $ show solution

main :: IO ()
main = do
    let solution = (Yellow, Red, Green, Blue)
    execStateT doGuess (solution, 0)
    return ()
