module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative
import Text.Trifecta
import Lib

color :: Parser Color
color = do
    color' <- oneOf "RBGPYT"
    case color' of
        'R' -> return Red
        'B' -> return Blue
        'G' -> return Green
        'P' -> return Pink
        'Y' -> return Yellow
        'T' -> return Turkis
        _ -> empty

row :: Parser Row
row = do
    a <- color
    b <- color
    c <- color
    d <- color
    return (a, b, c, d)

parseRow = parseString row mempty

doGuess :: StateT (Row, Int) IO ()
doGuess = do
    input <- liftIO getLine
    let parsed = parseRow input
    case parsed of
        Success guess -> do
            liftIO $ putStrLn $ show guess
            (solution, numTries) <- get
            let checked = check guess solution
            liftIO $ putStrLn $ show checked
            case checked of
                (4, 0) -> do
                    liftIO $ putStrLn $ show solution
                _ -> case numTries < 10 of
                    True -> do
                        modify $ fmap (+1)
                        doGuess
                    False -> do
                        liftIO $ putStrLn $ show solution
        Failure _ -> do
            doGuess

main :: IO ()
main = do
    let solution = (Yellow, Red, Green, Blue)
    execStateT doGuess (solution, 0)
    return ()
