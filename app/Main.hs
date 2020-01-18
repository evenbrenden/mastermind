module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative
import Text.Trifecta
import Lib

-- TODO generate random solution
solution = (Turkis, Turkis, Turkis, Turkis)
numAllowedTries = 10

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
    case parseRow input of
        Success guess -> do
            liftIO $ putStrLn $ show guess
            (solution, numTries) <- get
            let checked = check guess solution
            liftIO $ putStrLn $ show checked
            case checked of
                (4, 0) ->
                    return ()
                _ ->
                    if numTries < numAllowedTries then do
                        modify $ fmap (+1)
                        doGuess
                    else
                        return ()
        Failure _ -> do
            doGuess

main = do
    execStateT doGuess (solution, 1)
    putStrLn $ show solution
    return ()
