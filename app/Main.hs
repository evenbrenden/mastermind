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

doGuess :: Row -> StateT Int IO ()
doGuess solution = do
    input <- liftIO getLine
    -- TODO handle parser failure...differently
    case parseRow input of
        Success guess -> do
            liftIO $ putStrLn $ show guess
            let checked = check guess solution
            liftIO $ putStrLn $ show checked
            case checked of
                (4, 0) ->
                    return ()
                _ -> do
                    numTries <- get
                    if numTries < numAllowedTries then do
                        modify (+1)
                        doGuess solution
                    else
                        return ()
        Failure _ -> do
            doGuess solution

main = do
    execStateT (doGuess solution) 1
    putStrLn $ show solution
    return ()
