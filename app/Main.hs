module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Applicative
import Text.Trifecta as T
import MmLib

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

parseRow :: String -> T.Result Row
parseRow = parseString row mempty

doGuess :: StateT (Row, Int) IO ()
doGuess = do
    input <- liftIO getLine
    case parseRow input of
        Success aGuess -> do -- Parse success
            liftIO $ putStrLn $ show aGuess
            (solution, numTries) <- get
            let checked = check aGuess solution
            liftIO $ putStrLn $ show checked
            if checked == Result { numRightPositions = 4, numRightColors = 0 } then -- Right answer
                return ()
            else
                if numTries < numAllowedTries then do -- Wrong answer, more tries left
                    modify $ fmap (+1)
                    doGuess
                else -- Wrong answer, no tries left
                    return ()
        Failure _ -> do -- Parse failure
            liftIO $ putStrLn $ "Invalid input"
            doGuess

main = do
    solution <- randomRow
    execStateT doGuess (solution, 1)
    putStrLn $ show solution
    return ()
