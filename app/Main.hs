module Main where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Text.Trifecta
import MmLib

numAllowedTries = 10

doGuess :: StateT (Row, Int) IO ()
doGuess = do
    input <- liftIO getLine
    case parseRow input of
        Success aGuess -> do
            liftIO $ putStrLn $ show aGuess
            (solution, numTries) <- get
            let checked = check aGuess solution
            liftIO $ putStrLn $ show checked
            case checked of
                Result { numRightPositions = 4, numRightColors = 0 } ->
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
    solution <- randomRow
    execStateT doGuess (solution, 1)
    putStrLn $ show solution
    return ()
