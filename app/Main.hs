module Main where

import Control.Applicative ( (<|>), Alternative(empty) )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.State ( execStateT, get, modify, StateT )
import Text.Trifecta as T ( char, oneOf, parseString, Parser, Result(..) )
import MmLib

numAllowedTries :: Int
numAllowedTries = 10

red :: Parser Color
red = char 'R' >> return Red

blue :: Parser Color
blue = char 'B' >> return Blue

green :: Parser Color
green = char 'G' >> return Green

pink :: Parser Color
pink = char 'P' >> return Pink

yellow :: Parser Color
yellow = char 'Y' >> return Yellow

turkis :: Parser Color
turkis = char 'T' >> return Turkis

color :: Parser Color
color =
    red <|> blue <|> green <|> pink <|> yellow <|> turkis

row :: Parser Row
row = do
    a <- color
    b <- color
    c <- color
    d <- color
    return $ Row (a, b, c, d)

parseRow :: String -> T.Result Row
parseRow = parseString row mempty

doGuess :: StateT (Row, Int) IO ()
doGuess = do
    input <- liftIO getLine
    case parseRow input of
        Success aGuess -> do
            liftIO $ print aGuess
            (solution, numTries) <- get
            let checked = check aGuess solution
            liftIO $ print checked
            let isWrongAnswer = checked /= rightAnswer
            let hasMoreTries = numTries < numAllowedTries
            when (isWrongAnswer && hasMoreTries) $ do
                modify $ fmap (+1)
                doGuess
        Failure _ -> do
            liftIO $ print "Invalid input"
            doGuess

main :: IO ()
main = do
    putStrLn "Take a guess!"
    solution <- randomRow
    execStateT doGuess (solution, 1)
    print solution
    return ()
