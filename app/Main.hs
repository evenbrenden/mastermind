module Main where

import Lib

main :: IO ()
main = do
    let guess = (Red, Red, Yellow, Yellow)
    let answer = (Yellow, Red, Green, Blue)
    putStrLn $ show $ check guess answer
