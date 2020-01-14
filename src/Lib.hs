module Lib where

data Color =
    Red | Blue | Green | Pink | Yellow | Turkis
    deriving (Eq, Show)

type Row = (Color, Color, Color, Color)

check :: Row -> Row -> (Int, Int)
check guess answer = undefined
