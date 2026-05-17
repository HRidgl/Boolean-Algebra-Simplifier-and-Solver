module Levels where

import Expression
import Parsing
import Data.List

-- Level record
data Level = Level {
    target :: Int,
    nums :: [Int],
    ops :: [Op]
} deriving (Show, Eq)


-- Level Parsing 
-------------------

-- Safely parse each line in the level text file, ignoring extra spaces
levelParser :: Parser Level
levelParser = do
    symbol "target="
    t <- natInt

    symbol "nums="
    ns <- sepByComma natInt

    symbol "ops="
    os <- sepByComma opParser

    return (Level t ns os)

-- Level loading 
-------------------

-- Loads levels by returning an array of levels for each line in the file
loadLevel :: String -> Maybe Level
loadLevel line =
    case parse levelParser line of
        [(lvl, "")] -> Just lvl
        _           -> Nothing

-- https://hackage.haskell.org/package/random
-- https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random.html
-- https://stackoverflow.com/questions/16242909/anything-wrong-with-my-fisher-yates-shuffle
-- https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random.html#g:3