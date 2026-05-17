module Levels where

import Expression
import Parsing
import System.Random
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

-- Random seed generation
----------------------------

-- This function handles seed generation
makeSeed :: IO StdGen
makeSeed = newStdGen

-- List shuffling
-------------------

-- This function shuffles a list using the Fisher-Yates algorithm
shuffleList :: StdGen -> [a] -> ([a], StdGen)
shuffleList g [] = ([], g)
shuffleList g xs = (x:newtail,g2)
  where (i,g1) = randomR (0, length $ tail xs) g
        (xs1,x:xs2) = splitAt i xs
        (newtail,g2) = shuffleList g1 (xs1++xs2)

-- Builds a random AST structure from a list of numbers
buildRandomTree :: [Int] -> [Op] ->  StdGen -> (Expr, StdGen)
buildRandomTree [n] _ gen = (Val n, gen)
buildRandomTree ns ops gen =
    let (i, g1) = randomR (1, length ns - 1) gen
        (leftNs, rightNs) = splitAt i ns

        (leftTree, g2) = buildRandomTree leftNs ops g1
        (rightTree, g3) = buildRandomTree rightNs ops g2

        (opIdx, g4) = randomR (0, length ops - 1) g3
        op = ops !! opIdx
    in (Calc op leftTree rightTree, g4)

-- Generates a single, guaranteed solvable level
generateValidLevel :: StdGen -> (Level, StdGen)
generateValidLevel gen =
    -- Generate 4 random positive integers from 1-20
    let
        (n1, g1) = randomR (1, 20) gen
        (n2, g2) = randomR (1, 20) g1
        (n3, g3) = randomR (1, 20) g2
        (n4, g4) = randomR (1, 20) g3
        numsList = [n1, n2, n3, n4]

        -- Define standard operators
        opsList = [Add, Sub, Mul, Div]

        -- Shuffle the numbers so the tree builder gets them in a random order
        (shuffledNums, g5) = shuffleList g4 numsList

        -- Build a random tree
        (randomTree, g6) = buildRandomTree shuffledNums opsList g5

    -- Evaluate the tree
    in case eval randomTree of
        Right target | target > 0 -> (Level target numsList opsList, g6) -- it is valid
        _ -> generateValidLevel g6 -- it failed, so loop and try again

-- Generates a list of n playable levels
generateNLevels :: Int -> StdGen -> [Level]
generateNLevels 0 _ = []
generateNLevels n gen =
    let (lvl, nextGen) = generateValidLevel gen
    in lvl : generateNLevels (n - 1) nextGen

-- https://hackage.haskell.org/package/random
-- https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random.html
-- https://stackoverflow.com/questions/16242909/anything-wrong-with-my-fisher-yates-shuffle
-- https://hackage-content.haskell.org/package/random-1.3.1/docs/System-Random.html#g:3