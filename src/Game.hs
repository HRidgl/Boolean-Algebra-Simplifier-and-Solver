-- this will run the loop and validate inputs

module Game where

-- Other module imports
import Expression
import Levels
import Parsing

-- Library imports
import Data.List
import Data.Maybe (mapMaybe)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.IO (hFlush, stdout)

-- -------------------------
-- -- Game state and REPL --
-- -------------------------

-- Parses the user's mathematical expression, validates it, evaluates it, updates game state
compareResult :: String -> [(String, String, String)] ->IO ()
compareResult ui pastAttempts = do

  -- Attempt to parse string
  case cleanExpressionParser ui of
    -- Some sort of parsing error
    Left parseErrorMsg -> do
      putStrLn parseErrorMsg
      enterExpr pastAttempts

    -- Parsed successfully, now attempt to evaluate
    Right parsedTree -> do
      let normTree = normalise parsedTree
      putStrLn $ "Parsed and normalised as: " ++ prettyPrint normTree

      -- let usedNums = extractNums parsedTree
      -- let allowedLiterals = ['A'..'Z']

      -- let usedOps = extractOps parsedTree
      -- let allowedOps = [AND, OR, NOT]

      -- -- Error accumulation
      -- let numErrors =
      --       (["You can only use numbers in this list, as many times as they appear: " ++ show allowedNums | not (null (usedNums \\ allowedLiterals))])

      -- let opErrors =
      --       (["You can only use operators in this list: " ++ show allowedOps | any (`notElem` allowedOps) usedOps])

      -- let validationErrors = numErrors ++ opErrors

      let validationErrors = []

      if not (null validationErrors)
        then do
          let newAttempts = pastAttempts ++ map (\e -> (ui, "ERROR - Invalid input", e)) validationErrors
          displayAllErrors validationErrors
          enterExpr pastAttempts
        else do
          -- only evaluate if the valdiation passed
          case eval parsedTree of
            -- Some math rule was broken
            Left mathError -> do
              let e3 = "Math error: " ++ show mathError
              let newAttempts = pastAttempts ++ [(ui, "ERROR - Math error", e3)]
              displayAllErrors [e3]
              enterExpr pastAttempts

            -- Math is correct, now check if it is the right answer
            Right finalAnswer -> do
              putStrLn "Success"
              putStrLn "Type :new to enter a new expression."

              -- add to list of attempts
              let successMsg = "Evaluated to expression: " ++ show finalAnswer
              let newAttempts = pastAttempts ++ [(ui, "SUCCESS", successMsg)]

              enterExpr newAttempts

                  

-- Prompts the user for their next move and captures it
enterExpr :: [(String, String, String)] -> IO ()
enterExpr pastAttempts = do
  putStrLn "Enter your boolean expression"
  putStr "> "
  hFlush stdout
  ui <- getLine
  checkInput ui pastAttempts

-- Display level pack information when a pack is chosen
printDetails :: String -> IO ()
printDetails mode = do
  putStrLn ""
  putStrLn "Simplifying boolean expression mode"
  putStrLn "   Type :h for commands   "
  enterExpr []


-- -- Pretty print level details
-- printLevelDetails :: String -> [Level] -> Int -> Int -> Level -> IO ()
-- printLevelDetails packName levels idx levelNum lvl = do
--   putStrLn "--------------------------"
--   putStrLn $ "Level " ++ show levelNum ++ ". Target: " ++ show (target lvl)
--   putStrLn $ "Available numbers: " ++ show (nums lvl)
--   putStrLn $ "Allowed operators: " ++ show (ops lvl)
--   if packName == "Random Pack" then estimateDifficulty levels idx else return ()

-- Entry point for the game
mainLoop :: IO ()
mainLoop = do
  putStrLn "--------------------------"
  putStrLn "          WELCOME         "
  putStrLn "--------------------------"
  putStrLn "Select an option:"
  putStrLn "1. Simplify a boolean algebra expression"
  putStrLn "2. Solve boolean algebra problem"
  putStr "> "
  hFlush stdout
  choice <- getLine

  let option = case choice of
        "1" -> "expression simplifier"
        "2" -> "medium.txt"
        "3" -> "hard.txt"
        "4" -> "random.txt"
        _ -> ""

  if option == ""
    then do
      putStrLn "Invalid choice"
      mainLoop
    else do
      printDetails "expression simplifier"
      enterExpr []
      putStrLn "--------------------------"

-- -- This function lists out the commands to the user
-- help :: String -> [Level] -> Int -> Int -> [(String, String, String)] -> IO ()
-- help packName levels idx currentScore pastAttempts =
--   do
--     putStrLn "------------------------------------------"
--     putStrLn "                COMMANDS                  "
--     putStrLn "------------------------------------------"
--     putStrLn ":\ESC[1mh\ESC[0melp                         for commands"
--     putStrLn ":\ESC[1mq\ESC[0muit                              to quit"
--     putStrLn ":\ESC[1mhi\ESC[0mnt                        to get a hint"
--     putStrLn ":\ESC[1ma\ESC[0mi             to get ai to solve problem"
--     putStrLn ":\ESC[1mpa\ESC[0mst   to see your attempts on this level"
--     putStrLn ":\ESC[1mn\ESC[0mew          to proceed to the next level"
--     putStrLn ":\ESC[1ml\ESC[0mevel          to choose a specific level"
--     putStrLn ":\ESC[1ms\ESC[0mhow to reprint the current level details"
--     putStrLn ":\ESC[1mp\ESC[0mack           to choose a different pack"
--     putStrLn "------------------------------------------"
--     enterExpr packName levels idx currentScore pastAttempts

-- Handling of user commands
checkInput :: String -> [(String, String, String)] -> IO ()
checkInput ui pastAttempts
  | ui == ":q" || ui == ":quit" = do
    putStrLn "Thanks for playing!"

  -- -- show commands list
  -- | ui == ":h" || ui == ":help" = help packName levels idx currentScore pastAttempts

  -- -- Go to next level and print level details
  -- | ui == ":n" || ui == ":new" = do
  --     let nextIdx = (idx + 1) `mod` length levels
  --     let nextLevel = levels !! nextIdx
  --     printLevelDetails packName levels nextIdx (nextIdx + 1) nextLevel
  --     enterExpr packName levels nextIdx currentScore []

  -- -- Go to specific level and print level details
  -- | ui == ":l" || ui == ":level" = do
  --     putStrLn $ "Enter level number (1-" ++ show (length levels) ++ "): "
  --     putStr "> "
  --     hFlush stdout
  --     lvlStr <- getLine

  --     case reads lvlStr :: [(Int, String)] of
  --       [(n, "")] ->
  --         if n >= 1 && n <= length levels
  --           then do
  --             let nextIdx = n - 1
  --             let nextLevel = levels !! nextIdx
  --             printLevelDetails packName levels nextIdx n nextLevel
  --             enterExpr packName levels nextIdx currentScore []
  --           else do
  --             putStrLn "Unable to find level, returning to current level"
  --             enterExpr packName levels idx currentScore pastAttempts
  --       _ -> do
  --         putStrLn "Invalid input. Please enter a valid number."
  --         enterExpr packName levels idx currentScore pastAttempts

  -- -- Reprint the current level details concisely
  -- | ui == ":s" || ui == ":show" = do
  --     let currentLevel = levels !! idx
  --     putStrLn $ "Target: " ++ show (target currentLevel) ++ " | Available Nums: " ++ show (nums currentLevel) ++ " | Allowed Operators: " ++ show (ops currentLevel)
  --     enterExpr packName levels idx currentScore pastAttempts

  -- -- Back to main menu to choose a different pack
  -- | ui == ":p" || ui == ":pack" = mainLoop

  -- -- Output a hint to the user, showing them the first 2 characters in the expression
  -- | ui == ":hint" || ui == ":hi" = do
  --     let currentLevel = levels !! idx
  --     getPossibleExpressions (target currentLevel) (nums currentLevel) (ops currentLevel)
  --     enterExpr packName levels idx currentScore pastAttempts

  -- -- Output a solution to the problem using a brute force search
  -- | ui == ":ai" || ui == ":a" = do
  --     let currentLevel = levels !! idx
  --     let solution = solver (target currentLevel) (nums currentLevel) (ops currentLevel)
  --     if solution == "No solution"
  --       then putStrLn "There is no solution to this problem"
  --       else do
  --         putStrLn "The AI solution is "
  --         putStrLn solution
  --     enterExpr packName levels idx currentScore pastAttempts

  -- | ui == ":pa" || ui == ":past" = do
  --     displayPastAttempts pastAttempts
  --     enterExpr packName levels idx currentScore pastAttempts

  | otherwise = compareResult ui pastAttempts

-- -------------------------------------------
-- -- Abstract Syntax Tree helper functions --
-- -------------------------------------------

-- -- Helper function, walks through the tree and extracts a list of integers the player used
-- extractNums :: Expr -> [Int]
-- extractNums (Val n) = [n]
-- extractNums (Calc _ e1 e2) = extractNums e1 ++ extractNums e2

-- -- Helper function, to extract the list of operators the player used
-- extractOps :: Expr -> [Op]
-- extractOps (Val _) = []
-- extractOps (Calc op e1 e2) = op : (extractOps e1 ++ extractOps e2)

-- -------------------------------
-- -- Hints and AI solver logic --
-- -------------------------------

-- -- Uses the AI solver to lazily find a valid solution tree, then extracts and prints a single base level step as a hint
-- getPossibleExpressions :: Int -> [Int] -> [Op] -> IO ()
-- getPossibleExpressions target nums ops = do
--   let exprs = allValidExprs nums ops
--   case find (\e -> eval e == Right target) exprs of
--     Just validExpr -> do
--         case findBaseCalc validExpr of
--             Just baseStep -> putStrLn $ "Try starting with: " ++ prettyPrint baseStep
--             Nothing       -> putStrLn "The simplest solution is just a single number!"
--     Nothing -> putStrLn "No solution - insolvable problem"

-- -- Recursively walks down a valid expression tree to locate a base calculation for a hint
-- findBaseCalc :: Expr -> Maybe Expr
-- findBaseCalc (Val _) = Nothing
-- findBaseCalc (Calc op (Val left) (Val right)) = Just (Calc op (Val left) (Val right))
-- findBaseCalc (Calc _ l r) =
--     case findBaseCalc l of
--         Just c -> Just c
--         Nothing -> findBaseCalc r

-- -- Helper function to generate all valid orderings of all possible subsets of available numbers
-- -- eg: [1,2] gives [[1], [2], [1,2], [2,1]]
-- validNumLists :: [Int] -> [[Int]]
-- validNumLists nums = concatMap permutations (filter (not . null) (subsequences nums))

-- -- Helper function to split a list into all possible left and right non-empty parts
-- splits :: [a] -> [([a], [a])]
-- splits xs = [splitAt i xs | i <- [1 .. length xs -1]]

-- -- Recursively build all possible valid Expr trees from a specific list of numbers
-- buildASTs :: [Int] -> [Op] -> [Expr]
-- buildASTs [] _ = []
-- buildASTs [n] _ = [Val n]
-- buildASTs ns ops = do
--     (leftNs, rightNs) <- splits ns
--     leftAST <- buildASTs leftNs ops
--     rightAST <- buildASTs rightNs ops
--     op <- ops
--     return (Calc op leftAST rightAST)

-- -- Create a list of all possible mathematical expressions
-- allValidExprs :: [Int] -> [Op] -> [Expr]
-- allValidExprs nums ops = do
--     numList <- validNumLists nums
--     buildASTs numList ops

-- -- Core AI solver that searches the list of valid expressions and returns a pretty printed string of the very first expression that equals the target
-- solver :: Int -> [Int] -> [Op] -> String
-- solver target nums ops =
--     let exprs = allValidExprs nums ops
--         solution = find (\e -> eval e == Right target) exprs
--     in maybe "No solution" prettyPrint solution

-- -------------------------------
-- -- Outputting error handling --
-- -------------------------------

-- Prints a single, cleanly formatted error message with its index
outputError :: String -> Int -> IO ()
outputError e n = do
  let errorMsg = "[ERROR " ++ show n ++ "]  " ++ e
  putStrLn errorMsg

-- Recursively iterates through a list of error strings and prints them
outputErrors :: [String] -> Int -> IO ()
outputErrors [] _ = return ()
outputErrors (e : es) errNo = do
  outputError e errNo
  outputErrors es (errNo + 1)

-- This function tells the user how many errors they triggered and then outputs all errors to the terminal by calling the outputErrors function
displayAllErrors :: [String] -> IO ()
displayAllErrors es = do
  let errorCount = length es
  let errorStr = "Your expression caused " ++ show errorCount ++ " errors"
  putStrLn errorStr
  outputErrors es 1

-- ------------------------------
-- -- Outputting past attempts --
-- ------------------------------

-- -- This function outputs a three messages to the screen in the form "[Attempt n] ..." "-- Outcome ..." "-- Explanation ..."
-- outputAttempt :: (String, String, String) -> Int -> IO ()
-- outputAttempt (a, b, c) n = do
--   let attemptMsg = "[Attempt " ++ show n ++ "]  " ++ a
--   putStrLn attemptMsg
--   let outcomeMsg = "-- Outcome: " ++ b
--   putStrLn outcomeMsg
--   let explanationMsg = "-- Explanation: " ++ c
--   putStrLn explanationMsg

-- -- This function iterates over the attempts list and outputs the attempts one at a time by calling the outputAttempt function
-- outputAttempts :: [(String, String, String)] -> Int -> IO ()
-- outputAttempts [] _ = return ()
-- outputAttempts (a : as) attemptNo = do
--   outputAttempt a attemptNo
--   outputAttempts as (attemptNo + 1)

-- -- This function tells the user how many attempts they have had on this level to the terminal by calling the outputAttempts function
-- displayPastAttempts :: [(String, String, String)] -> IO ()
-- displayPastAttempts as = do
--   let attemptsCount = length as
--   let attemptsStr = "You have had " ++ show attemptsCount ++ " attempts on this level so far"
--   putStrLn attemptsStr
--   outputAttempts as 1

-- getNthLevel :: Int -> [Level] -> Level
-- getNthLevel _ [] = error "Level not found"
-- getNthLevel 1 (l:ls) = l
-- getNthLevel n (l:ls) = getNthLevel (n-1) ls

-- ---------------------------------------------
-- -- Identifying difficulty of random levels --
-- ---------------------------------------------

-- -- This function estimates the difficulty of a random level based on the depth of the tree produced by the solver
-- estimateDifficulty :: [Level] -> Int -> IO()
-- estimateDifficulty levels idx = do
--                                     let depth = evaluateDepthOfProblem levels idx
--                                     case depth of
--                                       -1 -> putStrLn "Error occured when estimating the difficulty level"
--                                       0 -> putStrLn "This is an easy random level"
--                                       1 -> putStrLn "This is an easy random level"
--                                       2 -> putStrLn "This is a medium random level"
--                                       _ -> putStrLn "This is a hard random level"

-- -- This function makes a function call to calculate the depth of the expression tree and handles all eventualitites of return values from other functions
-- evaluateDepthOfProblem :: [Level] -> Int -> Int
-- evaluateDepthOfProblem levels idx = do
--                                         let currentLevel = levels !! idx
--                                         let solution = solver (target currentLevel) (nums currentLevel) (ops currentLevel)
--                                         if solution == "No solution" then -1
--                                         else do
--                                           case cleanExpressionParser solution of 
--                                             Left parseErrorMsg -> -1
--                                             Right parsedTree -> calculateDepth (normalise parsedTree)

-- -- This function returns the depth of an expression tree using recursion
-- calculateDepth :: Expr -> Int
-- calculateDepth (Val n) = 0
-- calculateDepth (Calc o e1 e2) = 1 + max (calculateDepth e1) (calculateDepth e2)


-- -- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/System-IO-Error.html
-- -- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-List.html
-- -- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-IORef.html#t:IORef
