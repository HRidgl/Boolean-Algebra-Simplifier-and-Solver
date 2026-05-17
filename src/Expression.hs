module Expression where

-- Operator type
data Op = Add | Sub | Mul | Div | Pow | Mod deriving (Eq, Ord) -- allowed operators

instance Show Op where -- pretty printing for the operators
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Pow = "^"
    show Mod = "%"

-- Expression type
data Expr = Val Int | Calc Op Expr Expr deriving (Show, Eq, Ord) -- expression definition (either one value or a calculation)

-- Structured error type
data EvalError = DivByZero | NonIntegerDiv | NegativeExponent deriving (Eq) -- structured error

instance Show EvalError where -- pretty printing error messages
    show DivByZero = "Attempted to divide by zero"
    show NonIntegerDiv = "Division result is not an integer"
    show NegativeExponent = "Cannot use a negative exponent"

-- Evaluate the user's expression to ensure it is valid
eval :: Expr -> Either EvalError Int
eval (Val n) = Right n
-- calculation handling
eval (Calc op e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2

    case op of
        Add -> Right (v1 + v2)
        Sub -> Right (v1 - v2)
        Mul -> Right (v1 * v2)

        -- ensure a positive power
        Pow -> 
            if v2 < 0 then
                Left NegativeExponent
            else
                Right (v1 ^ v2)
        
        -- ensure you can't mod by zero
        Mod ->
            if v2 == 0 then
                Left DivByZero
            else
                Right (v1 `mod` v2)

        -- ensure only integer division
        Div -> 
            if v2 == 0
                then Left DivByZero
                else if v1 `mod` v2 /= 0
                    then Left NonIntegerDiv
                else
                    Right (v1 `div` v2)

prettyPrint :: Expr -> String
prettyPrint (Val n) = if n < 0 then "(" ++ show n ++ ")" else show n
prettyPrint (Calc op e1 e2) = "(" ++ prettyPrint e1 ++ " " ++ show op ++ " " ++ prettyPrint e2 ++ ")"

-- normalises an expression by alphabetising and sorting operations
normalise :: Expr -> Expr
normalise (Val n) = Val n
normalise (Calc op e1 e2) =
    let norm1 = normalise e1
        norm2 = normalise e2
    in if (op == Add || op == Mul) && norm1 > norm2 then
        Calc op norm2 norm1
    else
        Calc op norm1 norm2

-- anything returned of type Left before it is an error
-- anything with of type Right is success
-- https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Either.html