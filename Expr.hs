module Expr where

import Test.QuickCheck
import GHC.Natural (naturalFromInteger)
import Prelude 
--import DynFlags (IntegerLibrary(IntegerSimple), DynFlags (mainFunIs))
import Parsing ( chain, char, parse, readsP, (<|>), Parser )
--import RegAlloc.Graph.Stats (addSRM)
import Test.QuickCheck.Text (number)
import Data.Maybe ( fromJust )
import Data.Aeson (Value(Bool))


--A---------------------------------------------
data BinOp = Add | Mul
    deriving (Eq)

data TrigOp = Sin | Cos
    deriving (Eq)

data Expr
    = Num Double
    | Var
    | Bin BinOp Expr Expr
    | Trig TrigOp Expr
    deriving (Eq)

x :: Expr
x = Var

num :: Double -> Expr
num n = Num n

add,mul :: Expr -> Expr -> Expr
add n m = Bin Add n m
mul n m = Bin Mul n m

sin,cos :: Expr -> Expr
sin n = Trig Sin n
cos n = Trig Cos n

size :: Expr -> Int
size (Num n) = 0
size (Var) = 0
size (Bin op n m) = 1 + (size n) + (size m)
size (Trig op n) = 1 + (size n)
--A-------------------------------------------------

--B-------------------------------------------------
instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var)       = "x"
showExpr (Bin Add n m) = showExpr n ++ " + " ++ showExpr m
showExpr (Bin Mul n m) = showFactor n ++ " * " ++ showFactor m

showExpr (Trig Sin n)    = "sin " ++ showTrig n
showExpr (Trig Cos n)    = "cos " ++ showTrig n


showFactor (Bin Add n m) = "(" ++ showExpr (Bin Add n m) ++ ")"
showFactor n               = showExpr n

showTrig (Bin op n m) = "(" ++ showExpr (Bin op n m) ++ ")"
showTrig n              = showExpr n
--B-------------------------------------------------

--C-------------------------------------------------
eval :: Expr -> Double -> Double
eval (Num n) _       = n
eval (Var) value     = value
eval (Bin Add n m) x = eval n x + eval m x 
eval (Bin Mul n m) x = eval n x * eval m x 
eval (Trig Sin e) x  = Prelude.sin (eval e x)
eval (Trig Cos e) x  = Prelude.cos (eval e x)


evalExpr :: String -> Double -> Double
evalExpr input x = maybe 0 (\expr -> eval expr x) (readExpr input)


--D-------------------------------------------------
readExpr :: String -> Maybe Expr
readExpr input =  case parse expr $ filter (/=' ') input of
                 Just (a, rest) -> Just a
                 _              -> Nothing


expr, term, factor :: Parser Expr
expr = foldl1 (Bin Add) <$> chain term (char '+')
term = foldl1 (Bin Mul) <$> chain factor (char '*')
factor = Num <$> readsP
      <|> char '(' *> expr <* char ')'
      <|> char 'x' *> return Var
      <|> char 's' *> char 'i' *> char 'n' *> ((Trig Sin) <$> factor)   
      <|> char 'c' *> char 'o' *> char 's' *> ((Trig Cos) <$> factor)

--E--------------------------------------------------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr input = fromJust (readExpr (showExpr $ assoc input)) == assoc input
  where
    assoc :: Expr -> Expr

    assoc (Bin Add n (Bin Add m g)) = assoc (Bin Add (Bin Add n m) g)
    assoc (Bin Add n m)              = Bin Add (assoc n) (assoc m)
 
    assoc (Bin Mul n (Bin Mul m g)) = assoc (Bin Mul (Bin Mul n m) g)
    assoc (Bin Mul n m)              = Bin Mul (assoc n) (assoc m)
    assoc (Num n)                    = Num n
    assoc (Var)                      = Var

   
    assoc (Trig Sin n)               = Trig Sin (assoc n)
    assoc (Trig Cos n)               = Trig Cos (assoc n)


range = 99

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1,rNum),(s,rBin s),(s,rTrig s),(s,rVar)]
  where
    rNum = elements $ map Num [0..range]
    rVar = elements [Var]
    rBin s = do
        let s' = (s `div` 2)
        op <- elements [Bin Add, Bin Mul]
        n <- arbExpr s'
        m <- arbExpr s'
        return $ op n m
    rTrig s = do
        let s' = (s `div` 2)
        op <- elements [Trig Sin, Trig Cos]
        e <- arbExpr s'
        return $ op e

instance Arbitrary Expr where 
  arbitrary = sized arbExpr

simplify :: Expr -> Expr
simplify (Bin Add n m) = simplifyAdd (simplify n) (simplify m)
simplify (Bin Mul n m) = simplifyMul (simplify n) (simplify m)
simplify (Trig Sin n) = Trig Sin (simplify n)
simplify (Trig Cos n) = Trig Cos (simplify n)
simplify n = n

simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Num 0) n = n
simplifyAdd n (Num 0) = n
simplifyAdd n m = Bin Add n m

simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Num 0) n = Num 0
simplifyMul n (Num 0) = Num 0
simplifyMul (Num 1) n = n
simplifyMul n (Num 1) = n
simplifyMul n m = Bin Mul n m




prop_simplifyCorrect :: Expr -> Double -> Bool
prop_simplifyCorrect n m = eval (simplify n) m == eval n m

prop_simplifySimplicity :: Expr -> Property
prop_simplifySimplicity n = property $ isSimplified n ==> simplify n === simplify (simplify n)
  where
    isSimplified :: Expr -> Bool
    isSimplified m = simplify m == m

--G--------------------------------------------------------------------

differentiate :: Expr -> Expr
differentiate (Num n) = Num 0
differentiate (Var) = Num 1
differentiate (Bin Add n m) = simplify (Bin Add (differentiate n) (differentiate m))
differentiate (Bin Mul n m) = simplify (Bin Add (Bin Mul (differentiate n) m) (Bin Mul n (differentiate m)))
differentiate (Trig Sin n) = simplify (Bin Mul (Trig Cos n) (differentiate n))
differentiate (Trig Cos n) = simplify (Bin Mul (Num (-1)) (Bin Mul (Trig Sin n) (differentiate n)))




