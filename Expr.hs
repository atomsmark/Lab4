module Expr where

import Test.QuickCheck
import GHC.Natural (naturalFromInteger)
import Prelude 
import DynFlags (IntegerLibrary(IntegerSimple), DynFlags (mainFunIs))
import Parsing
import RegAlloc.Graph.Stats (addSRM)
import Test.QuickCheck.Text (number)
import Data.Maybe


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
showExpr (Trig Sin n)    = "sin " ++ showFactor n
showExpr (Trig Cos n)    = "cos " ++ showFactor n

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
-- | Runs the parser on the given string 
--  to return maybe a thing and a string
--parse :: Parser a -> String -> Maybe(a,String)
--parse (P f ) s = f s



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
      <|> char 's' *> char 'i' *> char 'n' *> ((Trig Sin) <$> factor)   --ev. generalisera
      <|> char 'c' *> char 'o' *> char 's' *> ((Trig Cos) <$> factor)


--prop_readExpr :: Expr -> Bool
--prop_readExpr input = fromJust (readExpr (showExpr input)) == input

{-
assoc :: Expr -> Expr
assoc (Add (Add n m) e3) = assoc (Add n (Add m e3))
assoc (Add n          m) = Add (assoc n) (assoc m)
assoc (Sub n m)          = Sub (assoc n) (assoc m)
assoc (Lit n)            = Lit n
-}
------------------------------------------------------------------------
--Parsea OCH kalkylera:
{-type ParserFun a = String -> Maybe (a,String)

num :: ParserFun Integer
num s = case span isDigit s of
    (d:ds, rest) -> Just (read (d:ds), rest)
    _            -> Nothing

addition0 :: ParserFun Integer
addition0 s = case num s of   --Olika fall för vad num s blir (Alltså en Just (ngt,ngt))
      Just (n, '+':r) -> case num r of       --Då nästa tecken som inte är en siffra är ett +
                              Just (m, r') -> Just (n+m, r')        --Adderar n och m (alla siffror fram till nästa tecken som inte är en siffra)
                              _            -> Nothing
      _               -> Nothing

multiplication0 :: ParserFun Integer
multiplication0 s = case num s of   --Olika fall för vad num s blir (Alltså en Just (ngt,ngt))
      Just (n, '*':r) -> case num r of       --Då nästa tecken som inte är en siffra är ett *
                              Just (m, r') -> Just (n*m, r')        --multiplicerar n med m (alla siffror fram till nästa tecken som inte är en siffra)
                              _            -> Nothing
      _               -> Nothing

calculation0 s = case addition0 s of
                      Nothing -> multiplication0 s
                      ok      -> ok-}

--------------------------------------------------------------------------
{-expr, term, factor :: Parser Expr

expr = foldl1 Add <$> chain term (char '+')
term = foldl1 Mul <$> chain factor (char '*')
factor = Num <$> number <|> char '(' *> expr <* char ')'


expr'  = do
    t <- term
    ts <- zeroOrMore (do char '+'; term)
    return $ foldl Add t ts

term'  = do
    t <- factor
    ts <- zeroOrMore (do char '*'; factor)
    return $ foldl Mul t ts


factor' = Num <$> number
     <|> do char '('
            e <- expr
            char ')'
            return e

chain item sep = do
    i <- item
    is <- zeroOrMore (sep *> item)
    return (i:is)-}

--E--------------------------------------------------------------------
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr input = fromJust (readExpr (showExpr $ assoc input)) == assoc input
  where
    assoc :: Expr -> Expr
    --assoc (Bin Add (Bin Add n m) e3) = assoc (Bin Add n (Bin Add m e3))
    assoc (Bin Add n (Bin Add m e3)) = assoc (Bin Add (Bin Add n m) e3)
    assoc (Bin Add n m)              = Bin Add (assoc n) (assoc m)
    --assoc (Bin Mul (Bin Mul n m) e3) = assoc (Bin Mul n (Bin Mul m e3))
    assoc (Bin Mul n (Bin Mul m e3)) = assoc (Bin Mul (Bin Mul n m) e3)
    assoc (Bin Mul n m)              = Bin Mul (assoc n) (assoc m)
    assoc (Num n)                    = Num n
    assoc (Trig Sin n)               = Trig Sin n
    assoc (Trig Cos n)               = Trig Cos n

range = 99

arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1,rNum),(s,rBin s),(s,rTrig s)]
  where
    rNum = elements $ map Num [0..range]
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


{-Also, define a generator for expressions:

arbExpr :: Int -> Gen Expr
Do not forget to take care of the size argument in the generator.
Make Expr an instance of the class Arbitrary and QuickCheck the result!

instance Arbitrary Expr where 
  arbitrary = sized arbExpr
quickCheck will call arbExpr with sizes in the range [0..99],
so make sure arbExpr produces reasonably sized expressions for testing
with this range of sizes.-}


-- 69 + 18*44*67 + cos 38 + sin 77

-- Fall 1
--Bin Add (69 + 18*(44*67))                                                   (cos 38 + sin 77)
--Bin Add (Bin Add (Num 69) (Bin Mul (Num 18) (Bin Mul (Num 44) (Num 67)))) (Bin Add (Trig Cos (Num 38)) (Trig Sin (Num 77)))

-- Fall 2
--Bin Add (69 + ((18*44)*67))                                                   (cos 38 + sin 77)
--Bin Add (Bin Add (Num 69) (Bin Mul (Num 18) (Bin Mul  (Num 67)))) (Bin Add (Trig Cos (Num 38)) (Trig Sin (Num 77)))


-- "4 * 5 * 7"

--Fall 1
--(4 * 5) * 7
--Bin Mul (Num 4) (Bin Mul (Num 5) (Num 7))

--Fall 2
--4 *(5 * 7)
--Bin Mul (Bin Mul (Num 4) (Num 5)) (Num 7)



--F--------------------------------------------------------------------
{-
simplify :: Expr -> Expr
simplify (Num n) = Num n 
simplify (Var) = Var 
simplify (Bin Add (Num 0) m) = simplify m 
simplify (Bin Add m (Num 0)) = simplify m
simplify (Bin Mul (Num 0) _) = Num 0 
simplify (Bin Mul _ (Num 0)) = Num 0 
simplify (Bin Mul m (Num 1)) = simplify m 
simplify (Bin Mul (Num 1) m) = simplify m
simplify (Bin Add n m) = simplifyBin Add (simplify n) (simplify m)
simplify (Bin Mul n m) = simplifyBin Mul (simplify n) (simplify m)
simplify (Trig Sin (Num 0)) = Num 0 -- fråga ifall detta behövs?
simplify (Trig Cos (Num 0)) = Num 1 -- fråga ifall detta behövs?
simplify (Trig Sin n) = Trig Sin (simplify n)
simplify (Trig Cos n) = Trig Cos (simplify n)

simplifyBin :: BinOp -> Expr -> Expr -> Expr
simplifyBin Add (Num n) (Num m) = Num (n+m)
simplifyBin Mul (Num n) (Num m) = Num (n*m)
simplifyBin op n m = Bin op n m
--simplifyBin op n m = Num (eval (Bin op n m) 0)  - Sätter in 0 för x och evaluerar hela uttrycket.

--quickCheck TODO
-}

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

prop_simplifyCorrect :: Expr -> Property
prop_simplifyCorrect n = property $ simplify n === n

prop_simplifySimplicity :: Expr -> Property
prop_simplifySimplicity n = property $ isFullySimplified n ==> simplify n === simplify (simplify n)
  where
    isFullySimplified :: Expr -> Bool
    isFullySimplified e = simplify e == e

--G--------------------------------------------------------------------

differentiate :: Expr -> Expr
differentiate (Num n) = Num 0
differentiate (Var) = Num 1
differentiate (Bin Add n m) = simplify (Bin Add (differentiate n) (differentiate m))
differentiate (Bin Mul n m) = simplify (Bin Add (Bin Mul (differentiate n) m) (Bin Mul n (differentiate m)))
--differentiate (Bin Add n m) = simplifyBin Add (differentiate n) (differentiate m)
--differentiate (Bin Mul n m) = simplifyBin Mul (Bin Mul (differentiate n) m) (Bin Mul n (differentiate m))
differentiate (Trig Sin n) = simplify (Bin Mul (Trig Cos n) (differentiate n))
differentiate (Trig Cos n) = simplify (Bin Mul (Num (-1)) (Bin Mul (Trig Sin n) (differentiate n)))




