import Test.QuickCheck
import GHC.Natural (naturalFromInteger)
import Prelude


--A---------------------------------------------
data BinOp = Add | Mul
    deriving Eq

data TrigOp = Sin | Cos
    deriving Eq

data Expr
    = Num Double
    | Var
    | Bin BinOp Expr Expr
    | Trig TrigOp Expr
    deriving Eq


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
eval (


eval :: Expr -> Double
eval (Num n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sin e)     = sin (eval e)
eval (Cos e)     = cos (eval e)


vars :: Expr -> [String]
vars (Num n)      = []
vars (Var s)      = [s]
vars (Add e1 e2)  = vars e1 ++ vars e2
vars (Mul e1 e2)  = vars e1 ++ vars e2
