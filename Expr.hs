import Test.QuickCheck
import GHC.Natural (naturalFromInteger)

data Expr
    = Num Integer | Add Expr Expr | Mul Expr Expr          --Expr som argument gör att det strukturen på datatypen blir "tree-shaped" och inte "list-shaped"
    deriving Eq 


instance Show Expr where
    show = showExpr



eval :: Expr -> Integer
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
showFactor e           = showExpr e

