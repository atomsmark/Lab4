import Test.QuickCheck
import GHC.Natural (naturalFromInteger)
import Prelude 
import DynFlags (IntegerLibrary(IntegerSimple))
import Parsing
import RegAlloc.Graph.Stats (addSRM)
import Test.QuickCheck.Text (number)


--A---------------------------------------------
data BinOp = Add | Mul
    deriving (Eq, Show)

data TrigOp = Sin | Cos
    deriving (Eq, Show)

data Expr
    = Num Double
    | Var
    | Bin BinOp Expr Expr
    | Trig TrigOp Expr
    deriving (Eq, Show)


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
--instance Show Expr where
--    show = showExpr

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


--D-------------------------------------------------
-- | Runs the parser on the given string 
--  to return maybe a thing and a string
--parse :: Parser a -> String -> Maybe(a,String)
--parse (P f ) s = f s



readExpr :: String -> Maybe Expr
readExpr input = case parse expr input of
                 Just (a, rest) -> Just a
                 _              -> Nothing

expr, term, factor :: Parser Expr
expr = foldl1 (Bin Add) <$> chain term (char '+')
term = foldl1 (Bin Mul) <$> chain factor (char '*')
factor = Num <$> readsP
      <|> char '(' *> expr <* char ')'
      <|> char 'x' *> return Var
      <|> char 's' char 'i' char 'n' *> (Trig Sin) expr


--prop_readExpr :: Expr -> Bool
--prop_readExpr input = readExpr (showExpr input) == input


{-assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc (Add e1 (Add e2 e3))
assoc (Add e1          e2) = Add (assoc e1) (assoc e2)
assoc (Sub e1 e2)          = Sub (assoc e1) (assoc e2)
assoc (Lit n)              = Lit n -}
                      

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




--F--------------------------------------------------------------------




--G--------------------------------------------------------------------


    











