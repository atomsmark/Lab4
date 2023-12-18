{- expr, term ,factor :: Parser Expr

expr

term

factor


abstraction:
library function "chain"
chain :: Parser item -> Parser sep -> Parser [item]



readEvalPrint = do      -- 5A (5)
    putStr "skriv ngt"
    s <- getLine
    let s' = filter (not.isSpace) s
    case parse expr s' of
        Just (ans,"") -> print (eval ans)
        _             -> putStrLn "Invalid Expression" -}

{-
* Lecture 5a (2), min 5.40, parsing case of, analyserar siffror från en lista med string
* Parse a single digit lecture 5(C) min 2

--example  Lecture 5a (2)
num :: ParserFun Integer
num s= case span isDigit s of
    (d:ds, rest) -> Just (read (d:ds), rest)
    _            -> Nothing
addition0
multiplication0
calculation0 s = case addition0 s of
                      Nothing ->
-}