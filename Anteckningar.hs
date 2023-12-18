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