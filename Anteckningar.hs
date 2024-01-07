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

*The function "read" can turn a String to an int. "Read" is a standard function

Library functions introduced in the lecture:
*read
*oneOrMore
* (+++) combining 2 parsers into 1 minut 11-12 i föreläsning sista 5A
* chain
* sat- takes a predicate from characters to bool and if its a certain kind of charchter then it compiles it 


Example with (+++)

phoneP :: Parser PhoneNR
phoneP = localP +++ globalP
    where localP= do 
                n <- nat
                return (Local n)
            globalP = do
                    cc <- countryCode (gives us a number called cc in this case)
                    chat ' ' 
                    n<- nat
                    return (Global cc n)



#############LECTURE 5B################
monad = actions rather than instructions? 

monadic value; a value of string is a monadic value, whereas a string is a an ordinary value but an i/o of a string is a monadic value.
It just means that the type is an instance of the class monad like IO, gen or parser. "the memeber of the class monad then u can use do notation"

What is inside a monad? To be an instance of a class u need to have certain functions for ex to be in the show class you need to include the show function
*(>>=) & (>>) bind operation 
*minut 3 i f 5B(1)

take10 = do
    filename <- getLine
    contents <- readFile filename 
    putStr(take 10 contents)

    ---> Kan också skrivas som: do notation is a shorthand 

take10' = getLine >> readFile >>=  putStr . take 10


maybe monad: for ex readFile produces a string but its wrapped in the i/o context, a kind of i/o bubble that you canrt get out of.
Another example is a gen of a string; so its a string wrapped in a randomization context

functions presented in the lecture:
* Half minut 11  
* How to avoid spaghetti code minut: 20 in the video 5b(A)
* the result of a return is a Just, does the same thing
* library functions: failure, sat, item, char, digit, (+++), oneOrMore, zeroOrMore, chain


#####################Example Parsers##############################
The abstract data type representing a Parser
data Parser a = P (String -> Maybe (a, String))

#####Runs the parser on the given String to return a maybe thing and a string

parse :: Parser a -> String -> Maybe (a, String)
parse(P p) s = p s

Example on tryig the first parser and if it fails tries the second-> min 7 in 5B(2)
    


-}