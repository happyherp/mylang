module Parser where

import Data.List
import Data.Char

{- Type of a function that is able of parsing the beginning of a string into zero or more possible things. -}
type Parser a = String -> [(a, String)]


--Match single Char 
atom :: Char -> Parser Char
atom c (x:xs) | c == x = [(c, xs)]
atom a b   = [] 


-- Parse one thing, then another, then combine the results.
next :: Parser a -> Parser b -> (a->b->c) -> Parser c
next pa pb f e = [ (f matcha matchb,restb) 
                     |(matcha, resta) <- pa e, 
                      (matchb, restb) <- pb resta ]

--useful for longer sequences. 
--Surely there is a better way to write this down.
next3 p1 p2 p3 f          = next (next  p1 p2 f         ) p3 id
next4 p1 p2 p3 p4 f       = next (next3 p1 p2 p3 f      ) p4 id
next5 p1 p2 p3 p4 p5 f    = next (next4 p1 p2 p3 p4 f   ) p5 id
next6 p1 p2 p3 p4 p5 p6 f = next (next5 p1 p2 p3 p4 p5 f) p6 id

-- Try two diffrent parsers.
alt :: Parser a -> Parser a -> Parser a 
alt pa1 pa2 e = (pa1 e) ++ (pa2 e)


--Convert from one result to another.
convert :: (a->b) -> Parser a -> Parser b
convert f p e =  map (\(match, rest) -> (f match, rest) ) (p e)

--Never matches anything.
never e = []

--always maches and gives value v
always v e= [(v, e)]


--Match at the end.
end v "" = [(v,"")]
end v _  = []

--only accept results with no rest.
norest p = next p (end 0) const

opt ::  a -> Parser a -> Parser a
opt a p = alt (always a) p


--- Handy parsers

--Create a branch for each parser. 
alts :: [Parser a] ->  Parser a
alts = foldr alt never

--Use the parser at least one time, put content in list.
some :: Parser a -> Parser [a]
some p = next p (opt [] (some p)) (:)

-- Some or no occurences of the parser.
maybeSome :: Parser a -> Parser [a]
maybeSome p = alt (always []) (some p)

--match a string.
atoms :: String -> Parser String
atoms (c:[]) = convert (:[]) (atom c)
atoms (c:cs) = next (atom c) (atoms cs) (:)

--Use the first parser once. Then, if possible use the second then the first parser as often as possible. Combine results with the given function from the right and use the second one for the last occurence of a.
prrepeat :: (a->b->c->c) -> (a->c) -> Parser a -> Parser b -> Parser c
prrepeat fabc fa pa pb = next (maybeSome (next pa pb (\a b -> (a,b ))))
                              pa
                              collect
    where collect abs a = foldr (\(a,b) c -> fabc a b c) (fa a) abs
                              
--parses a single digit. 
parseDigit =  convert  
                  (fromIntegral . digitToInt)
                  (alts (map atom ['0'..'9']))
                  
parseNumber :: Parser Integer
parseNumber = convert f (some parseDigit)
   where f = foldl (\a b -> a*10 + b ) 0 


parseCalc = norest (next parseNumber 
                         (opt id (next parseOp parseCalc id))
                         (flip id))

parseOp = alts [
                convert (const (+)) (atom '+'), 
                convert (const (*)) (atom '*')
               ]


