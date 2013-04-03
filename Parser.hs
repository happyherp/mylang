module Parser where

import Data.List
import Data.Char

{- Type of a function that takesa String and parses it into something else, which may be anything. The result is a number of possible interpretations of the string together with the remaning chars -}
type Parser a = String -> [(a, String)]


{-Match single Char. Result of the parser is that char. 

   ------>( <Char> ) ------>

-}
atom :: Char -> Parser Char
atom c (x:xs) | c == x = [(c, xs)]
atom a b   = [] 


{- Parse one thing, then another, then combine the results using the given function

----->[ a ]---->[ b ]--->

-}
next :: Parser a -> Parser b -> (a->b->c) -> Parser c
next pa pb f e = [ (f matcha matchb,restb) 
                     |(matcha, resta) <- pa e, 
                      (matchb, restb) <- pb resta ]

--useful for longer sequences. 
--Surely there is a better way to write this down.
next3 p1 p2 p3 f             = next (next  p1 p2 f            ) p3 id
next4 p1 p2 p3 p4 f          = next (next3 p1 p2 p3 f         ) p4 id
next5 p1 p2 p3 p4 p5 f       = next (next4 p1 p2 p3 p4 f      ) p5 id
next6 p1 p2 p3 p4 p5 p6 f    = next (next5 p1 p2 p3 p4 p5 f   ) p6 id
next7 p1 p2 p3 p4 p5 p6 p7 f = next (next6 p1 p2 p3 p4 p5 p6 f) p7 id

{- Try two diffrent parsers.


------>[ a ]----->
   |          |
   --->[ b ]---

-}
alt :: Parser a -> Parser a -> Parser a 
alt pa1 pa2 e = (pa1 e) ++ (pa2 e)


--Convert from one result to another.
convert :: (a->b) -> Parser a -> Parser b
convert f p e =  map (\(match, rest) -> (f match, rest) ) (p e)


--Remove interpretations that don't match a filter
pFilter :: (a->Bool) -> Parser a -> Parser a
pFilter f pa s= filter (f . fst) (pa s)


--Never matches anything.
never e = []

--always maches and gives value v
always v e= [(v, e)]


--Match at the end.
end v "" = [(v,"")]
end v _  = []

--only accept results with no rest.
norest p = next p (end 0) const


{- Parser for optional Syntax-Elements 

----->[ a ]----->
  |          |
   ----->----

-}
opt ::  a -> Parser a -> Parser a
opt a p = alt (always a) p


--- Handy parsers

{-Create a branch for each parser. All Parsers must be of the same type. 

------>[ a1 ]----->
   |           |
   |-->[ a2 ]--|
   |           |
    -->[ an ]-- 

-}
alts :: [Parser a] ->  Parser a
alts = foldr alt never

{-Use the parser at least one time, put content in list.

----->[ a ]------>
  |          |
   ----<-----         

-}
some :: Parser a -> Parser [a]
some p = next p (opt [] (some p)) (:)

{- Some or no occurences of the parser.

-------->------>
  |          |
   --[ a ]<--    

-}
maybeSome :: Parser a -> Parser [a]
maybeSome p = alt (always []) (some p)

{- match a string. Result is that string.

---->( <Somestring> )--->

-}
atoms :: String -> Parser String
atoms (c:[]) = convert (:[]) (atom c)
atoms (c:cs) = next (atom c) (atoms cs) (:)

{-Use the first parser once. Then, if possible use the second then the first parser as often as possible. Combine results with the given function from the right and use the second one for the last occurence of a.

----->[ a ]------->
   |           |
    --[ b ]<---


-}
prrepeat :: (a->b->c->c) -> (a->c) -> Parser a -> Parser b -> Parser c
prrepeat fabc fa pa pb = next (maybeSome (next pa pb (\a b -> (a,b ))))
                              pa
                              collect
    where collect abs a = foldr (\(a,b) c -> fabc a b c) (fa a) abs
          

{--Parse things seperated by something which has no semantik meaning. Results
go in list.

----->[ a ]------->
   |           |
    --[ b ]<---

-}
pSep :: Parser a -> Parser b -> Parser [a]
pSep pa pb= prrepeat (\a b c -> a:c) (:[]) pa pb

{- Parse things separated with kommas and spaces

----------------->[ a ]---------------->
   |                                 |
    --[ spaces ]<-( , )<-[ spaces ]<-

-}
pKommaSep :: Parser a -> Parser [a]
pKommaSep pa = pSep pa (next3 maybespace (atom ',') maybespace (\_ _ _ -> ()))


maybespace = maybeSome (atom ' ')
somespace  = some (atom ' ')
                    
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


