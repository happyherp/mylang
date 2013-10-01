module Indent where

import Parser


{- Parse indented blocks of code -}

type IndentParser a = Int -> Parser a

data SLang = Simple | Block [SLang] 
  deriving Show

{- sample language:
stmt
stmt
block
  stmt
  block
    stmt
  stmt
stmt 
-}

{-Parses a single statement of the sample-language -}
parseSLang :: IndentParser SLang
parseSLang i = next (atoms (replicate i ' '))
                    (alt (convert (const Simple) (atoms "stmt\r\n") )
                         (next (atoms "block\r\n") (parseNewBlock i) 
                               (\_                  b                  ->  b)))
                    (\_ s -> s)

{- finds a new, deeper indented block -}
parseNewBlock i = next (atoms (replicate i ' ')) 
                       (nextP (convert length (some (atom ' ')))
                              (\indentAdd -> next (parseSLang 0) --First Statement 
                                                  (maybeSome (parseSLang (i+indentAdd)))
                                                  (:)))
                       (\_ b -> Block b)
                   
