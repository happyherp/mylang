module Syntax where

import Prelude hiding (negate)

import LangDef
import Parser



parseExpr :: Parser Expression
parseExpr = parseExprLvl1

--parses one statement, or multiple connected by operators. 
parseExprLvl1 = next parseExprLvl2 (maybeSome (opNexpr)) assocl
   where opNexpr = next4 maybespace 
                         pTwoOp 
                         maybespace
                         parseExprLvl2 
                         (\ _ op _ e2 e1-> op e1 e2)
         assocl = foldl (\e op -> op e) 

--Parser with mapping of operators.
pTwoOp :: Parser (Expression -> Expression -> Expression)
pTwoOp = alts (map c [
                      ("+",  plus), 
                      ("-",  minus), 
                      ("*",  mult), 
                      ("==", equal)
                     ] )
            where c (s, f) = convert (const f) (atoms s)


-- parse "smaller" things.
parseExprLvl2 = next  body 
                      pCall 
                      (\b c ->c b)
   where body = alts [pConcrete,
                      pRef,
                      pOneOpExpr,
                      pBraces
                     ]


pConcrete = convert Concrete parseNumber

pRef  = convert Ref parseIdent

pOneOpExpr = next pOneOp parseExpr id

pOneOp = alts (map c [("~" ,negate), ("X", square)])
   where c (s, f) = convert (const f) (atoms s)


pBraces = next3 (atom '(' ) 
                parseExprLvl1 
                (atom ')' ) 
                (\_ e _ -> e) 

--Any Expression may be a call of that expression, if followed by braces.
pCall = opt id (next4 maybespace
                      (atom '(') 
                      pParams 
                      (atom ')') 
                      (\_ _ params _ foo -> Call foo params))
         

pParams :: Parser [Expression]   
pParams = alt (convert (const []) maybespace)
              (prrepeat (\e _ ps-> e:ps)
                        (:[])
                        (next3 maybespace parseExpr maybespace (\_ e _-> e))
                        (atom ','))

parseIdent = some (alts chars)
   where chars = map atom (['A'..'Z']++['a'..'z'])

maybespace = maybeSome (atom ' ')
somespace  = some (atom ' ')

parseStmt :: Parser Statement
parseStmt = parseStmtSeq

parseStmtSeq = next pAtomStmt
                    (maybeSome (next (atom ';')
                                     pAtomStmt
                                     (\_ s -> s)))
                    plainOrSeq
       where plainOrSeq stmt [] = stmt
             plainOrSeq stmt stmts = Sequence (stmt:stmts)


pAtomStmt = next3 maybespace 
                  (alts [pAssign, pReturn, pObjAssign, pOption, pAlt] )
                  maybespace 
                  (\_ s _ -> s)

pAssign = next3 parseIdent 
                (next maybespace (atom '=') (\_ _  -> 1))  
                parseExpr 
                (\i _ e -> Assignment i e)

pReturn = next (atoms "return") parseExpr (\_ e -> Return e)


pObjAssign = next6 parseExpr 
                   (atom '.') 
                   pKey 
                   maybespace
                   (atom '=')
                   parseExpr
                   (\obj _ key _ _ val -> ObjAssignment obj key val )
pKey = parseIdent

pOption = next4 (atoms "if") 
                parseExpr 
                (atoms "then") 
                parseStmt
                (\_ e _ stmt -> Option e stmt)

pAlt = next6 (atoms "if") 
             parseExpr 
             (atoms "then") 
             parseStmt
             (atoms "else")
             parseStmt
             (\_ e _ stmtif _ stmtelse -> Alternative e stmtif stmtelse)
