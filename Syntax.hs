module Syntax where

import Prelude hiding (negate)

import LangDef
import Parser



parseExpr :: Parser Expression
parseExpr = parseExprLvl1

--parses one statement, or multiple connected by operators. 
parseExprLvl1 = next parseExprLvl2 
                     (maybeSome (opNexpr)) 
                     assocl
   where opNexpr = next4 maybespace pTwoOp maybespace parseExprLvl2 
                         (\_        op     _          e2              e1
                           -> op e1 e2)
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
parseExprLvl2 = next (alts [pConcrete,
                            pRef,
                            pOneOpExpr,
                            pBraces
                           ])             pCall 
                     (\b                  c      ->
                     c b)



pConcrete = convert Concrete parseNumber

pRef  = convert Ref parseIdent

pOneOpExpr = next pOneOp parseExpr id

pOneOp = alts (map c ops)
   where c (s, f) = convert (const f) (atoms s)
         ops = [("~" ,negate), ("X", square)]


pBraces = next5 (atom '(') maybespace parseExprLvl1 maybespace (atom ')') 
                (\_        _          e             _          _          -> e) 

--Any Expression may be a call of that expression, if followed by braces.
pCall = opt id (next4 maybespace (atom '(') pParams (atom ')') 
                      (\_        _          params  _           foo -> 
                      Call foo params))
         

pParams :: Parser [Expression]   
pParams = alt (convert (const []) maybespace)
              (prrepeat (\e _ ps-> e:ps)
                        (:[])
                        (next3 maybespace parseExpr maybespace (\_ e _-> e))
                        (atom ','))


parseStmt :: Parser Statement
parseStmt = parseStmtSeq

parseStmtSeq = next pAtomStmt
                    (maybeSome (next (atom ';')
                                     pAtomStmt
                                     (\_ s -> s)))
                    plainOrSeq
       where plainOrSeq stmt [] = stmt
             plainOrSeq stmt stmts = Sequence (stmt:stmts)


pAtomStmt = next3 maybespace (alts 
                             [pAssign, 
                              pReturn, 
                              pObjAssign, 
                              pOption, 
                              pAlt,
                              pLoop] )    maybespace 
                  (\_         s           _          -> s)


-- i be coding yeah yeah hackathon that rocks hacking away on -
 
pAssign = next5 parseIdent  maybespace (atom '=') maybespace parseExpr 
                (\ident     _          _          _          expr      ->
                Assignment ident expr)

pReturn = next3 (atoms "return") somespace parseExpr 
                (\_              _         e -> 
                Return e)

pObjAssign = next7 
  parseExpr (atom '.') pKey maybespace (atom '=') (maybespace) parseExpr
  (\obj     _          key  _          _          _            val      -> 
  ObjAssignment obj key val )



pOption = next7 
 (atoms "if") somespace parseExpr somespace (atoms "then") somespace parseStmt
 (\_          _         e         _         _              _         stmt     ->
 Option e stmt)

pAlt = next5 pOption somespace (atoms "else") somespace parseStmt
             (\(Option e stmtif) 
                     _         _              _         stmtelse    -> 
             Alternative e stmtif stmtelse)

pLoop = next7 
  (atoms "while") somespace parseExpr somespace (atoms "do") somespace parseStmt
  (\_             _         cond      _         _            _         stmt      ->
          Loop cond stmt)

--other parsers
parseIdent = some (alts chars)
   where chars = map atom (['A'..'Z']++['a'..'z'])
pKey = parseIdent
maybespace = maybeSome (atom ' ')
somespace  = some (atom ' ')
