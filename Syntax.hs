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
                            pBraces,
                            pLambda
                           ])             pCall 
                     (\b                  c      ->
                     c b)



pConcrete = convert Concrete parseNumber

pRef  = convert Ref parseIdent

pOneOpExpr = next3 pOneOp maybespace parseExpr
                   (\op   _          expr      -> op expr)

pOneOp = alts (map c ops)
   where c (s, f) = convert (const f) (atoms s)
         ops = [
                 ("~" ,negate), 
                 ("X", square),
                 ("not", notop)
                ]


pBraces = next5 (atom '(') maybespace parseExprLvl1 maybespace (atom ')') 
                (\_        _          e             _          _          -> e) 

--Any Expression may be a call of that expression, if followed by braces.
pCall = opt id (next4 maybespace (atom '(') pParams (atom ')') 
                      (\_        _          params  _           foo -> 
                      Call foo params))
         
-- use pSep here
pParams :: Parser [Expression]   
pParams = alt (convert (const []) maybespace)
              (next3 maybespace (pKommaSep parseExpr) maybespace 
                     (\_        exprs                 _          ->exprs))
                    


pLambda = next7
  (atoms "function") maybespace (atom '(') vars (atom ')') somespace parseStmt
  (\_                _          _          vars _          _         stmt      ->
      Lambda vars stmt)
   
   where vars = alt (convert (const []) maybespace)
                    (next3 maybespace (pKommaSep parseIdent) maybespace 
                           (\_        vars                   _          ->vars))
                    


parseStmt :: Parser Statement
parseStmt = alts [pAssign, 
                  pReturn, 
                  pObjAssign, 
                  pOption, 
                  pAlt,
                  pLoop,
                  pSequence] 
 
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


pSequence = next4 (atom '{') stmts maybespace (atom '}') 
                  (\_        stmts _          _          -> Sequence stmts)
   where stmts = pSep (next maybespace parseStmt (\_ stmt -> stmt))
                      (atom ';')

--other parsers
parseIdent = some (alts chars)
   where chars = map atom (['A'..'Z']++['a'..'z'])
pKey = parseIdent

