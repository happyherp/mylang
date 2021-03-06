{- Tests that strings are converted into language elements.-}
module SyntaxTest where

import Test.HUnit hiding (State)

import LangDef
import Syntax
import Parser


raisepErr result = case result of 
      e ->  error ("Did not expect" ++ show e)


concrete = TestCase (assertEqual "concrete" True
                                 (case norest parseExpr "234" of 
                                    [(Concrete 234,"")] -> True
                                    e -> raisepErr e))


ref = TestCase (assertEqual "Reference" True
                            (case norest parseExpr "varname" of 
                                [(Ref "varname", "")] -> True
                                e -> raisepErr e))

noref = TestCase (assertEqual "no Reference" True
                            (case norest parseExpr "not" of 
                                [] -> True
                                e -> raisepErr e))

negation = TestCase (assertEqual "oneop negation" 5
    (case norest parseExpr "~5" of 
      [(OneOp f (Concrete val), "")] -> val
      e -> raisepErr e))

notcase = TestCase (assertEqual "oneop not" 5
    (case norest parseExpr "not 5" of 
      [(OneOp f (Concrete val), "")] -> val
      e -> raisepErr e))

multcase = TestCase (assertEqual "twoop mult" (4,5)
    (case norest parseExpr "4 * 5" of 
      [(TwoOpInf (Concrete val) f (Concrete val2), rest)] -> (val, val2)
      e -> raisepErr e))

addcase = TestCase (assertEqual "twoop add" (4,5)
    (case norest parseExpr "4 + 5" of 
      [(TwoOpInf (Concrete val) f (Concrete val2), rest)] -> (val, val2)
      e -> raisepErr e))

-- in case i switch back to right association
rassoc = TestCase (assertEqual "mix" (4,5,6)
    (case norest parseExpr "4 + 5 * 6" of 
      [(TwoOpInf (Concrete val) 
                 f 
                 (TwoOpInf (Concrete val2) 
                           f2 
                           (Concrete val3)), rest)] -> (val, val2, val3)
      e -> raisepErr e))

lassoc = TestCase (assertEqual "mix" (4,5,6)
    (case norest parseExpr "4 + 5 * 6" of 
      [(TwoOpInf (TwoOpInf (Concrete val) f (Concrete val2)) 
                 f2 
                 (Concrete val3), rest)] -> (val, val2, val3)
      e -> raisepErr e))

braces = TestCase (assertEqual "mix" (4,5,6)
    (case norest parseExpr "4 + (5 * 6  )" of 
      [(TwoOpInf (Concrete val) 
                 f 
                 (TwoOpInf (Concrete val2) 
                           f2 
                           (Concrete val3)), rest)] -> (val, val2, val3)
      e -> raisepErr e))


callcase = TestCase (assertEqual "call" 
                                 ("ab", "cd", "ef") 
                                 (case norest parseExpr "ab(cd, ef)" of 
              [(Call (Ref ab) [(Ref cd), (Ref ef)], "")] -> (ab,cd,ef) 
              e -> raisepErr e))

callempty = TestCase (assertEqual "call" "ab"
                                 (case norest parseExpr "ab(  )" of 
              [(Call (Ref ab) [], "")] -> ab 
              e -> raisepErr e))

lambdacase = TestCase (assertEqual "lambda" ("a", "a")
                       (case norest parseExpr "function(a) return a" of
                               [(LambdaExpr ([v], (Return (Ref v2))),"")] -> (v,v2) 
                               e -> raisepErr e))


runallexpr = runTestTT ( "All Expr Tests" ~: test testLst )
  where testLst = [concrete, ref, noref, negation, multcase, notcase, addcase, lassoc, braces, callcase,callempty, lambdacase]



assign = TestCase (assertEqual "assign" ("a", 2)
                      (case norest parseStmt "a = 2" of 
                           [(Assignment a (Concrete b), "")] -> (a,b)
                           e -> raisepErr e))

seqcase = TestCase (assertEqual "assign" ("a", 2, "b", 3)
                      (case norest parseStmt "{a = 2; b = 3}" of 
                           [(Sequence [
                                      Assignment a (Concrete b),
                                      Assignment c (Concrete d)
                                      ], "")] -> (a,b,c,d)
                           e -> raisepErr e))

seqcase2 = TestCase (assertEqual "assign" True
                      (case norest parseStmt "{a = 2; b = 3; c = 4}" of 
                           [(Sequence [a, b, c], "")] -> True
                           e -> raisepErr e))

returncase = TestCase (assertEqual "return" 3
                      (case norest parseStmt "return 3" of 
                           [(Return (Concrete a), "")] -> a
                           e -> raisepErr e))

obsassigncase = TestCase (assertEqual "objassign" ("a", "attr", 2)
                      (case norest parseStmt "a.attr = 2" of 
                           [(ObjAssignment (Ref ref) 
                                           key 
                                           (Concrete val),"")] -> (ref, key, val)
                           e -> raisepErr e))

optioncase = TestCase (assertEqual "option" (1,2)
                      (case norest parseStmt "if 1 then return 2" of 
                           [(Option (Concrete c) (Return (Concrete i)),"")] -> (c, i)
                           e -> raisepErr e))

altcase = TestCase (assertEqual "option" (1,2,3)
    (case norest parseStmt "if 1 then return 2 else return 3" of 
                           [(Alternative (Concrete c) 
                                         (Return (Concrete i))
                                         (Return (Concrete e)),"")] -> (c, i,e)
                           e -> raisepErr e))

loopcase = TestCase (assertEqual "option" (1,2)
    (case norest parseStmt "while 1 do return 2" of 
                           [(Loop (Concrete c) (Return (Concrete i)),"")] -> 
                                      (c, i)
                           e -> raisepErr e))

letcase = TestCase (assertEqual "let" True
    (case norest parseStmt "let {a = function() return 1}" of 
                           [(Let [("a",([], Return (Concrete 1)))],"")] -> True
                           e -> raisepErr e))

letcase2 = TestCase (assertEqual "let" True
    (case norest parseStmt "let {a = function() return 1;b=function() return 2}" of 
                           [(Let [("a",([], Return (Concrete 1))),
                                  ("b",([], Return (Concrete 2)))],"")] -> True
                           e -> raisepErr e))

runallstmt = runTestTT ( "All Stmt Tests" ~: test testLst )
  where testLst = [assign, seqcase, seqcase2, returncase, 
                   obsassigncase,optioncase, altcase, loopcase, letcase, letcase2 ]


