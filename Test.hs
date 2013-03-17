module Test where

import Test.HUnit

import LangDef
import Parser
import ImpObj


testexpr :: String -> String -> Integer -> Test
testexpr msg s ie = TestCase (assertEqual msg ie result)
   where expr = case parseBigExpr s of 
            Nothing -> error ("Could not parse "++s)
            Just (expr, rest) -> expr
         result = case eval expr newState of 
            (MyInteger i, state) -> i 
            _ -> error (s ++ " did not eval to number")

simple = testexpr "simple" "1" 1
simple2 = testexpr "simple2" " 2)+3 -2" 2
ops = testexpr "calc with operators" " 2 * 2 +3 -2" 6
braces = testexpr "braces" "(2 * 2)+3 -2" 5
braces2 = testexpr "braces2" "(2*2)" 4
bracessimple = testexpr "braces simple" " ( 3 )" 3

runall = runTestTT ( "AllTests" ~: test testLst )
  where testLst = [simple, simple2, ops, braces, braces2, bracessimple]
