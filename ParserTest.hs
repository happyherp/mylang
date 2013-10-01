{- Test the Parser-Module-}
module ParserTest where 

import Test.HUnit
import Parser


a = TestCase (assertEqual "parse char" 
                                      [('a',"b")]
                                      (atom 'a' "ab"))

b = TestCase (assertEqual "parse char" 
                                      [] 
                                      (atom 'b' "ab"))

ab =  TestCase (assertEqual "parse ab next"
                   [("ab", "c")]
                   (next (atom 'a') (atom 'b') (\ a b -> [a, b]) "abc" ))


aaltb =  TestCase (assertEqual "parse alt"
                   [('b', "xx")]
                   (alt (atom 'a') (atom 'b') "bxx"))

digit = TestCase (assertEqual "parse digit"
                   [(3, "aaa")]
                   (parseDigit "3aaa"))

number = TestCase (assertEqual "parse Number"
                    [(1, "23"), (12, "3"), (123,"")]
                    (parseNumber "123"))


calc = TestCase (assertEqual "calculation"
                   [(4, "")]
                   (parseCalc "2+2"))

filtertest = TestCase(assertEqual "filter" [(True, "")]
       (pFilter id  (alts [always True, always False] ) ""))



longsyntax = TestCase (assertEqual "long"
                         [("yo","")]
                         (next (next (next (atom 'a') (atom 'b') f)
                                     (atom 'c') 
                                     id)
                               (atom 'd') 
                               id
                               "abcd"))
            where f a b c d = "yo" 

testnext4 = TestCase (assertEqual "next4"
                         [("yo","")]
                         (next4 (atom 'a') 
                                (atom 'b') 
                                (atom 'c') 
                                (atom 'd')
                                (\ a b c d -> "yo")
                                "abcd"))
                                
testatoms = TestCase (assertEqual "testatoms" 
                          [("abcd","")]
                          (atoms "abcd" "abcd"))

testrrepeat = TestCase (assertEqual "testrepeat"
                                   [("a a a","")]
                                   (norest (prrepeat (\a b c -> a:(' ':c))
                                                     (\ a -> [a])
                                                     (atom 'a') 
                                                     (atom ','))
                                            "a,a,a"))

testpsep = TestCase (assertEqual "testsep"
                                 [("aaa","")]
                                 (norest (pSep (atom 'a') (atom ',')) "a,a,a"))
                                 
testkommasep = TestCase (assertEqual "testkommasep"
                                 [("aaa","")]
                                 (norest (pKommaSep (atom 'a')) "a , a,a"))

testNextP = TestCase (
  assertEqual "testNextP"
            [("bbbb","b")]
            (nextP (some (atom 'a'))
                   (\as -> atoms (replicate (2*(length as)) 'b'))
                   "aabbbbb"))             

runall = runTestTT ( "AllTests" ~: test testLst )
  where testLst = [a,b, ab, aaltb, digit, number, calc, longsyntax, filtertest, testnext4, testatoms, testrrepeat, testpsep, testkommasep, testNextP]
