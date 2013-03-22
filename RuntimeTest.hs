
module RuntimeTest where 

import Runtime
import LangDef
import Test.HUnit hiding (State)
import Prelude hiding (negate)


import Data.Map


--Helpful definitios for building statements
zero  = Concrete 0
one   = Concrete 1
two   = Concrete 2
three = Concrete 3
four  = Concrete 4

-- Execute statement, then return value of a specific 
-- variable of inttype
runRet :: Statement -> VarId -> Integer 
runRet statement var  = 
       let State stack heap = exec statement newState
       in case getVar stack var of
           MyInteger val -> val
           _ -> error "Not an int"

-- Direcly build a testcase with name "s" that checks that after "stmt" is executed
-- the variable "id" has the value "expected"
testStmt :: String -> Statement -> Integer -> VarId -> Test
testStmt s stmt expected id = TestCase (assertEqual s expected (runRet stmt id))


seqstmt= Sequence [
   (Assignment "a" two), 
   (Assignment "a" three), 
   (Assignment "b" four)]
seqcase = testStmt "testsequence" seqstmt 3 "a"

alt = Alternative two (Assignment "a" one ) (Assignment "a" two )
altcase = testStmt "testalt" alt 2 "a"

expr = Assignment "a" (negate (four `mult` three))
exprcase = testStmt "testmultexpr" expr (-12) "a"

fak x = Sequence [
   Assignment "a" (Concrete x), 
   Assignment "b" one,
   Loop 
     (notop (equal one (Ref "a")))
     (Sequence [
       (Assignment "b" ((Ref "a") `mult` (Ref "b"))),
       (Assignment "a" ((Ref "a") `minus` one))
     ])]
fakcase = testStmt "testfak" (fak 4) 24 "b"


fakrek x = Sequence [
       Assignment "fak" (Lambda 
                          ["n"] 
                          (Alternative (equal one (Ref "n"))
                                (Return (Ref "n"))
                                (Return (mult
                                          (Call (Ref "fak") [(Ref "n") `minus` one ])
                                          (Ref "n")
                                ))
                          )
                        ),
       Assignment "a" (Call (Ref "fak") [Concrete x])
                    ]
fakrekcase = testStmt "testfakrek" (fakrek 4) 24 "a"


ctxcheck = Sequence [
              Assignment "a" one,
              Assignment "foo" (Lambda [] (Return (Ref "a"))),
              Assignment "a" four,
              Assignment "b" (Call (Ref "foo") [])
                    ]
ctxcheckcase = testStmt "testctx" ctxcheck 1 "b"


lambdadouble = Assignment "a" (Call 
              (Lambda ["a"] (Return (two `mult` (Ref "a") )) )
              [Concrete 4])
lambdadoublecase = testStmt "test lambda" lambdadouble 8 "a"

objectstmt = Sequence [
              Assignment "a" New,
              ObjAssignment (Ref "a") "size" four,
              Assignment "b" (AccessObj (Ref "a") "size")
             ]
objectcase = testStmt "test object" objectstmt 4 "b"



runall = runTestTT ( "AllTests" ~: test testLst )
  where testLst = [seqcase, altcase, exprcase, fakcase, fakrekcase, lambdadoublecase, objectcase, ctxcheckcase]

