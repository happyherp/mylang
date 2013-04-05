module LangDef where

import qualified Data.Map

data Statement =  Noop 
                | Assignment VarId Expression 
                | Sequence [Statement] 
                | Alternative Expression Statement Statement
                | Option      Expression Statement  
                | Loop        Expression Statement
                | Return      Expression
                | ObjAssignment Expression ObjKey Expression
                | Let [(VarId, Lambda)]
                  deriving (Show)


data Expression =  Concrete Integer
                 | OneOp (Value -> Value) Expression
                 | TwoOpInf 
                      Expression 
                      (Value -> Value -> Value) 
                      Expression
                 | Ref VarId
                 | LambdaExpr Lambda
                 | Call Expression [Expression]
                 | New 
                 | AccessObj Expression ObjKey


data Value =   MyInteger Integer
             | Function [VarId] Statement Stack
             | ObjRef ObjId
               deriving (Show)

data State = State Stack Heap
             deriving (Show)

data Stack =   Top Vartable
             | Sub Stack Vartable
               deriving (Show)


type Heap     = Data.Map.Map ObjId Object
type Vartable = Data.Map.Map VarId Value 
type VarId = String
type ObjId = Integer
type Object = Data.Map.Map ObjKey Value
type ObjKey = String
type Lambda = ([VarId], Statement)

newState = State (Top Data.Map.empty) Data.Map.empty

instance Show Expression where
  show (Concrete a) = show a
  show (OneOp f expr) = "f("++(show expr)++")"
  show (TwoOpInf left f right) = "|"++(show left) ++ " x " ++ (show right) ++ "|"
  show (Ref varid) = "Var#"++(show varid)
  show (Call foo params) = "Call"++(show foo)++" With "++ (show params)
  show (LambdaExpr (vars, body)) = (show vars) ++ "->" ++ (show body)
  show New = "New"
  show (AccessObj expr key) = show expr ++ "." ++ key


---- Wraps around haskell functions
intWrap :: (Integer -> Integer -> Integer) 
           -> Expression -> Expression -> Expression
intWrap f a b = TwoOpInf a wrap b 
  where wrap (MyInteger a) (MyInteger b) = MyInteger (f a b)
        wrap x y = error (show x ++ " or " ++ show y ++" are no numbers")


mult  = intWrap (*)
plus  = intWrap (+)
minus = intWrap (-)
equal = intWrap (\x y -> if x == y then 0 else 1)


singleIntWrap :: (Integer -> Integer) -> Expression -> Expression
singleIntWrap f o = OneOp wrap o
   where wrap (MyInteger x) = MyInteger (f x)

--rewrite using singleIntWrap
negate :: Expression -> Expression
negate a = OneOp f a
  where f (MyInteger i) = MyInteger (-i) 

square :: Expression -> Expression 
square a = OneOp f a 
  where f (MyInteger i) = MyInteger (i*i)

notop = singleIntWrap f
  where f 0 = 1
        f x = 0

