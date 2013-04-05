
module Runtime where

import qualified Data.Map

import Debug.Trace (trace)

import LangDef

--Defines how a Value is treated when branching
isTrue :: Value -> Bool
isTrue (MyInteger a) = a == 0

returnvar = "XreturnX" --Special Variable for return values... not cool.


-- Stack operations
push :: Stack -> Vartable -> Stack
push stack vartable = Sub stack vartable

pop :: Stack -> Stack
pop (Top _)  = error "Can't pop top level stack"
pop (Sub parent _) = parent

bottom :: Stack -> Vartable
bottom (Top t)   = t
bottom (Sub s t) = t

setVar :: Stack -> VarId -> Value -> Stack
setVar (Top table) id val = Top (Data.Map.insert id val table) 
setVar (Sub parent table) id val = Sub parent (Data.Map.insert id val table)

getVar :: Stack -> VarId -> Value
getVar (Top table) id = case Data.Map.lookup id table of
    Nothing -> error ("Unkown variable referenced " ++ show id)
    Just val -> val
getVar (Sub parent table) id = case Data.Map.lookup id table of
    Nothing -> getVar parent id
    Just val -> val



--Execute a Single Statement
exec :: Statement -> State -> State

--debugline
--exec stmt state | trace ("exec stmt:" ++ show stmt ++ " state:" ++ show state) False = undefined

exec Noop state = state

exec (Assignment var expr) state = State (setVar stack var value) heap
   where (value, State stack heap) = eval expr state

exec (Sequence statements) state = foldl (flip exec) state statements

exec (Alternative expr thenstatement elsestatement) state = 
  exec (if  isTrue condResult then thenstatement else elsestatement) newstate 
      where (condResult, newstate) = eval expr state

exec (Option expr statement) state = exec (Alternative expr statement Noop) state

exec (Loop expr statement) state  = if  isTrue condResult
                                   then exec 
                                         (Loop expr statement) 
                                         (exec statement newstate)
                                   else newstate
      where (condResult, newstate) = eval expr state

--A Return is just an assignment to a specific variable.. No Change in control flow.
exec (Return expr) state = exec (Assignment returnvar expr) state


exec (ObjAssignment src key to) state = case eval src state of
           (ObjRef id, state2) -> State vartable (Data.Map.insert id newObj heap) 
                  where (value, State vartable heap) = eval to state2
                        oldObj = getObj id (State vartable heap)
                        newObj = Data.Map.insert key value oldObj
           _ -> error "Tried to assign non object"

exec (Let pairs) (State stack heap) = State newstack heap
   where newstack = foldr stackfold stack fcts
         stackfold (id, foo) prevstack = setVar prevstack id foo
         fcts = map fctconvert pairs
         fctconvert (id, (vars, body)) = (id, Function vars body newstack)

getObj :: ObjId -> State -> Object
getObj ref (State vartable heap) = justlook ref heap "object id unknown"

justlook :: Ord a => a -> (Data.Map.Map a b) -> String -> b
justlook a map message = case Data.Map.lookup a map of
  Nothing -> error message
  Just b -> b 


-- Evaluate then Apply funtion
evalWith ::  (Value -> State -> a) -> Expression -> State -> (a, State) 
evalWith f expr state = (f val newstate, newstate)
   where (val, newstate) = eval expr state

-- Take result of an eval, then do another evaluation with that and process both values
-- together
evalChain :: (b -> Value -> State -> a) -> Expression -> (b, State) -> (a, State)
evalChain f expr (b, state) = evalWith (f b) expr state

eval :: Expression -> State -> (Value, State)

--debugline
--eval expr state | trace ("eval expr:" ++ show expr ++ " state:" ++ show state) False = undefined

eval (Concrete n) state = (MyInteger n, state)

eval (OneOp f expr) state = evalWith (\v s -> f v) expr state

eval (TwoOpInf left f right) state =  evalChain (\a v s -> f a v) right (eval left state)

eval (Ref id) (State stack heap) = (getVar stack id, State stack heap)

eval (AccessObj expr key) state = evalWith f expr state
  where f value (State stack heap) = let 
           id = (case value of ObjRef id -> id; _ -> error "no objectid returned"  )
           object = justlook id heap "Object not found in heap"
           in justlook key object "key not found in object"

eval (LambdaExpr (vars, body)) (State stack heap) = 
    (Function vars body stack, (State stack heap))

eval (Call expr paramExprs) (State stack1 heap1)
  --eval to get Function-Value 
  = case eval expr (State stack1 heap1) of  
    ((Function vars body fstack), (State stack2 heap2)) -> 
        -- evaluate the Arguments
        let (arguments, State stack3 heap3) = multiEval paramExprs (State stack2 heap2)
            -- create extend state of function with parameters
            callstate = State (buildCallStack fstack vars arguments) heap3
            -- call the function
            State stackPostCall heapPostCall = exec body callstate
        --Return return-value from call and the new heap
        in (getVar stackPostCall returnvar, State stack1 heapPostCall)

    x -> error ("Tried to call non function " ++ (show x))

eval New (State vartable heap) = (ObjRef key, State vartable (Data.Map.insert key obj heap))
        where key = head (filter 
                             (\x -> Data.Map.notMember x heap)
                             (iterate (1+) 0))
              obj = Data.Map.empty

--Prepare a new Variablestate for a method call.
--Evaluate Expressions and add to stack under the given parameter ids
buildCallContext :: [VarId] -> [Expression] -> State -> State
buildCallContext vars paramExprs state = 
    State (push stack (Data.Map.fromList (zip vars paramVals))) heap
       where (paramVals, State stack heap) = multiEval paramExprs state

buildCallStack :: Stack -> [VarId] -> [Value] -> Stack
buildCallStack stack paramvars values = 
     push stack (Data.Map.fromList (zip paramvars values))

--Evaluate expressions while passing state arround.
multiEval ::  [Expression] -> State -> ([Value], State)
multiEval [] state = ([], state)
multiEval (x:xs) state = (val:nextvals, endstate)
   where (val, newstate) = eval x state
         (nextvals, endstate) = multiEval xs newstate



