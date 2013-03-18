module Lang where

import LangDef
import Parser
import Syntax
import qualified Runtime 

eval :: String -> String
eval s = case norest parseExpr s of 
           [(expr,"")] -> show (fst (Runtime.eval expr newState))
           [] -> error ("Could not parse "++s)
           exps  -> error ("Multiple interpretations: "++ show exps)

