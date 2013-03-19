module Lang where

import LangDef
import Parser
import Syntax
import qualified Runtime 

{- High level commands for executing code -}

parseIt :: Parser a -> String -> a
parseIt parser s= case norest parser s of 
           [(result,"")] -> result
           [] -> error ("Could not parse "++s)
           exps  -> error ("Multiple interpretations: "++ show exps)


eval :: String -> String
eval s = show (fst (Runtime.eval (parseIt parseExpr s) newState))

run :: String -> String
run s = show (Runtime.exec (parseIt parseStmt s) newState)

