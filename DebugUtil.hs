module DebugUtil where

import Debug.Trace


passTrace :: Show a => String -> a -> a
passTrace msg a = trace (msg++" " ++ show a) a
