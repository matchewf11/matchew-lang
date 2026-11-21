module Error where

import qualified Parser as P

data Error = ParseError [P.Error Char]
           | EvalError String
           deriving (Show, Eq)
