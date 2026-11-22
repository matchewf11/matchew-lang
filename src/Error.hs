module Error where

import qualified Parser as Parser

data Error = ErrorParser [Parser.Error Char]
           | ErrorEval String
           deriving (Show, Eq)
