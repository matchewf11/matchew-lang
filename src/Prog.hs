module Prog where

import Parser
import Stmt
import Control.Applicative

newtype Prog = Prog [Stmt]
  deriving (Show, Eq)

prog :: Parser Char Prog
prog = Prog <$> ((many stmt) <* ws)
