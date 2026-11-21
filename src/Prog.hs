module Prog where

import Parser
import Stmt
import Control.Applicative
import Data.List.NonEmpty

newtype Prog = Prog (NonEmpty Stmt)
  deriving (Show, Eq)

prog :: Parser Char Prog
prog = Prog <$> (notNull $ many stmt)
