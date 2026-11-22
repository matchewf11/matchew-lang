module Val where

import Display
import Data.List

data Value = ValInt Int
           | ValString String
           | ValBool Bool
           | ValList [Value]
           | ValTuple [Value]
           deriving (Show, Eq)

instance Display Value where
  display (ValInt i) = show i
  display (ValString s) = show s
  display (ValBool True) = "true"
  display (ValBool False) = "false"
  display (ValList vals) = '[' : (intercalate ", " (display <$> vals)) ++ "]"
  display (ValTuple [x]) = '(' : (display x) ++ ",)"
  display (ValTuple xs) = '(' : (intercalate ", " (display <$> xs)) ++ ")"
