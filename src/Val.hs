module Val where

import Env
import Expr
import Display
import Data.List

data Value = ValInt Int
           | ValString String
           | ValBool Bool
           | ValList [Value]
           | ValTuple [Value]
           | ValFn String Expr (Env Value)
           | ValBuiltin (Value -> Value)

instance Display Value where
  display (ValInt i) = show i
  display (ValString s) = show s
  display (ValBool True) = "true"
  display (ValBool False) = "false"
  display (ValList vals) = '[' : (intercalate ", " (display <$> vals)) ++ "]"
  display (ValTuple [x]) = '(' : (display x) ++ ",)"
  display (ValTuple xs) = '(' : (intercalate ", " (display <$> xs)) ++ ")"
  display (ValFn param _ _) = "<fn " ++ param ++ ">"
  display (ValBuiltin _) = "<builtin>"
