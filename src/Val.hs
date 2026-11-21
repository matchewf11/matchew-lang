module Val where

import Display

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

  -- TODO: this is borked
  display (ValList vals) = (reverse $ foldr (\x acc -> acc ++ display x) "[" vals) ++ "]"
  display (ValTuple vals) = (reverse $ foldr (\x acc -> acc ++ display x) "(" vals) ++ ")"
