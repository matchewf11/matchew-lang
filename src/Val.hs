module Val where

data Value = ValInt Int
           | ValString String
           | ValBool Bool
           | ValList [Value]
           | ValTuple [Value]
           deriving (Show, Eq)
