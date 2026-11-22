module Builtin where

import Val
import Env

envBuiltin :: Env Value
envBuiltin = setEnv "add" addBuiltin emptyEnv

addBuiltin :: Value
addBuiltin = ValBuiltin f
  where
    f (ValInt i) = ValBuiltin (g i)
    f _ = error "add only works on two ints"

    g x (ValInt y) = ValInt (x + y)
    g _ _ = error "ahh"
