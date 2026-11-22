module Env (emptyEnv, getEnv, setEnv, Env) where

import qualified Data.Map as Map

newtype Env v = Env (Map.Map String v)
  deriving (Show, Eq)

emptyEnv :: Env v
emptyEnv = Env Map.empty

getEnv :: String -> Env v -> Maybe v
getEnv name (Env env) = Map.lookup name env

setEnv :: String -> v -> Env v -> Env v
setEnv s v (Env b) = Env (Map.insert s v b)
