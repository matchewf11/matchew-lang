module Env (emptyEnv, getEnv, setEnv, Env) where

import Val
import Control.Applicative
import qualified Data.Map as Map

data Env = Env
  { getParent   :: Maybe Env
  , getBindings :: Map.Map String Value
  } deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = Env Nothing Map.empty

getEnv :: String -> Env -> Maybe Value
getEnv name env = Map.lookup name (getBindings env) <|> (getParent env >>= getEnv name)

setEnv :: String -> Value -> Env -> Env
setEnv s v (Env p b) = Env p (Map.insert s v b)
