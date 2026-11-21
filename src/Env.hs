module Env (newScope, emptyEnv, getEnv, setEnv, Env) where

import Val
import Control.Applicative
import qualified Data.Map as Map

data Env = Env
  { getBindings :: Map.Map String Value
  , getParent   :: Maybe Env
  } deriving (Show, Eq)

emptyEnv :: Env
emptyEnv = Env Map.empty Nothing

getEnv :: String -> Env -> Maybe Value
getEnv name env = Map.lookup name (getBindings env) <|> (getParent env >>= getEnv name)

setEnv :: String -> Value -> Env -> Env
setEnv s v (Env b p) = Env (Map.insert s v b) p

newScope :: Env -> Env
newScope = Env Map.empty . Just
