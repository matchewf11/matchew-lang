module Env (newScope, emptyEnv, getEnv, setEnv, Env) where

import Control.Applicative
import qualified Data.Map as Map

data Env v = Env
  { getBindings :: Map.Map String v
  , getParent   :: Maybe (Env v)
  } deriving (Show, Eq)

emptyEnv :: Env v
emptyEnv = Env Map.empty Nothing

getEnv :: String -> Env v -> Maybe v
getEnv name env = Map.lookup name (getBindings env) <|> (getParent env >>= getEnv name)

setEnv :: String -> v -> Env v -> Env v
setEnv s v (Env b p) = Env (Map.insert s v b) p

newScope :: Env v -> Env v
newScope = Env Map.empty . Just
