module File where

import Data.Bifunctor
import Val
import Parser
import Eval
import Prog
import Builtin
import qualified Error as E

executeString :: String -> Either E.Error (Maybe Value)
executeString contents = (first E.ErrorParser $ runParser prog contents) >>= \(program, _) ->
  (\(mv, _) -> mv) <$> (first E.ErrorEval (runEval (eval program) envBuiltin))

executeFile :: String -> IO (Either E.Error (Maybe Value))
executeFile fileName = executeString <$> (readFile fileName)
