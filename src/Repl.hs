module Repl where

import System.IO (isEOF, hFlush, stdout)

import Env
import Val
import Eval
import Prog
import Parser

evalInput :: String -> Either [Error Char] (Eval (Maybe Value))
evalInput = fmap (\(program, _) -> eval program) . runParser prog

handleEOF :: IO () -> IO ()
handleEOF a = isEOF >>= \b -> if b then putStrLn "" else a

putPrefix :: IO ()
putPrefix = putStr "> " >> hFlush stdout

replLoop :: Env -> IO ()
replLoop env = putPrefix >> (handleEOF (getLine >>= handleInput env))

handleInput :: Env -> String -> IO ()
handleInput env input =
  case evalInput input of
    Left e -> print e >> replLoop env
    Right v ->
      case runEval v env of
        Left e -> print e >> replLoop env
        Right (Just mv, env') -> print mv >> replLoop env'
        Right (Nothing, env') -> replLoop env'

repl :: IO ()
repl = replLoop emptyEnv
