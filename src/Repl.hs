module Repl where

import System.IO (isEOF, hFlush, stdout)

import Env
import Val
import Display
import Eval
import Prog
import Parser

-- TODO: only return value if the '_' is empty
evalInput :: String -> Either [Error Char] (Eval (Maybe Value))
evalInput = fmap (\(program, _) -> eval program) . runParser prog

handleEOF :: IO () -> IO ()
handleEOF a = isEOF >>= \b -> if b then putStrLn "" else a

putPrefix :: IO ()
putPrefix = putStr "> " >> hFlush stdout

replLoop :: Env Value -> IO ()
replLoop env = putPrefix >> (handleEOF (getLine >>= handleInput env))

handleInput :: Env Value -> String -> IO ()
handleInput env input =
  case evalInput input of
    Left e -> print e >> replLoop env
    Right v ->
      case runEval v env of
        Left e -> putStrLn e >> replLoop env
        Right (Just mv, env') -> putStrLn (display mv) >> replLoop env'
        Right (Nothing, env') -> replLoop env'

repl :: IO ()
repl = replLoop emptyEnv
