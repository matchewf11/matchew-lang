module Main (main) where

import System.Environment

import Repl
import Display
import File
import Val
import Error

-- TODO: dont print the display v in the long term
handleOutput :: Either Error (Maybe Value) -> IO ()
handleOutput (Left e) = print e
handleOutput (Right Nothing) = return ()
handleOutput (Right (Just v)) = putStrLn (display v)

evalArgs :: [String] -> IO ()
evalArgs [] = repl
evalArgs [x] = executeFile x >>= handleOutput
evalArgs _ = error "Could Not Handle N Args"

main :: IO ()
main = getArgs >>= evalArgs
