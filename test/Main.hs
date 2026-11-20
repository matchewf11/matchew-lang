module Main (main) where

import Test.HUnit
import Parser
import Data.Char

testParser :: Test
testParser = "parser" ~: TestList
  [ runParser (eqP 'c') "char" ~=? Just ('c', "har")
  , runParser (eqP 'c') "har" ~=? Nothing

  , runParser (listP "hello") "hello world" ~=? Just ("hello", " world")
  , runParser (listP "hello") "world" ~=? Nothing

  , runParser boolLit "true" ~=? Just (BoolLit True, "")
  , runParser boolLit "false" ~=? Just (BoolLit False, "")
  , runParser boolLit "jim" ~=? Nothing

  , runParser (spanP isDigit) "123abc" ~=? Just ("123", "abc")
  , runParser (spanP isDigit) "abc123" ~=? Just ("", "abc123")

  , runParser stringLit "\"foo\"" ~=? Just (StringLit "foo", "")
  , runParser stringLit "\"\"" ~=? Just (StringLit "", "")
  , runParser stringLit "foo" ~=? Nothing

  , runParser intLit "123" ~=? Just (IntLit 123, "")
  , runParser intLit "abc" ~=? Nothing

  , runParser ident "abc" ~=? Just (Ident "abc", "")
  , runParser ident "" ~=? Nothing

  , runParser parens "(abc)" ~=? Just (Ident "abc", "")
  , runParser parens "(abc" ~=? Nothing

  , runParser expr "(abc)" ~=? Just (Ident "abc", "")
  , runParser expr "abc" ~=? Just (Ident "abc", "")
  , runParser expr "\"abc\"" ~=? Just (StringLit "abc", "")
  , runParser expr "123" ~=? Just (IntLit 123, "")
  , runParser expr "true" ~=? Just (BoolLit True, "")
  ]

main :: IO ()
main = runTestTT testParser >>= print
