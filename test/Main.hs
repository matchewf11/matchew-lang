module Main (main) where

import Test.HUnit
import Parser
import Expr
import Stmt
import Data.Char

testStmt :: Test
testStmt = "stmt" ~: TestList
  [ runParser stmtExpr "()" ~=? Nothing
  , runParser stmtExpr "" ~=? Nothing
  , runParser stmtExpr "  1   ;" ~=? Just (StmtExpr (IntLit 1), "")

  , runParser stmt "  1   ;" ~=? Just (StmtExpr (IntLit 1), "")
  ]

testExpr :: Test
testExpr = "expr" ~: TestList
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

  , runParser listLit "[]" ~=? Just (ListLit [], "")
  , runParser listLit "[1]" ~=? Just (ListLit [IntLit 1], "")
  , runParser listLit "[1,1]" ~=? Just (ListLit [IntLit 1, IntLit 1], "")
  , runParser listLit "[  ]" ~=? Just (ListLit [], "")
  , runParser listLit "[1 ]" ~=? Just (ListLit [IntLit 1], "")
  , runParser listLit "[ 1]" ~=? Just (ListLit [IntLit 1], "")
  , runParser listLit "[ 1 ]" ~=? Just (ListLit [IntLit 1], "")
  , runParser listLit "[" ~=? Nothing
  , runParser listLit "[ 1 , 2 ]" ~=? Just (ListLit [IntLit 1, IntLit 2], "")
  , runParser listLit "[ 1 , 2 , 3 ]" ~=? Just (ListLit [IntLit 1, IntLit 2, IntLit 3], "")
  , runParser listLit "[[1], 1]" ~=? Just (ListLit [ListLit [IntLit 1], IntLit 1], "")

  , runParser ident "abc" ~=? Just (Ident "abc", "")
  , runParser ident "" ~=? Nothing

  , runParser ws "  foo" ~=? Just ("  ", "foo")

  , runParser parens "(abc)" ~=? Just (Ident "abc", "")
  , runParser parens "(abc" ~=? Nothing

  , runParser tupleLit "()" ~=? Just (TupleLit [], "")
  , runParser tupleLit "(foo)" ~=? Nothing
  , runParser tupleLit "( 1 , )" ~=? Just (TupleLit [IntLit 1], "")
  , runParser tupleLit "( 1 , 1 )" ~=? Just (TupleLit [IntLit 1, IntLit 1], "")
  , runParser tupleLit "( 1 , 1 , 1)" ~=? Just (TupleLit [IntLit 1, IntLit 1, IntLit 1], "")

  , runParser expr "[1, 2]" ~=? Just (ListLit [IntLit 1, IntLit 2], "")
  , runParser expr "(abc)" ~=? Just (Ident "abc", "")
  , runParser expr "abc" ~=? Just (Ident "abc", "")
  , runParser expr "(abc)" ~=? Just (Ident "abc", "")
  , runParser expr "(abc,)" ~=? Just (TupleLit [Ident "abc"], "")
  , runParser expr "\"abc\"" ~=? Just (StringLit "abc", "")
  , runParser expr "123" ~=? Just (IntLit 123, "")
  , runParser expr "true" ~=? Just (BoolLit True, "")
  , runParser expr "()" ~=? Just (TupleLit [], "")


  ]

main :: IO ()
main = runTestTT (TestList [testExpr, testStmt]) >>= print
