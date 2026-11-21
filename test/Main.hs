module Main (main) where

import Test.HUnit
import Parser
import Expr
import Stmt
import Data.Char

testStmt :: Test
testStmt = "stmt" ~: TestList
  [ runParser stmtExpr "  1   ;" ~=? Right (StmtExpr (IntLit 1), "")
  , runParser stmtExpr "()" ~=? Left [EndOfInput]
  , runParser stmtExpr "" ~=? Left [EndOfInput, Empty]
  , runParser stmt "  1   ;" ~=? Right (StmtExpr (IntLit 1), "")
  ]

testExpr :: Test
testExpr = "expr" ~: TestList
  [ runParser (eqP 'c') "char" ~=? Right ('c', "har")
  , runParser (eqP 'c') "har" ~=? Left [Unexpected 'h']

  , runParser (listP "hello") "hello world" ~=? Right ("hello", " world")
  , runParser (listP "hello") "world" ~=? Left [Unexpected 'w']

  , runParser boolLit "true" ~=? Right (BoolLit True, "")
  , runParser boolLit "false" ~=? Right (BoolLit False, "")
  , runParser boolLit "jim" ~=? Left [Unexpected 'j']

  , runParser (spanP isDigit) "123abc" ~=? Right ("123", "abc")
  , runParser (spanP isDigit) "abc123" ~=? Right ("", "abc123")

  , runParser stringLit "\"foo\"" ~=? Right (StringLit "foo", "")
  , runParser stringLit "\"\"" ~=? Right (StringLit "", "")
  , runParser stringLit "foo" ~=? Left [Unexpected 'f']

  , runParser intLit "123" ~=? Right (IntLit 123, "")
  , runParser intLit "abc" ~=? Left [Empty]

  , runParser listLit "[]" ~=? Right (ListLit [], "")
  , runParser listLit "[1]" ~=? Right (ListLit [IntLit 1], "")
  , runParser listLit "[1,1]" ~=? Right (ListLit [IntLit 1, IntLit 1], "")
  , runParser listLit "[  ]" ~=? Right (ListLit [], "")
  , runParser listLit "[1 ]" ~=? Right (ListLit [IntLit 1], "")
  , runParser listLit "[ 1]" ~=? Right (ListLit [IntLit 1], "")
  , runParser listLit "[ 1 ]" ~=? Right (ListLit [IntLit 1], "")
  , runParser listLit "[" ~=? Left [EndOfInput]
  , runParser listLit "[ 1 , 2 ]" ~=? Right (ListLit [IntLit 1, IntLit 2], "")
  , runParser listLit "[ 1 , 2 , 3 ]" ~=? Right (ListLit [IntLit 1, IntLit 2, IntLit 3], "")
  , runParser listLit "[[1], 1]" ~=? Right (ListLit [ListLit [IntLit 1], IntLit 1], "")

  , runParser ident "abc" ~=? Right (Ident "abc", "")
  , runParser ident "" ~=? Left [Empty]

  , runParser ws "  foo" ~=? Right ("  ", "foo")

  , runParser parens "(abc)" ~=? Right (Ident "abc", "")
  , runParser parens "(abc" ~=? Left [EndOfInput]

  , runParser tupleLit "()" ~=? Right (TupleLit [], "")
  , runParser tupleLit "(foo)" ~=? Left [Unexpected 'f']
  , runParser tupleLit "( 1 , )" ~=? Right (TupleLit [IntLit 1], "")
  , runParser tupleLit "( 1 , 1 )" ~=? Right (TupleLit [IntLit 1, IntLit 1], "")
  , runParser tupleLit "( 1 , 1 , 1)" ~=? Right (TupleLit [IntLit 1, IntLit 1, IntLit 1], "")

  , runParser expr "[1, 2]" ~=? Right (ListLit [IntLit 1, IntLit 2], "")
  , runParser expr "(abc)" ~=? Right (Ident "abc", "")
  , runParser expr "abc" ~=? Right (Ident "abc", "")
  , runParser expr "(abc)" ~=? Right (Ident "abc", "")
  , runParser expr "(abc,)" ~=? Right (TupleLit [Ident "abc"], "")
  , runParser expr "\"abc\"" ~=? Right (StringLit "abc", "")
  , runParser expr "123" ~=? Right (IntLit 123, "")
  , runParser expr "true" ~=? Right (BoolLit True, "")
  , runParser expr "()" ~=? Right (TupleLit [], "")


  ]

main :: IO ()
main = runTestTT (TestList [testExpr, testStmt]) >>= print
