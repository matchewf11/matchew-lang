module Stmt where

import Expr
import Parser

data Stmt = StmtExpr Expr -- <expr>;
  deriving (Show, Eq)

stmtExpr :: Parser Char Stmt
stmtExpr = StmtExpr <$> (ws *> expr <* ws <* eqP ';')

stmt :: Parser Char Stmt
stmt = stmtExpr
