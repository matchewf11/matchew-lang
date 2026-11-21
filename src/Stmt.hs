module Stmt where

import Expr
import Parser
import Data.List.NonEmpty
import Control.Applicative

data Stmt = StmtExpr Expr -- <ws><expr><ws>;
          | StmtDef String Expr -- <ws><ident><ws>=<stmtExpr>
  deriving (Show, Eq)

stmtExpr :: Parser Char Stmt
stmtExpr = StmtExpr <$> (ws *> expr <* ws <* eqP ';')

stmtDef :: Parser Char Stmt
stmtDef = ((StmtDef . toList) <$> (ws *> getIdent <* ws <* eqP '=')) <*> (ws *> expr <* ws <* eqP ';')

stmt :: Parser Char Stmt
stmt = stmtExpr <|> stmtDef
