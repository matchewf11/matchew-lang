module Eval where

import Prog
import Stmt
import Expr
import qualified Data.List.NonEmpty as NE

data Value = ValInt Int
           | ValString String
           | ValBool Bool
           | ValList [Value]
           | ValTuple [Value]
           deriving (Show, Eq)

eval :: Prog -> Value
eval (Prog xs) = evalStmt $ NE.last xs

evalStmt :: Stmt -> Value
evalStmt (StmtExpr e) = evalExpr e

evalExpr :: Expr -> Value
evalExpr (BoolLit b) = ValBool b
evalExpr (IntLit i) = ValInt i
evalExpr (StringLit s) = ValString s
evalExpr (TupleLit xs) = ValTuple $ evalExpr <$> xs
evalExpr (ListLit xs) = ValList $ evalExpr <$> xs
evalExpr (Ident _) = error "TODO: add defs and env"
