module Eval where

import Val
import Prog
import Env
import Stmt
import Expr

newtype Eval output = Eval
  { runEval :: Env Value -> Either String (output, Env Value)
  }

instance Functor Eval where
  fmap f (Eval e) = Eval (fmap (\(a, env) -> (f a, env)) . e)

instance Applicative Eval where
  pure a = Eval (Right . (,) a)
  (Eval e1) <*> (Eval e2) = Eval $ \env ->
    e1 env >>= \(f, env') ->
      (\(a, env'') -> (f a, env'')) <$> (e2 env')

instance Monad Eval where
  return = pure
  (Eval e1) >>= f = Eval $ \env ->
    e1 env >>= \(a, env') ->
    runEval (f a) env'

eval :: Prog -> Eval (Maybe Value)
eval (Prog []) = Eval $ \env -> Right (Nothing, env)
eval (Prog [x]) = evalStmt x
eval (Prog (x:y:xs)) = evalStmt x >> (eval (Prog $ (y:xs)))

evalFn :: Value -> Value -> Either String Value
evalFn (ValFn param ex env) inputVal = (\(v, _) -> v) <$>
  (runEval (evalExpr ex) (setEnv param inputVal env))
evalFn (ValBuiltin f) inputVal = Right $ f inputVal
evalFn _ _ = Left "LHS of Apply must be a Function"

evalExpr :: Expr -> Eval Value
evalExpr (BoolLit b) = Eval $ \env -> Right (ValBool b, env)
evalExpr (IntLit i) = Eval $ \env -> Right (ValInt i, env)
evalExpr (StringLit s) = Eval $ \env -> Right (ValString s, env)
evalExpr (TupleLit xs) = ValTuple <$> (sequenceA (evalExpr <$> xs))
evalExpr (ListLit xs) = ValList <$> (sequenceA (evalExpr <$> xs))
evalExpr (Ident s) = lookupVar s
evalExpr (Lambda param e) = Eval $ \env -> Right (ValFn param e env, env)
evalExpr (Apply fnExpr inputExpr) = Eval $ \env ->
  runEval (evalExpr fnExpr) env >>= \(fnVal, env') ->
  runEval (evalExpr inputExpr) env' >>= \(inputVal, env'') ->
  evalFn fnVal inputVal >>= \outputVal ->
  Right (outputVal, env'')

evalStmt :: Stmt -> Eval (Maybe Value)
evalStmt (StmtExpr e) = Just <$> evalExpr e
evalStmt (StmtDef s e) = (\_ -> Nothing) <$> (evalExpr e >>= defineVar s)

defineVar :: String -> Value -> Eval ()
defineVar name val = Eval $ \env -> Right ((), (setEnv name val env))

lookupVar :: String -> Eval Value
lookupVar name = Eval $ \env ->
  case getEnv name env of
    Just v -> Right (v, env)
    Nothing -> Left $ "Could Not Find Var: " ++  name
