module Expr where

import Control.Applicative
import Data.Char
import Parser
import Data.List.NonEmpty

data Expr = BoolLit Bool -- true | false
          | IntLit Int -- [0-9]+
          | StringLit String -- " [^"]* "
          | Ident String -- [a-zA-Z]+
          | ListLit [Expr] -- [ (epsilon | <expr> (, <expr>)*) ]
          | TupleLit [Expr] -- () | ( <expr>, ) | ( expr (,<expr>)+ )
          | Lambda String Expr -- | <ident>* | <expr>
          -- ( <expr> )
          deriving (Show, Eq)

expr :: Parser Char Expr
expr = parens
  <|> boolLit
  <|> intLit
  <|> stringLit
  <|> listLit
  <|> tupleLit
  -- must be after all keywords / bools to avoid conflicts
  <|> ident
  <|> lambda

lambda :: Parser Char Expr
lambda = ((Lambda . toList) <$> (eqP '|' *> ws *> getIdent <* ws <* eqP '|' <* ws)) <*> expr

listLit :: Parser Char Expr
listLit = ListLit <$> (eqP '[' *> ws *> elements <* ws <* eqP ']')
  where
    elements :: Parser Char [Expr]
    elements = sepBy expr (ws *> eqP ',' <* ws)

intLit :: Parser Char Expr
intLit = (IntLit . read . toList) <$> (notNull $ spanP isDigit)

stringLit :: Parser Char Expr
stringLit = StringLit <$> (eqP '"' *> (spanP (/= '"')) <* eqP '"')

parens :: Parser Char Expr
parens = eqP '(' *> expr <* eqP ')'

ident :: Parser Char Expr
ident =  (Ident . toList) <$> getIdent

getIdent :: Parser Char (NonEmpty Char)
getIdent = notNull $ spanP isAlpha

tupleLit :: Parser Char Expr
tupleLit = TupleLit <$> (eqP '(' *> ws *> elements <* ws <* eqP ')')
  where
    tuple :: Parser Char [Expr]
    tuple = ((:) <$> expr <* ws <* eqP ',' <* ws) <*> (sepBy expr (ws *> (eqP ',') <* ws))
    elements :: Parser Char [Expr]
    elements = tuple <|> pure []

boolLit :: Parser Char Expr
boolLit = trueP <|> falseP
  where
    trueP = (const $ BoolLit True) <$> (listP "true")
    falseP = (const $ BoolLit False) <$> (listP "false")
