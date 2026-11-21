module Parser where

import Control.Applicative
import Data.Char
import Control.Monad

newtype Parser input output = Parser
  { runParser :: [input] -> Maybe (output, [input])
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser (fmap (\(a, xs) -> (f a, xs)) . p)

instance Applicative (Parser i) where
  pure c = Parser (Just . (,) c)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    p1 input >>= \(f, input') ->
    (\(a, input'') -> (f a, input'')) <$> (p2 input')

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

data Expr = BoolLit Bool -- true | false
          | IntLit Int -- [0-9]+
          | StringLit String -- " [^"]* "
          | Ident String -- [a-zA-Z]+
          | ListLit [Expr] -- [ (epsilon | <expr> (, <expr>)*) ]
          | TupleLit [Expr] -- () | ( <expr>, ) | ( expr (,<expr>)+ )
          -- ( <expr> )
          deriving (Show, Eq)

eqP :: Eq a => a -> Parser a a
eqP x = Parser $ \xs ->
  case xs of
    (y:ys) | y == x -> Just (y, ys)
    _ -> Nothing

listP :: Eq a => [a] -> Parser a [a]
listP = sequenceA . map eqP

spanP :: (a -> Bool) -> Parser a [a]
spanP f = Parser (Just . span f)

notNull :: Parser a [b] -> Parser a [b]
notNull (Parser p) = Parser $ \input ->
  p input >>= \(a, xs) ->
  guard (not $ null a) *> Just (a, xs)

ws :: Parser Char String
ws = spanP isSpace

nullable :: Parser a b -> Parser a (Maybe b)
nullable (Parser p) = Parser $ \input ->
  case p input of
    Nothing -> Just (Nothing, input)
    Just (a, xs) -> Just (Just a, xs)

sepBy :: Parser a b -> Parser a s -> Parser a [b]
sepBy element sep = ((:) <$> element <*> (many (sep *> element))) <|> pure []

listLit :: Parser Char Expr
listLit = ListLit <$> (eqP '[' *> ws *> elements <* ws <* eqP ']')
  where
    elements :: Parser Char [Expr]
    elements = sepBy expr (ws *> eqP ',' <* ws)

intLit :: Parser Char Expr
intLit = (IntLit . read) <$> (notNull $ spanP isDigit)

stringLit :: Parser Char Expr
stringLit = StringLit <$> (eqP '"' *> (spanP (/= '"')) <* eqP '"')

parens :: Parser Char Expr
parens = eqP '(' *> expr <* eqP ')'

ident :: Parser Char Expr
ident =  Ident <$> (notNull $ spanP isAlpha)

tupleLit :: Parser Char Expr
tupleLit = TupleLit <$> (eqP '(' *> ws *> elements <* ws <* eqP ')')
  where
    tuple :: Parser Char [Expr]
    tuple = ((:) <$> expr <* ws <* eqP ',' <* ws) <*> (sepBy expr (ws *> (eqP ',') <* ws))
    elements :: Parser Char [Expr]
    elements = tuple <|> pure []

expr :: Parser Char Expr
expr = parens <|> boolLit <|> intLit <|> stringLit <|> ident <|> listLit <|> tupleLit

boolLit :: Parser Char Expr
boolLit = trueP <|> falseP
  where
    trueP = (const $ BoolLit True) <$> (listP "true")
    falseP = (const $ BoolLit False) <$> (listP "false")
