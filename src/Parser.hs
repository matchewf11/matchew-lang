module Parser where

-- https://serokell.io/blog/parser-combinators-in-haskell

import Control.Applicative
import Data.Char
import Data.List
import Data.List.NonEmpty (NonEmpty(..))

data Error input
  = EndOfInput
  | Unexpected input
  | Empty
  deriving (Show, Eq)

newtype Parser input output = Parser
  { runParser :: [input] -> Either [Error input] (output, [input])
  }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser (fmap (\(a, xs) -> (f a, xs)) . p)

instance Applicative (Parser i) where
  pure c = Parser (Right . (,) c)
  (Parser p1) <*> (Parser p2) = Parser $ \input ->
    p1 input >>= \(f, input') ->
    (\(a, input'') -> (f a, input'')) <$> (p2 input')

instance Monad (Parser i) where
  return = pure
  Parser p >>= k = Parser $ \input ->
    (p input) >>= \(output, rest) ->
    runParser (k output) rest

instance (Eq i) => Alternative (Parser i) where
  empty = Parser $ const (Left [Empty])
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    case p1 input of
      Right (output, rest) -> Right (output, rest)
      Left err ->
        case p2 input of
          Right (output, rest) -> Right (output, rest)
          Left err' -> Left $ nub $ err <> err'

eqP :: Eq a => a -> Parser a a
eqP x = Parser $ \xs ->
  case xs of
    [] -> Left [EndOfInput]
    (y:ys) | y == x -> Right (y, ys)
    (y:_) -> Left [Unexpected y]

listP :: Eq a => [a] -> Parser a [a]
listP = sequenceA . map eqP

spanP :: (a -> Bool) -> Parser a [a]
spanP f = Parser (Right . span f)

notNull :: Parser a [b] -> Parser a (NonEmpty b)
notNull (Parser p) = Parser $ \input ->
  case p input of
    Right ([], _) -> Left [Empty]
    Left err -> Left err
    Right (x:xs, rhs) -> Right (x :| xs, rhs)

ws :: Parser Char String
ws = spanP isSpace

sepBy :: Eq a => Parser a b -> Parser a s -> Parser a [b]
sepBy element sep = ((:) <$> element <*> (many (sep *> element))) <|> pure []
