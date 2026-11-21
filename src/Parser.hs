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
