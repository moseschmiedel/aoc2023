module Parser
  ( Parser
  , Error
  , parse
  , satisfy
  , many
  , int
  , digit
  , char
  , string
  , newline
  , space
  )
where

import Data.Char (isDigit, digitToInt)
import Data.Functor ((<&>))
import Data.List (nub, singleton)
import Control.Applicative (Alternative, empty, (<|>))

data Error e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected Char  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser e a =
  Parser { runParser :: [Char] -> Either ([Error e], [Char]) (a, [Char])
         }

parse :: Parser e a -> [Char] -> Either ([Error e], [Char]) a
parse p input = do
  (output,_) <- runParser p input
  return output

instance Functor (Parser e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)

instance Applicative (Parser e) where
  pure a = Parser $ \input -> Right (a, input)
  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')


instance Monad (Parser e) where
  return = pure
  Parser p >>= f = Parser $ \input -> do
    (output, rest) <- p input
    runParser (f output) rest

instance Eq e => Alternative (Parser e) where
  empty = Parser $ \input -> Left ([Empty], input)
  Parser fa <|> Parser fb = Parser $ \input ->
    case fa input of
      Left (errsA, restA) -> case fb input of
        Left (errB, restB) -> Left (nub (errsA <> errB), restA)
        Right (ob, rb) -> Right (ob, rb)
      Right (oa, ra) -> Right (oa, ra)

satisfy :: (Char -> Bool) -> Parser e Char
satisfy p = Parser $ \input -> 
  case input of
    [] -> Left ([EndOfInput], input)
    (i:is)
      | p i -> Right (i, is)
      | otherwise -> Left ([Unexpected i], is)

many :: Eq e => Parser e a -> Parser e [a]
many p = do
  x <- p
  xs <- many p <|> return []
  return (x:xs)

char :: Char -> Parser e Char
char c = satisfy (== c)

space :: Eq e => Parser e [Char]
space = (many $ (char ' ' <|> char '\t')) <|> pure ""

newline :: Parser e Char
newline = char '\n'

string :: [Char] -> Parser e [Char]
string [] = return []
string (x:xs) = do
  y <- char x
  ys <- string xs
  return (y:ys)

digit :: Parser e Int
digit = satisfy isDigit <&> digitToInt

int :: (Eq e) => Parser e Int
int = pure (foldl ((+) . (* 10)) 0) <*> many digit
