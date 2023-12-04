import Data.Char (isDigit, digitToInt)
import Data.Functor ((<&>))
import Data.List (nub, singleton)
import Control.Monad (mfilter)
import Control.Applicative (Alternative, empty, (<|>))

data Color = Red | Green | Blue deriving (Show, Eq)
  
data Cubes = Cubes { amount :: Int
                   , col :: Color
                   } deriving (Show, Eq)

data CubeSet = CubeSet [Cubes] deriving (Show, Eq)

data GameConfig = GameConfig { getRed :: Int
                             , getGreen :: Int
                             , getBlue :: Int
                             } deriving (Show, Eq)

data Game = Game { gameid :: Int
                 , cs :: CubeSet
                 } deriving (Show, Eq)

data Error e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected Char  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

newtype Parser e a =
  Parser { runParser :: [Char] -> Either ([Error e], [Char]) (a, [Char])
         }

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

char :: Char -> Parser e Char
char c = satisfy (== c)

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

many :: Eq e => Parser e a -> Parser e [a]
many p = do
  x <- p
  xs <- many p <|> return []
  return (x:xs)

space :: Eq e => Parser e [Char]
space = (many $ (char ' ' <|> char '\t')) <|> pure ""

int :: (Eq e) => Parser e Int
int = pure (foldl ((+) . (* 10)) 0) <*> many digit

red :: Parser e Color
red = string "red" >> return Red
green :: Parser e Color
green = string "green" >> return Green
blue :: Parser e Color
blue = string "blue" >> return Blue
  
color :: Eq e => Parser e Color
color = red <|> green <|> blue

cubes :: Eq e => Parser e Cubes
cubes = do
  amount <- int
  char ' '
  col <- color
  return Cubes { amount = amount
               , col = col }

cubeset :: Eq e => Parser e CubeSet
cubeset = many (
  do
    cs <- cubes
    string "," <|> string ";"
    space
    return cs
  ) >>= return . (++)
  >>= \x -> pure x <*> (fmap singleton cubes)
  >>= return . CubeSet

game :: Eq e => Parser e Game
game = do
  space
  string "Game"
  space
  gameid <- int
  char ':'
  space
  cs <- cubeset
  return Game { gameid = gameid
              , cs = cs
              }
parse :: Parser e a -> [Char] -> Either ([Error e], [Char]) a
parse p input = do
  (output,_) <- runParser p input
  return output

gameconfig :: GameConfig
gameconfig = GameConfig { getRed = 12
                        , getGreen = 13
                        , getBlue = 14
                        }

possible :: GameConfig -> Game -> Bool
possible gc g =
  let (CubeSet cubes) = cs g
  in all (\(Cubes {amount, col}) -> case col of
       Red -> amount <= getRed gc
       Green -> amount <= getGreen gc
       Blue -> amount <= getBlue gc) cubes

games :: Parser () [Game]
games = many $ game <* space <* newline

possiblegames :: GameConfig -> Parser () [Game]
possiblegames gc = games >>= return . (filter $ possible gameconfig)

minimumcubes :: Game -> GameConfig
minimumcubes (Game _ (CubeSet cubes)) =
  foldl (\gc cs ->
           case (col cs) of
             Red -> GameConfig (max (amount cs) (getRed gc)) (getGreen gc) (getBlue gc)
             Green -> GameConfig (getRed gc) (max (amount cs) (getGreen gc)) (getBlue gc)
             Blue -> GameConfig (getRed gc) (getGreen gc) (max (amount cs) (getBlue gc))
        ) (GameConfig 0 0 0) cubes

powercubes :: GameConfig -> Int
powercubes gc = getRed gc * getGreen gc * getBlue gc
  

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ (parse (possiblegames gameconfig) input) <&> map gameid <&> sum
  print $ (parse games input) <&> map minimumcubes <&> map powercubes <&> sum
