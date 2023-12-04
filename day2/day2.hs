import Parser (Parser, int, many, string, space, newline, parse, char)
import Data.Functor ((<&>))
import Data.List (singleton)
import Control.Applicative ((<|>))

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
