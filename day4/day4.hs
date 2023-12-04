import Parser (Parser, char, string, space, newline, int, many, parse)
import Control.Applicative ((*>))
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersect)
import Data.Map (Map, (!), fromList)

data CardID = CardID Integer deriving (Show, Eq, Ord)
data Card = Card CardID [Integer] [Integer] deriving (Show, Eq)

cardid :: Card -> CardID
cardid (Card cid _ _) = cid

cardidParser :: Eq e => Parser e CardID
cardidParser = int <&> toInteger <&> CardID

card :: Parser () Card
card = do
  string "Card"
  space
  cid <- cardidParser
  char ':'
  winning <- many (space *> int <&> toInteger)
  space
  char '|'
  given <- many (space *> int <&> toInteger)
  return $ Card cid winning given

cards :: Parser () [Card]
cards = many (card <* newline)


score :: Card -> Int
score card = case matching card of
  0 -> 0
  len -> 2 ^ (len-1)

matching :: Card -> Int
matching (Card _ winning given) = length $ intersect winning given

clone :: Int -> Integer -> [(Card, Integer)] -> [(Card, Integer)]
clone numcards amount cards =
  let
    needcloning = take numcards cards
    cloned = map (\(card, camount) -> (card, camount + amount)) needcloning
  in
    cloned ++ drop numcards cards

play :: [(Card, Integer)] -> [(Card, Integer)]
play (c:[]) = [c]
play (c:cs) =
  c : (play $ clone (matching . fst $ c) (snd c) cs)

main :: IO ()
main = do
  input <- readFile "day4/input.txt"
  print $ parse cards input <&> map score <&> sum
  print $ parse cards input <&> (\cs ->
    play (map (\c -> (c, 1)) cs))
    <&> map snd
    <&> sum
    
