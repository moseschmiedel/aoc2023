import Data.Char (isDigit, digitToInt)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>), liftA2)

parseDigit :: String -> Maybe Int
parseDigit str 
  | str == "1" = Just 1
  | str == "one" = Just 1
  | str == "2" = Just 2
  | str == "two" = Just 2
  | str == "3" = Just 3
  | str == "three" = Just 3
  | str == "4" = Just 4
  | str == "four" = Just 4
  | str == "5" = Just 5
  | str == "five" = Just 5
  | str == "6" = Just 6
  | str == "six" = Just 6
  | str == "7" = Just 7
  | str == "seven" = Just 7
  | str == "8" = Just 8
  | str == "eight" = Just 8
  | str == "9" = Just 9
  | str == "nine" = Just 9
  | otherwise = Nothing

findDigit :: String -> Maybe Int
findDigit str =
  (parseDigit $ [head str])
  <|> (parseDigit $ take 3 str)
  <|> (parseDigit $ take 4 str)
  <|> (parseDigit $ take 5 str)
  <|> (findDigit $ tail str)

findDigitRev :: String -> Maybe Int
findDigitRev [] = Nothing
findDigitRev str =
  (parseDigit $ reverse $ [head str])
  <|> (parseDigit $ reverse $ take 3 str)
  <|> (parseDigit $ reverse $ take 4 str)
  <|> (parseDigit $ reverse $ take 5 str)
  <|> (findDigitRev $ tail str)
  
getCoord :: String -> Int
getCoord line = fromMaybe 0 (liftA2 (+) (findDigit line <&> (* 10)) (findDigitRev $ reverse line))

listCoords :: String -> [Int]
listCoords fileContent = (lines fileContent) <&> getCoord

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ listCoords input
  print $ sum $ listCoords input
