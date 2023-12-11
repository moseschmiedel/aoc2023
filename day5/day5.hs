import Prelude hiding (lookup)
import Parser (Parser, parse, int, char, string, many, space, chomp, newline)
import Data.Maybe (fromJust, mapMaybe, fromMaybe, catMaybes)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, nub, singleton)
import Control.Parallel (par)
import Control.Parallel.Strategies (parMap, rdeepseq, parListChunk, using)
import Control.Monad (ap, Functor, (>=>))
import Data.Traversable (sequence)

-- MapRange source dest length  
data MapRange = MapRange Integer Integer Integer deriving (Show, Eq)

-- Range start end (inclusive)
data Range = Range Integer Integer deriving (Show, Eq)

mkRange :: Integer -> Integer -> Maybe Range
mkRange s e = if s <= e then Just $ Range s e else Nothing

intersect :: Range -> Range -> Maybe Range
intersect (Range sa ea) (Range sb eb) = mkRange (max sa sb) (min ea eb)

inMR :: Integer -> MapRange -> Bool
inMR k (MapRange source dest len) = source <= k && k < source + len

sourceToRange :: MapRange -> Range
sourceToRange (MapRange source _ len) = Range source (source+len-1)
destToRange :: MapRange -> Range
destToRange (MapRange _ dest len) = Range dest (dest+len-1)

rIntMR :: Range -> MapRange -> Maybe MapRange
rIntMR range (MapRange source dest len) =
  let
    mr = MapRange source dest len
  in
    intersect range (sourceToRange mr) <&>
      \(Range s e) ->
        MapRange s (dest+(s-source)) (e-s+1)

(%>) :: MapRange -> Integer -> Integer
(%>) (MapRange source dest _) k = dest + (k - source)

(%%>) :: MapRange -> Range -> (Maybe Range, [Range])
(%%>) (MapRange source dest len) (Range s e) =
  maybe
  (Nothing, [Range s e])
  (\(MapRange is id ilen) ->
     ( Just (destToRange $ MapRange is id ilen)
     , catMaybes [ mkRange s (is - 1)
                 , mkRange (is + ilen) e
                 ]
     ))
  (rIntMR (Range s e) (MapRange source dest len))

runRec :: [Range] -> [Range] -> [MapRange] -> [Range]
runRec [] resolved _ = resolved
runRec rgs resolved [] = nub $ resolved ++ rgs
runRec rgs resolved (mr:mrs) =
  let
    transformed = map (mr %%>) rgs
    resolved' = mapMaybe fst transformed ++ resolved
    rest = nub $ concatMap snd transformed
  in runRec rest resolved' mrs

run :: [MapRange] -> [Range] -> [Range]
run mrs rgs = runRec rgs [] mrs

minR :: Range -> Range -> Range
minR (Range sa ea) (Range sb eb)
  | sa < sb = Range sa ea
  | sb < sa = Range sb eb
  | ea < eb = Range sa ea
  | otherwise = Range sb eb

minimumR :: [Range] -> Maybe Range
minimumR [] = Nothing
minimumR (rg:rgs) = Just $ foldl minR rg rgs

seedParser :: Parser () [Int]
seedParser = do
  chomp $ string "seeds:"
  many (space *> int)

seedRangeParser :: Parser () [Range]
seedRangeParser = do
  chomp $ string "seeds:"
  many (
    do
      space
      start <- int <&> toInteger
      space
      len <- int <&> toInteger
      return $ Range start (start+len))

maprange :: Eq e => Parser e MapRange
maprange = do
  space
  dest <- int
  space
  source <- int
  space
  int <&> MapRange (toInteger source) (toInteger dest) . toInteger

mapParser :: Eq e => String -> Parser e [MapRange]
mapParser identifier = do
  chomp $ string identifier
  space
  newline
  many (maprange <* newline)

seedSoilParser :: Parser () [MapRange]
seedSoilParser = mapParser "seed-to-soil map:"

soilFertParser :: Parser () [MapRange]
soilFertParser = mapParser "soil-to-fertilizer map:"

fertWaterParser :: Parser () [MapRange]
fertWaterParser = mapParser "fertilizer-to-water map:"

waterLightParser :: Parser () [MapRange]
waterLightParser = mapParser "water-to-light map:"

lightTempParser :: Parser () [MapRange]
lightTempParser = mapParser "light-to-temperature map:"

tempHumidParser :: Parser () [MapRange]
tempHumidParser = mapParser "temperature-to-humidity map:"

humidLocParser :: Parser () [MapRange]
humidLocParser = mapParser "humidity-to-location map:"

main :: IO ()
main  = do
  input <- readFile "day5/input.txt"
  print $ do
    seeds <- parse (seedParser <&> map ((\int -> Range int int) . toInteger)) input
    seedSoil <- parse seedSoilParser input
    soilFert <- parse soilFertParser input
    fertWater <- parse fertWaterParser input
    waterLight <- parse waterLightParser input
    lightTemp <- parse lightTempParser input
    tempHumid <- parse tempHumidParser input
    humidLoc <- parse humidLocParser input
    let locs =  (run humidLoc . run tempHumid . run lightTemp . run waterLight . run fertWater . run soilFert . run seedSoil) seeds
    return $ minimumR locs
  print $ do
    seeds <- parse seedRangeParser input
    seedSoil <- parse seedSoilParser input
    soilFert <- parse soilFertParser input
    fertWater <- parse fertWaterParser input
    waterLight <- parse waterLightParser input
    lightTemp <- parse lightTempParser input
    tempHumid <- parse tempHumidParser input
    humidLoc <- parse humidLocParser input
    let locs =  (run humidLoc . run tempHumid . run lightTemp . run waterLight . run fertWater . run soilFert . run seedSoil) seeds
    return $ minimumR locs

