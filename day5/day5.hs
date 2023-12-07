import Prelude hiding (lookup)
import Parser (Parser, parse, int, char, string, many, space, chomp, newline)
import Data.Maybe (fromJust, mapMaybe, fromMaybe, catMaybes)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Control.Parallel (par)
import Control.Parallel.Strategies (parMap, rdeepseq, parListChunk, using)
import Control.Monad (ap, Functor, (>=>))

-- MapRange source dest length  
data MapRange = MapRange Integer Integer Integer deriving (Show, Eq)

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

(%%>) :: MapRange -> Range -> [Range]
(%%>) (MapRange source dest len) (Range s e) =
  maybe
  [destToRange mr]
  (\(MapRange is id ilen) ->
    catMaybes [ mkRange s (is - 1)
              , Just (destToRange $ MapRange is id ilen)
              , mkRange (is + ilen) e
              ])
  (rIntMR r mr)
  where
    mr = MapRange source dest len
    r = Range s e


-- Range start length (inclusive)
data Range = Range Integer Integer deriving (Show, Eq)

mkRange :: Integer -> Integer -> Maybe Range
mkRange s e = if s <= e then Just $ Range s e else Nothing

intersect :: Range -> Range -> Maybe Range
intersect (Range sa ea) (Range sb eb) = mkRange (max sa sb) (min ea eb)

minR :: Range -> Range -> Range
minR (Range sa ea) (Range sb eb) =
  if sa < sb
  then Range sa ea
  else if sb < sa
  then Range sb eb
  else if ea < eb
  then Range sa ea
  else Range sb eb

minimumR :: [Range] -> Range
minimumR = foldl minR


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
  len <- int
  return $ MapRange (toInteger source) (toInteger dest) (toInteger len)


mapParser :: Eq e => String -> Parser e (Integer -> Integer)
mapParser identifier = do
  chomp $ string identifier
  space
  newline
  mrs <- many (maprange <* newline)
  return $ \k ->
    maybe k (%> k) (find (inMR k) mrs)

seedSoilParser :: Parser () (Integer -> Integer)
seedSoilParser = mapParser "seed-to-soil map:"

soilFertParser :: Parser () (Integer -> Integer)
soilFertParser = mapParser "soil-to-fertilizer map:"

fertWaterParser :: Parser () (Integer -> Integer)
fertWaterParser = mapParser "fertilizer-to-water map:"

waterLightParser :: Parser () (Integer -> Integer)
waterLightParser = mapParser "water-to-light map:"

lightTempParser :: Parser () (Integer -> Integer)
lightTempParser = mapParser "light-to-temperature map:"

tempHumidParser :: Parser () (Integer -> Integer)
tempHumidParser = mapParser "temperature-to-humidity map:"

humidLocParser :: Parser () (Integer -> Integer)
humidLocParser = mapParser "humidity-to-location map:"

resolveMapping :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer) -> Int -> Integer
resolveMapping seedSoil soilFert fertWater waterLight lightTemp tempHumid humidLoc =
  humidLoc . tempHumid . lightTemp . waterLight . fertWater . soilFert . seedSoil . toInteger

main :: IO ()
main  = do
  input <- readFile "day5/input.txt"
  print $ do
    seeds <- parse seedParser input
    seedSoil <- parse seedSoilParser input
    soilFert <- parse soilFertParser input
    fertWater <- parse fertWaterParser input
    waterLight <- parse waterLightParser input
    lightTemp <- parse lightTempParser input
    tempHumid <- parse tempHumidParser input
    humidLoc <- parse humidLocParser input
    let locs =  map (resolveMapping seedSoil soilFert fertWater waterLight lightTemp tempHumid humidLoc) seeds
    return $ minimum locs
  print $ do
    seeds <- parse seedRangeParser input
    seedSoil <- parse seedSoilParser input
    soilFert <- parse soilFertParser input
    fertWater <- parse fertWaterParser input
    waterLight <- parse waterLightParser input
    lightTemp <- parse lightTempParser input
    tempHumid <- parse tempHumidParser input
    humidLoc <- parse humidLocParser input
    let locs =  map (\sr -> seedSoil %%> sr >=> (%%>) soilFert >=> (%%>) fertWater >=> (%%>) waterLight >=> (%%>) lightTemp >=> (%%>) tempHumid >=> (%%>) humidLoc) seeds
    return $ minimumR locs

