module Day7 where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (cons, length)
import Data.Foldable (sum, foldr)
import Data.Int (ceil, floor, toNumber)
import Effect (Effect)
import Effect.Console (logShow)
import Math (abs)
import Parsers (parseInt, parseFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (many1)
import Text.Parsing.Parser.String (char, noneOf, eof)

partOneSampleSolution :: Effect Unit
partOneSampleSolution =
  parseFile "src/Day7/sample_input.txt" inputParser
    >>= (map partOne >>> logShow)

partOneSolution :: Effect Unit
partOneSolution =
  parseFile "src/Day7/input.txt" inputParser
    >>= (map partOne >>> logShow)

partTwoSampleSolution :: Effect Unit
partTwoSampleSolution =
  parseFile "src/Day7/sample_input.txt" inputParser
    >>= (map partTwo >>> logShow)

partTwoSolution :: Effect Unit
partTwoSolution =
  parseFile "src/Day7/input.txt" inputParser
    >>= (map partTwo >>> logShow)

partOne :: Array Int -> Number
partOne input =
  foldr
    ( \cur champ ->
        if minDistance cur < champ then minDistance cur else champ
    )
    (minDistance $ geoMidpoint $ toNumber <$> input)
    (toNumber <$> input)
  where
  minDistance cur = sum $ toNumber >>> (\a -> abs (cur - a)) <$> input

partTwo :: Array Int -> Int
partTwo input =
  foldr
    ( \cur champ ->
        if minDistance cur < champ then minDistance cur else champ
    )
    geoMin
    input
  where
  geoMin =
    let
      midpoint = geoMidpoint $ toNumber <$> input

      floorMp = minDistance $ floor midpoint

      ceilMp = minDistance $ ceil midpoint
    in
      if floorMp < ceilMp then floorMp else ceilMp

  minDistance :: Int -> Int
  minDistance cur = sum $ (\a -> actualDist <<< absInt $ (cur - a)) <$> input

absInt :: Int -> Int
absInt = toNumber >>> abs >>> ceil

inputParser :: Parser String (Array Int)
inputParser = do
  x <- many1 (noneOf [ ',' ]) >>= parseInt
  xs <- (eof *> pure []) <|> (char ',' *> inputParser)
  pure $ cons x xs

geoMidpoint :: Array Number -> Number
geoMidpoint v = sum v / (toNumber $ length v)

actualDist :: Int -> Int
actualDist = actualDist' 0 0
  where
  actualDist' :: Int -> Int -> Int -> Int
  actualDist' _ acc 0 = acc

  actualDist' cur acc count = actualDist' (cur + 1) (acc + cur + 1) (count - 1)
