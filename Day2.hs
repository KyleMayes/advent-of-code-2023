module Day2 (day2) where

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Text as T

data Sample = Sample {red :: Int, green :: Int, blue :: Int}

instance Semigroup Sample where
  Sample r1 g1 b1 <> Sample r2 g2 b2 = Sample (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Sample where
  mempty = Sample 0 0 0

parseSampleElement :: T.Text -> Sample
parseSampleElement text =
  let (prefix : (color : _)) = T.splitOn " " text
      count = read . T.unpack $ prefix
   in case color of
        "red" -> Sample count 0 0
        "green" -> Sample 0 count 0
        "blue" -> Sample 0 0 count

parseSample :: T.Text -> Sample
parseSample = mconcat . map parseSampleElement . T.splitOn ", "

parseSamples :: T.Text -> [Sample]
parseSamples = map parseSample . T.splitOn "; "

type Game = (Int, [Sample])

parseGame :: T.Text -> Game
parseGame text =
  let (prefix : (samples : _)) = T.splitOn ": " text
   in (read . T.unpack . T.drop 5 $ prefix, parseSamples samples)

type Input = [Game]

parse :: T.Text -> Input
parse = map parseGame . filter (not . T.null) . T.lines

part1 :: Input -> Int
part1 input =
  let check a = red a <= 12 && green a <= 13 && blue a <= 14
   in sum . map fst . filter (all check . snd) $ input

power :: Game -> Int
power (_, samples) =
  let max color = F.maximum . map color $ samples
   in max red * max green * max blue

part2 :: Input -> Int
part2 = sum . map power

day2 = (parse, part1, part2)
