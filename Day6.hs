module Day6 (day6) where

import qualified Data.Text as T

data Race = Race {raceTime :: Int, raceDistance :: Int} deriving (Show)

type Input = [Race]

parse :: T.Text -> Input
parse text =
  let [times, distances] = T.lines text
      numbers = map (read . T.unpack) . drop 1 . T.words
   in zipWith Race (numbers times) (numbers distances)

raceWins :: Race -> [Int]
raceWins (Race time distance) = filter (> distance) [x * (time - x) | x <- [0 .. time]]

part1 :: Input -> Int
part1 = product . map (length . raceWins)

part2 :: Input -> Int
part2 races =
  let time = read $ concatMap (show . raceTime) races
      distance = read $ concatMap (show . raceDistance) races
   in length . raceWins $ Race time distance

day6 = (parse, part1, part2)
