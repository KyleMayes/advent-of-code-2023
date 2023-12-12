module Day9 (day9) where

import qualified Data.List as L
import qualified Data.Text as T

type Input = [[Int]]

parse :: T.Text -> Input
parse = map (map (read . T.unpack) . T.words) . filter (not . T.null) . T.lines

deltas :: [Int] -> [Int]
deltas values =
  let go [_] values = values
      go (a : b : rest) values = go (b : rest) ((b - a) : values)
   in reverse $ go values []

levels :: [Int] -> [[Int]]
levels values =
  let go values levels
        | null values = levels
        | all (0 ==) values = (values : levels)
        | otherwise = go (deltas values) (values : levels)
   in go values []

placeholders1 :: [[Int]] -> [[Int]]
placeholders1 levels =
  let go [a] prev levels = ((a ++ [last a + prev]) : levels)
      go (a : b : rest) prev levels = let v = last a + prev in go (b : rest) v ((a ++ [v]) : levels)
   in go levels 0 []

placeholders2 :: [[Int]] -> [[Int]]
placeholders2 levels =
  let go [a] prev levels = (((head a - prev) : a) : levels)
      go (a : b : rest) prev levels = let v = head a - prev in go (b : rest) v ((v : a) : levels)
   in go levels 0 []

part1 :: Input -> Int
part1 = sum . map (last . head . placeholders1 . levels)

part2 :: Input -> Int
part2 = sum . map (head . head . placeholders2 . levels)

day9 = (parse, part1, part2)
