module Day23 (day23) where

import qualified Data.Text as T

type Input = [T.Text]

parse :: T.Text -> Input
parse = filter (not . T.null) . T.lines

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

day23 = (parse, part1, part2)
