module Day8 (day8) where

import Control.Applicative (ZipList (ZipList))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T

type Direction = Char

type Node = T.Text

data Input = Input {inputTurns :: [Direction], inputEntries :: M.Map Node (Node, Node)}

parseEntry :: T.Text -> (Node, (Node, Node))
parseEntry text =
  let [start, directions] = T.splitOn " = " text
      [left, right] = T.splitOn ", " directions
   in (start, (T.drop 1 left, T.take 3 right))

parse :: T.Text -> Input
parse text =
  let [turns, lines] = T.splitOn "\n\n" text
      entries = M.fromList . map parseEntry . T.lines $ lines
   in Input (T.unpack turns) entries

follow :: Node -> Direction -> Input -> Node
follow node direction (Input _ entries) =
  let (left, right) = fromJust . M.lookup node $ entries
   in if direction == 'L' then left else right

trek :: Node -> Input -> [Node]
trek node input =
  let next (node, turn : turns) = (follow node turn input, turns)
   in map fst . iterate next $ (node, cycle . inputTurns $ input)

part1 :: Input -> Int
part1 = length . takeWhile ("ZZZ" /=) . trek "AAA"

part2 :: Input -> Int
part2 input =
  let starts = filter ("A" `T.isSuffixOf`) . M.keys . inputEntries $ input
      treks = map (`trek` input) starts
      lengths = map (length . takeWhile (not . ("Z" `T.isSuffixOf`))) treks
   in foldl lcm 1 lengths

day8 = (parse, part1, part2)
