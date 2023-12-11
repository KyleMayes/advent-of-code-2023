module Day4 (day4) where

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace (trace)

type Card = (S.Set Int, [Int])

parseCard :: T.Text -> Card
parseCard text =
  let [a, b] = T.splitOn " | " text
      numbers = map read . words . T.unpack
   in (S.fromList . numbers $ a, numbers b)

type Input = [(Int, Card)]

parseLine :: T.Text -> (Int, Card)
parseLine text =
  let [a, b] = T.splitOn ": " text
      index = read . T.unpack . T.drop 5 $ a
   in (index, parseCard b)

parse :: T.Text -> Input
parse = map parseLine . filter (not . T.null) . T.lines

cardWinners :: Card -> Int
cardWinners (winning, numbers) = length . filter (`elem` winning) $ numbers

cardScore :: Card -> Int
cardScore card =
  let winners = cardWinners card
   in if winners == 0 then 0 else 2 ^ (winners - 1)

part1 :: Input -> Int
part1 = sum . map (cardScore . snd)

type Counts = M.Map Int Int

mergeCounts :: Counts -> (Int, Int) -> Counts
mergeCounts counts (number, winners) =
  let count = fromMaybe 1 . M.lookup number $ counts
      won = M.fromList . map (,count) $ [number + 1 .. number + winners]
   in M.unionWith (+) counts won

part2 :: Input -> Int
part2 input =
  let max = maximum . map fst $ input
      initial = M.fromList . map ((,1) . fst) $ input
      counts = foldl (\acc card -> mergeCounts acc (second cardWinners card)) initial input
   in sum . map snd . filter ((<= max) . fst) . M.toList $ counts

day4 = (parse, part1, part2)
