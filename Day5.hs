module Day5 (day5) where

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Search (binarySearchBy)

type Range = ((Int, Int), Int)

parseRange :: T.Text -> Range
parseRange text =
  let [dst, src, rng] = map (read . T.unpack) . T.words $ text
   in ((src, src + rng - 1), dst)

compareRange :: Int -> Range -> Ordering
compareRange number ((left, right), _)
  | number < left = LT
  | number > right = GT
  | otherwise = EQ

type Mapping = V.Vector Range

parseMapping :: T.Text -> Mapping
parseMapping = V.fromList . L.sortOn (fst . fst) . map parseRange . drop 1 . T.lines

findNumber :: Int -> Mapping -> Int
findNumber number mapping =
  case binarySearchBy mapping (compareRange number) of
    Left _ -> number
    Right index ->
      let ((src, _), dst) = (V.!) mapping index
       in dst + (number - src)

data Almanac = Almanac {almanacSeeds :: [Int], almanacMappings :: [Mapping]}

type Input = Almanac

parse :: T.Text -> Input
parse text =
  let (head : tail) = T.splitOn "\n\n" text
      seeds = map (read . T.unpack) . T.words . T.drop 7 $ head
      mappings = map parseMapping tail
   in Almanac seeds mappings

findLocation :: [Mapping] -> Int -> Int
findLocation mappings seed = foldl findNumber seed mappings

part1 :: Input -> Int
part1 (Almanac seeds mappings) = minimum . map (findLocation mappings) $ seeds

part2 :: Input -> Int
part2 (Almanac ranges mappings) =
  let seeds = split ranges []
   in minimum . map (findLocation mappings) $ seeds
  where
    split [] acc = acc
    split (a : b : tail) acc = split tail ([a .. a + b - 1] ++ acc)

day5 = (parse, part1, part2)
