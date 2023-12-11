module Day3 (day3) where

import qualified Data.Char as C
import Data.Foldable (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Entry
--------------------------------------------------------------------------------

data Entry = Symbol Char | Number Int deriving (Eq, Show)

toSymbol :: Entry -> Maybe Char
toSymbol (Symbol c) = Just c
toSymbol _ = Nothing

toNumber :: Entry -> Maybe Int
toNumber (Number n) = Just n
toNumber _ = Nothing

entryLength :: Entry -> Int
entryLength (Symbol _) = 1
entryLength (Number n) = (+ 1) . truncate . logBase 10 . fromIntegral $ n

entryNeighbors :: Entry -> (Int, Int) -> [(Int, Int)]
entryNeighbors entry (x, y) =
  let length = entryLength entry
      xs = [x - 1 .. x + length]
      ys = [y - 1 .. y + 1]
      points = [(x, y) | x <- xs, y <- ys]
   in filter (\(nx, ny) -> ny /= y || nx < x || nx >= x + length) points

parseEntry :: T.Text -> Maybe Entry
parseEntry text
  | T.head text == '.' = Nothing
  | C.isDigit (T.head text) = Just . Number . read . T.unpack . T.takeWhile C.isDigit $ text
  | otherwise = Just . Symbol . T.head $ text

parseEntries :: T.Text -> [(Int, Entry)]
parseEntries text =
  let go x "" entries = entries
      go x remainder entries =
        let entry = parseEntry remainder
            length = maybe 1 entryLength entry
         in go (x + length) (T.drop length remainder) (((x,) <$> entry) : entries)
   in catMaybes $ go 0 text []

--------------------------------------------------------------------------------
-- Schematic
--------------------------------------------------------------------------------

type Schematic = M.Map (Int, Int) Entry

getSymbol :: Schematic -> (Int, Int) -> Maybe Char
getSymbol schematic target = M.lookup target schematic >>= toSymbol

getNumber :: Schematic -> (Int, Int) -> Maybe ((Int, Int), Int)
getNumber schematic target =
  let inside ((x, y), entry) = target `elem` [(x, y) | x <- [x .. x + entryLength entry - 1]]
      entry = find inside (M.toList schematic)
   in entry >>= (\(coords, entry) -> (coords,) <$> toNumber entry)

parseSchematicLine :: Int -> T.Text -> Schematic
parseSchematicLine y text = M.fromList . map (\(x, entry) -> ((x, y), entry)) $ parseEntries text

parseSchematic :: T.Text -> Schematic
parseSchematic = mconcat . zipWith parseSchematicLine [0 ..] . filter (not . T.null) . T.lines

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

type Input = Schematic

parse :: T.Text -> Input
parse = parseSchematic

toPartNumber :: Input -> ((Int, Int), Entry) -> Maybe Int
toPartNumber input (coords, entry) =
  if any (isJust . getSymbol input) (entryNeighbors entry coords)
    then toNumber entry
    else Nothing

part1 :: Input -> Int
part1 input = sum . mapMaybe (toPartNumber input) . M.toList $ input

findEntry :: Input -> (Int, Int) -> Maybe ((Int, Int), Entry)
findEntry input (x, y) = find (\((ex, ey), entry) -> ey == y && elem y [ex .. ex + entryLength entry]) . M.toList $ input

toGear :: Input -> ((Int, Int), Entry) -> Maybe Int
toGear input (coords, entry) =
  let neighbors = entryNeighbors entry coords
      numbers = S.toList . S.fromList . mapMaybe (getNumber input) $ neighbors
   in if length numbers == 2
        then let [a, b] = numbers in Just (snd a * snd b)
        else Nothing

part2 :: Input -> Int
part2 input =
  let stars = filter ((==) (Symbol '*') . snd) (M.toList input)
   in sum . mapMaybe (toGear input) $ stars

day3 = (parse, part1, part2)
