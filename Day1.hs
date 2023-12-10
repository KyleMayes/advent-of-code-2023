module Day1 (day1) where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T

type Input = [T.Text]

parse :: T.Text -> Input
parse = filter (not . T.null) . T.lines

digits :: [(T.Text, Int)]
digits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

getDigitPrefix :: Bool -> T.Text -> Maybe Int
getDigitPrefix names text
  | isDigit . T.head $ text = Just . digitToInt . T.head $ text
  | names = snd <$> L.find ((`T.isPrefixOf` text) . fst) digits
  | otherwise = Nothing

getSuffixes :: T.Text -> [T.Text]
getSuffixes text =
  let go "" suffixes = suffixes
      go text suffixes = go (T.drop 1 text) (text : suffixes)
   in go text []

getDigits :: Bool -> T.Text -> [Int]
getDigits names = reverse . mapMaybe (getDigitPrefix names) . getSuffixes

getCalibration :: Bool -> T.Text -> Int
getCalibration names text =
  let digits = getDigits names text
   in read $ (show . head $ digits) ++ (show . last $ digits)

getCalibrationSum :: Bool -> Input -> Int
getCalibrationSum names = sum . map (getCalibration names)

part1 :: Input -> Int
part1 = getCalibrationSum False

part2 :: Input -> Int
part2 = getCalibrationSum True

day1 = (parse, part1, part2)
