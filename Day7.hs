module Day7 (day7) where

import qualified Data.Bifunctor as Bi
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Card
--------------------------------------------------------------------------------

newtype Card = Card Char
  deriving (Eq, Show)

instance Ord Card where
  compare :: Card -> Card -> Ordering
  compare (Card a) (Card b) = compare (rank a) (rank b)
    where
      rank card = case card of
        'A' -> 14
        'K' -> 13
        'Q' -> 12
        'J' -> 11
        'T' -> 10
        'W' -> 1
        _ -> C.ord card - 48

--------------------------------------------------------------------------------
-- Hand
--------------------------------------------------------------------------------

newtype Hand = Hand [Card]
  deriving (Eq, Ord, Show)

parseHand :: T.Text -> Hand
parseHand = Hand . map Card . T.unpack

wildHand :: Hand -> Hand
wildHand (Hand cards) = Hand . map (\c -> if c == Card 'J' then Card 'W' else c) $ cards

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Ord, Show)

evaluateHand :: Hand -> HandType
evaluateHand (Hand cards)
  | first == 5 = FiveOfAKind
  | first == 4 = FourOfAKind
  | first == 3 && second == 2 = FullHouse
  | first == 3 = ThreeOfAKind
  | first == 2 && second == 2 = TwoPair
  | first == 2 = OnePair
  | otherwise = HighCard
  where
    (wild, tame) = L.partition (== Card 'W') cards
    counted = foldl (\acc card -> M.insertWith (+) card 1 acc) M.empty tame
    sorted = L.sortBy (O.comparing O.Down) . map snd . M.toList $ counted
    first = (if null sorted then 0 else head sorted) + length wild
    second = if length sorted > 1 then sorted !! 1 else 0

--------------------------------------------------------------------------------
-- Round
--------------------------------------------------------------------------------

type Round = (Hand, Int)

parseRound :: T.Text -> Round
parseRound text =
  let [hand, wager] = T.words text
   in (parseHand hand, read . T.unpack $ wager)

evaluateRound :: Round -> (HandType, Hand)
evaluateRound (hand, _) = (evaluateHand hand, hand)

--------------------------------------------------------------------------------
-- Solution
--------------------------------------------------------------------------------

type Input = [Round]

parse :: T.Text -> Input
parse = map parseRound . filter (not . T.null) . T.lines

solve :: Input -> Int
solve = sum . zipWith (*) [1 ..] . map snd . L.sortBy (O.comparing evaluateRound)

part1 :: Input -> Int
part1 = solve

part2 :: Input -> Int
part2 = solve . map (Bi.first wildHand)

day7 :: (T.Text -> Input, Input -> Int, Input -> Int)
day7 = (parse, part1, part2)
