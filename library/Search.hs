module Search (binarySearch, binarySearchBy) where

import qualified Data.Vector as V

binarySearch :: (Ord a) => V.Vector a -> a -> Either Int Int
binarySearch vector item = binarySearchBy vector (`compare` item)

binarySearchBy :: (Ord a) => V.Vector a -> (a -> Ordering) -> Either Int Int
binarySearchBy vector f = go 0 (V.length vector)
  where
    go left right
      | left >= right = Left left
      | otherwise =
          let middle = left + ((right - left) `div` 2)
              ordering = f . (V.!) vector $ middle
           in case ordering of
                LT -> go left middle
                GT -> go (middle + 1) right
                EQ -> Right middle
