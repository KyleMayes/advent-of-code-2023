{-# LANGUAGE OverloadedStrings #-}

module Day1 (day1) where

import qualified Data.Text as T

type Input = T.Text

parse :: T.Text -> Input
parse = id

part1 :: Input -> Int
part1 input = 0

part2 :: Input -> Int
part2 input = 0

day1 = (parse, part1, part2)
