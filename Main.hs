{-# LANGUAGE OverloadedStrings #-}

module Main where

import Day1 (day1)
import Solution (Input (..), executeSolution)
import System.Environment (getArgs)

main :: IO ()
main = do
  day <- head <$> getArgs
  case day of
    "1" -> executeSolution day1 (FileInput "day1.txt")
    _ -> error $ "Invalid day: " ++ day
