module Solution
  ( Input (..),
    loadInput,
    Parser,
    Solver,
    Solution (..),
    Display (..),
    executeSolution,
  )
where

import qualified Data.Text as T

data Input = FileInput T.Text | StringInput T.Text

loadInput :: Input -> IO T.Text
loadInput (FileInput file) = T.pack <$> readFile ("inputs/" ++ T.unpack file)
loadInput (StringInput string) = pure string

type Parser a = T.Text -> a

type Solver a b = a -> b

type Solution a b c = (Parser a, Solver a b, Solver a c)

class Display a where
  display :: a -> String

instance Display Int where
  display = show

instance Display String where
  display = id

instance Display T.Text where
  display = T.unpack

executeSolution :: forall a b c. (Display b, Display c) => Solution a b c -> Input -> IO ()
executeSolution (parse, part1, part2) input = do
  input <- parse <$> loadInput input
  putStrLn $ concat $ replicate 40 "─"
  putStrLn $ "Part 1: " ++ (display . part1 $ input)
  putStrLn $ "Part 2: " ++ (display . part2 $ input)
