module Main where

import Data.List (group, intercalate)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day12"
  print $ part1 input
  print $ part2

part1 :: [([Maybe Cell], [Int])] -> Int
part1 = sum . map (\(cs, vs) -> length . filter (valid vs) . assignments $ cs)

part2 :: String
part2 = "I refuse to do memoization again"

inflate :: ([Maybe Cell], [Int]) -> ([Maybe Cell], [Int])
inflate (ss, gs) = (intercalate [Nothing] $ replicate 5 ss, concat $ replicate 5 gs)

parse :: String -> [([Maybe Cell], [Int])]
parse = map go . lines
  where
    go x =
      let
        (ss, gs) = span (/= ' ') x
        springs = map spring ss
        groups = read . ('[' :) . (<> "]") . drop 1 $ gs
      in (springs, groups)

    spring '.' = Just Operational
    spring '#' = Just Damaged
    spring _ = Nothing

data Cell = Operational | Damaged
  deriving (Eq, Show)

assignments :: [Maybe Cell] -> [[Cell]]
assignments [] = [[]]
assignments (Just known : xs) = (known :) <$> assignments xs
assignments (Nothing : xs) = (:) <$> [Operational, Damaged] <*> assignments xs

valid :: [Int] -> [Cell] -> Bool
valid vs = (vs ==) . map length . filter ((== Damaged) . head) . group
