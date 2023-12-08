module Main where

import Data.Char (isUpper)
import Data.Map (Map)
import Data.Map qualified as M
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  (directions, island) <- parse <$> readFile "inputs/day8"
  print $ part1 directions island
  print $ part2 directions island

type Location = String

data Direction = L | R
  deriving (Show, Eq, Ord)

parse :: String -> ([Direction], Map (Location, Direction) Location)
parse = fst . last . readP_to_S go
  where
    go = do
      ds <- many1 direction
      skipSpaces
      ms <- concat <$> sepBy entry skipSpaces
      pure $ (ds, M.fromList ms)

    direction = choice [L <$ char 'L', R <$ char 'R']
    location = count 3 (satisfy isUpper)

    entry = do
      from <- location
      _ <- string " = ("
      toL <- location
      _ <- string ", "
      toR <- location
      _ <- string ")"
      skipSpaces
      pure [((from, L), toL), ((from, R), toR)]

walk :: Location -> [Direction] -> Map (Location, Direction) Location -> [Location]
walk from [] _ = [from]
walk from (x : directions) island =
  let next = island M.! (from, x)
   in from : walk next directions island

walkFromTo :: Location -> (Location -> Bool) -> [Direction] -> Map (Location, Direction) Location -> [Location]
walkFromTo from to directions = takeWhile (not . to) . walk from (cycle directions)

part1 :: [Direction] -> Map (Location, Direction) Location -> Int
part1 directions = length . walkFromTo "AAA" (== "ZZZ") directions

part2 :: [Direction] -> Map (Location, Direction) Location -> Int
part2 directions island =
  let walks = map (\start -> length $ walkFromTo start (endsWith 'Z') directions island) starts
   in foldr1 (\a b -> a * b `div` gcd a b) walks
  where
    endsWith c = (== c) . last
    starts = filter (endsWith 'A') . map fst $ M.keys island
