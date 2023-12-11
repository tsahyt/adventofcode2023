{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (guard)
import Data.List (transpose)
import Data.Map (Map, fromList, (!))
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day11"
  print $ part1 input
  print $ part2 input

data Cell = Empty | Galaxy

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

parse :: String -> Universe
parse = map (map cell) . lines
  where
    cell '.' = Empty
    cell _ = Galaxy

type Universe = [[Cell]]

part1 :: Universe -> Int
part1 = shortest 2

part2 :: Universe -> Int
part2 = shortest 1000000

type Pos = (Int, Int)

distance :: Pos -> Pos -> Int
distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

galaxies :: Universe -> [Pos]
galaxies = catMaybes . concat . zipWith line [0 ..]
  where
    line i = zipWith (space i) [0 ..]
    space i j = \case
      Galaxy -> Just (j, i)
      _ -> Nothing

inflate :: Int -> Universe -> Map Int Int
inflate s xs =
  fromList . zip [0 ..] . scanl1 (+) . map size $ xs
  where
    size line
      | all isEmpty line = s
      | otherwise = 1

shortest :: Int -> Universe -> Int
shortest s input =
  let ys = inflate s input
      xs = inflate s (transpose input)
      g = map (\(x, y) -> (xs ! x, ys ! y)) . galaxies $ input
      ds = do
        (i, a) <- zip [1 :: Int ..] g
        (j, b) <- zip [1 ..] g
        guard (i < j)
        pure (distance a b)
   in sum ds
