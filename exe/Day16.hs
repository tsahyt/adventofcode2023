module Main where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Foldable (toList)
import Data.List (nub)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day16"
  print $ part1 input
  print $ part2 input

type Pos = (Int, Int)

data Direction = North | East | South | West
  deriving (Eq, Ord)

stepOne :: Pos -> Direction -> (Pos, Direction)
stepOne (i, j) North = ((i - 1, j), North)
stepOne (i, j) East = ((i, j + 1), East)
stepOne (i, j) South = ((i + 1, j), South)
stepOne (i, j) West = ((i, j - 1), West)

data Tile
  = Empty
  | SplitVertical
  | SplitHorizontal
  | MirrorForward
  | MirrorBackward
  deriving Show

parse :: String -> Map Pos Tile
parse = M.fromList .
  concatMap (\(i, s) -> map (\(j, c) -> ((i, j), tile c)) . zip [0..] $ s) . zip [0..] . lines
  where
    tile '/' = MirrorForward
    tile '\\' = MirrorBackward
    tile '|' = SplitVertical
    tile '-' = SplitHorizontal
    tile _ = Empty

-- | Generalized breadth first traversal
bft :: (Foldable t, Ord a) => (a -> t a) -> t a -> [a]
bft suc = go S.empty . toList
    where go _ [] = []
          go visited (n:ns)
              | n `S.member` visited = go visited ns
              | otherwise = n : go (n `S.insert` visited)
                                   (ns ++ toList (suc n))

shine :: Pos -> Direction -> Map Pos Tile -> [Pos]
shine sp sd m = nub . map fst . bft suc $ [(sp, sd)]
  where
    suc (pos, dir) =
      filter (flip M.member m . fst) $ case (M.lookup pos m, dir) of
        (Nothing, _) -> []
        (Just Empty, d) -> [stepOne pos d]
        (Just SplitVertical, d)
          | d == North || d == South -> [stepOne pos d]
          | otherwise -> [stepOne pos North, stepOne pos South]
        (Just SplitHorizontal, d)
          | d == East || d == West -> [stepOne pos d]
          | otherwise -> [stepOne pos East, stepOne pos West]
        (Just MirrorForward, North) -> [stepOne pos East]
        (Just MirrorForward, East) -> [stepOne pos North]
        (Just MirrorForward, South) -> [stepOne pos West]
        (Just MirrorForward, West) -> [stepOne pos South]
        (Just MirrorBackward, North) -> [stepOne pos West]
        (Just MirrorBackward, East) -> [stepOne pos South]
        (Just MirrorBackward, South) -> [stepOne pos East]
        (Just MirrorBackward, West) -> [stepOne pos North]

border :: Map Pos Tile -> [(Pos, Direction)]
border m = north <> east <> south <> west
  where
    maxV = maximum . map fst . M.keys $ m
    maxH = maximum . map snd . M.keys $ m

    north = [((0, x), South) | x <- [0 .. maxH]]
    east = [((x, maxH), West) | x <- [0 .. maxV]]
    south = [((maxV, x), North) | x <- [0 .. maxH]]
    west = [((x, 0), East) | x <- [0 .. maxV]]

part1 :: Map Pos Tile -> Int
part1 = length . shine (0,0) East

part2 :: Map Pos Tile -> Int
part2 m = maximum . map (\(sp, sd) -> length $ shine sp sd m) $ border m
