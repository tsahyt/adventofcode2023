{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Algorithm.Search
import Data.Map (Map)
import Data.Map qualified as M

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day17"
  -- print $ part1 input
  print $ part2 input

type Pos = (Int, Int)

data Direction = North | East | South | West
  deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

parse :: String -> Map Pos Int
parse =
  M.fromList .
  concatMap (\(i, s) -> map (\(j, c) -> ((i, j), read . pure $ c)) . zip [0..] $ s) .
  zip [0..] . lines

defaultRules :: Map Pos Int -> Pos -> Pos -> Maybe _
defaultRules m from to = dijkstra neighbors cost ((== to) . fst) (from, [])
  where
    neighbors ((i, j), hist) = filter (uncurry constraints)
      [ ((i - 1, j), North : take 3 hist), ((i + 1, j), South : take 3 hist)
      , ((i, j - 1), West : take 3 hist), ((i, j + 1), East : take 3 hist) ]
    constraints pos hist =
      pos `M.member` m
      && (length hist == 4 ==> not (allEqual hist))
      && (length hist > 2 ==> hist !! 0 /= opposite (hist !! 1))
    cost _ (pos, _) = m M.! pos

ultraRules :: Map Pos Int -> Pos -> Pos -> Maybe _
ultraRules m from to = dijkstra neighbors cost ((== to) . fst) (from, [])
  where
    neighbors ((i, j), hist) = filter (uncurry constraints)
      [ ((i - 1, j), North : take 10 hist), ((i + 1, j), South : take 10 hist)
      , ((i, j - 1), West : take 10 hist), ((i, j + 1), East : take 10 hist) ]
    constraints pos hist =
      pos `M.member` m
      && (length hist == 11 ==> not (allEqual hist))
      && (length hist > 1 ==> (hist !! 0 /= hist !! 1 ==> allEqual (take 4 . drop 1 $ hist)))
      && (length hist > 2 ==> hist !! 0 /= opposite (hist !! 1))
    cost _ (pos, _) = m M.! pos

infixl 3 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

allEqual :: Eq a => [a] -> Bool
allEqual xs = and $ map (== head xs) (tail xs)

part1 :: Map Pos Int -> Maybe Int
part1 m = fmap fst $ defaultRules m (0,0) (maximum . M.keys $ m)

part2 :: Map Pos Int -> Maybe Int
part2 m = fmap fst $ ultraRules m (0,0) (maximum . M.keys $ m)
