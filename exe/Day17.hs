{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Graph.AStar
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics (Generic)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day17"
  print $ part1 input
  print $ part2 input

type Pos = (Int, Int)

data Direction = North | East | South | West
  deriving (Show, Eq, Ord, Generic)

instance Hashable Direction

opposite :: Direction -> Direction
opposite North = South
opposite East = West
opposite South = North
opposite West = East

data History = History !Int !Direction
  deriving (Eq, Ord, Generic, Show)

instance Hashable History

move :: Direction -> History -> History
move d (History s1 d1)
  | d == d1 = History (s1 + 1) d
  | otherwise = History 1 d

atMostDir :: Int -> History -> Bool
atMostDir l (History s1 _) = s1 <= l

noReverse :: History -> History -> Bool
noReverse (History _ approach) (History _ d1) = d1 /= opposite approach

minMoves :: Int -> History -> History -> Bool
minMoves l (History s2 _) (History s1 _) = not (s1 == 1 && s2 /= 0) || s2 >= l

parse :: String -> Map Pos Int
parse =
  M.fromList
    . concatMap (\(i, s) -> map (\(j, c) -> ((i, j), read . pure $ c)) . zip [0 ..] $ s)
    . zip [0 ..]
    . lines

manhattan :: Pos -> Pos -> Int
manhattan (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

unconstrained :: Map Pos Int -> Map Pos Int
unconstrained m = m'
  where
    maxPos = maximum . M.keys $ m
    m' = M.fromList (map go (M.keys m))

    go :: Pos -> (Pos, Int)
    go pos@(i, j)
      | pos == maxPos = (pos, m M.! pos)
      | otherwise =
        let
          c = m M.! pos
          h = minimum ([ m' M.! pos' | pos' <- [ (i + 1, j), (i, j + 1) ], pos' `M.member` m])
        in (pos, c + h)

crucible :: Int -> Int -> Map Pos Int -> Pos -> Pos -> Maybe Int
crucible minDist maxDist m from to =
  fmap (sum . map ((m M.!) . fst)) $
  aStar neighbors cost heuristic goal (from, History 0 East)
  where
    m' = unconstrained m
    neighbors ((i, j), hist) =
      HS.fromList $
        filter
          (uncurry (constraints hist))
          [ ((i - 1, j), move North hist),
            ((i + 1, j), move South hist),
            ((i, j - 1), move West hist),
            ((i, j + 1), move East hist)
          ]
    heuristic (pos, _) = if minDist > 0 then m' M.! pos else manhattan pos to
    goal (pos, History x _) = x >= minDist && pos == to
    constraints current pos hist =
      pos `M.member` m
        && atMostDir maxDist hist
        && noReverse current hist
        && minMoves minDist current hist
    cost _ (pos, _) = m M.! pos

part1 :: Map Pos Int -> Maybe Int
part1 m = crucible 0 3 m (0, 0) (maximum . M.keys $ m)

part2 :: Map Pos Int -> Maybe Int
part2 m = crucible 4 10 m (0, 0) (maximum . M.keys $ m)
