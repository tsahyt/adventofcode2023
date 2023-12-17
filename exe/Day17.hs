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

data History = History !Int !Direction !Int !Direction
  deriving (Eq, Ord, Generic, Show)

instance Hashable History

move :: Direction -> History -> History
move d (History s1 d1 s2 d2)
  | d == d1 = History (s1 + 1) d s2 d2
  | otherwise = History 1 d s1 d1

atMostDir :: Int -> History -> Bool
atMostDir l (History s1 _ s2 _) = s1 <= l && s2 <= l

noReverse :: History -> Bool
noReverse (History _ d1 _ d2) = d1 /= opposite d2

minMoves :: Int -> History -> Bool
minMoves l (History s1 _ s2 _) = (s1 == 1 && s2 /= 0) ==> s2 >= l

parse :: String -> Map Pos Int
parse =
  M.fromList
    . concatMap (\(i, s) -> map (\(j, c) -> ((i, j), read . pure $ c)) . zip [0 ..] $ s)
    . zip [0 ..]
    . lines

manhattan :: Pos -> Pos -> Int
manhattan (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

crucible :: Bool -> Map Pos Int -> Pos -> Pos -> Maybe Int
crucible ultra m from to =
  fmap (sum . map ((m M.!) . fst)) $
  aStar neighbors cost heuristic goal (from, History 0 East 0 East)
  where
    neighbors ((i, j), hist) =
      HS.fromList $
        filter
          (uncurry constraints)
          [ ((i - 1, j), move North hist),
            ((i + 1, j), move South hist),
            ((i, j - 1), move West hist),
            ((i, j + 1), move East hist)
          ]
    heuristic (pos, _) = manhattan pos to
    goal (pos, History x _ _ _)
      | ultra = x >= 4 && pos == to
      | otherwise = pos == to
    constraints pos hist =
      pos `M.member` m
        && atMostDir (if ultra then 10 else 3) hist
        && noReverse hist
        && (ultra ==> minMoves 4 hist)
    cost _ (pos, _) = m M.! pos

infixl 3 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

part1 :: Map Pos Int -> Maybe Int
part1 m = crucible False m (0, 0) (maximum . M.keys $ m)

part2 :: Map Pos Int -> Maybe Int
part2 m = crucible True m (0, 0) (maximum . M.keys $ m)
