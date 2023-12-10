module Main where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as G
import Algebra.Graph.AdjacencyMap.Algorithm qualified as G
import Control.Monad (guard)
import Data.Foldable (find)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust)

main :: IO ()
main = do
  (graph, start) <- parseGraph <$> readFile "inputs/day10"
  print $ part1 graph start
  print $ part2

type Pos = (Int, Int)

-- | Parse graph without starting tile and starting position
parseGraph :: String -> (AdjacencyMap Pos, Pos)
parseGraph s =
  let coordinated = concatMap (\(i, js) -> map (\(j, z) -> ((i, j), z)) js) . zip [0 ..] . map (zip [0 ..]) . lines $ s
      mapped = M.fromList coordinated
      edges = concatMap (conns mapped) coordinated
   in (G.edges edges, fst . fromJust $ find ((== 'S') . snd) coordinated)
  where
    conns m (p@(i, j), piece) = case piece of
      '|' -> [(p, (i - 1, j)), (p, (i + 1, j))]
      '-' -> [(p, (i, j - 1)), (p, (i, j + 1))]
      'L' -> [(p, (i - 1, j)), (p, (i, j + 1))]
      'J' -> [(p, (i - 1, j)), (p, (i, j - 1))]
      '7' -> [(p, (i + 1, j)), (p, (i, j - 1))]
      'F' -> [(p, (i + 1, j)), (p, (i, j + 1))]
      'S' ->
        catMaybes
          [ (p, (i - 1, j)) <$ ((\n -> guard (n `elem` "|7F")) =<< (i - 1, j) `M.lookup` m),
            (p, (i + 1, j)) <$ ((\n -> guard (n `elem` "|LJ")) =<< (i + 1, j) `M.lookup` m),
            (p, (i, j - 1)) <$ ((\n -> guard (n `elem` "-LF")) =<< (i, j - 1) `M.lookup` m),
            (p, (i, j + 1)) <$ ((\n -> guard (n `elem` "-J7")) =<< (i, j + 1) `M.lookup` m)
          ]
      _ -> []

part1 :: AdjacencyMap Pos -> Pos -> Int
part1 g = (`div` 2) . length . G.reachable g

part2 :: String
part2 = "not doing flood fills this year"
