module Main where

import Data.List.Extra (splitOn, sort, intercalate, transpose, group)
import Data.Map qualified as M

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day14"
  print $ part1 input
  print $ part2 input

data Stone = Rolling | Empty | Fixed
  deriving (Eq, Ord, Show)

parse :: String -> [[Stone]]
parse = transpose . map (map stone) . lines
  where
    stone '#' = Fixed
    stone 'O' = Rolling
    stone _ = Empty

roll :: [Stone] -> [Stone]
roll = intercalate [Fixed] . map sort . splitOn [Fixed]

rolls :: [[Stone]] -> [[Stone]]
rolls = (!! 5) . iterate (rotate . map roll)

score :: [Stone] -> Int
score = sum . map fst . filter ((== Rolling) . snd) . zip [1..] . reverse

rotate :: [[a]] -> [[a]]
rotate = map reverse . transpose

findLoop :: Ord a => (a -> a) -> a -> (Int, Int)
findLoop f = go M.empty 1
  where
    go seen i x =
      let x' = f x
      in case x' `M.lookup` seen of
           Just i' -> (i', i - i')
           Nothing -> go (M.insert x i seen) (i + 1) x'

part1 :: [[Stone]] -> Int
part1 = sum . map score . map roll

part2 :: [[Stone]] -> Int
part2 = sum . map score . go
  where
    go platform =
      let
        (cyc, clen) = findLoop rolls platform
        remaining = (1000000000 - cyc) `rem` clen
      in iterate rolls platform !! (1 + cyc + remaining)
