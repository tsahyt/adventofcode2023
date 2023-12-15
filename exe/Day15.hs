module Main where

import Data.Char (ord)
import Data.Foldable (foldl')
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as M

main :: IO ()
main = do
  input <- lines . map (\c -> if c == ',' then '\n' else c) <$> readFile "inputs/day15"
  print $ part1 input
  print $ part2 (map parse input)

hash :: String -> Int
hash = foldl' go 0
  where
    go z c = (17 * (z + ord c)) `rem` 256

data Instruction = Lens String Int | Remove String

parse :: String -> Instruction
parse s
  | '=' `elem` s =
      let (l, x) = span (/= '=') s
       in Lens l (read $ tail x)
  | otherwise = Remove (init s)

eval :: Map Int [(String, Int)] -> Instruction -> Map Int [(String, Int)]
eval m (Remove s) = M.adjust (filter ((/= s) . fst)) (hash s) m
eval m (Lens s x) = M.adjust go (hash s) m
  where
    go ls
      | Just idx <- findIndex ((== s) . fst) ls =
          take idx ls <> [(s, x)] <> drop (idx + 1) ls
      | otherwise = ls <> [(s, x)]

focusPower :: (Int, (Int, (String, Int))) -> Int
focusPower (box, (pos, (_, fl))) = (1 + box) * pos * fl

part1 :: [String] -> Int
part1 = sum . map hash

part2 :: [Instruction] -> Int
part2 =
  sum . map focusPower . concatMap (\(box, ls) -> map (box,) (zip [1 ..] ls)) .
  M.toList . foldl' eval (M.fromList [(i, []) | i <- [0 .. 255]])
