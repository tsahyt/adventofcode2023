module Main where

import Data.Char (isDigit)
import Data.List (group)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust, mapMaybe, maybeToList)

type Grid a = Map (Int, Int) a

main :: IO ()
main = do
  grid <- readGrid <$> readFile "inputs/day3"
  print $ part1 grid
  print $ part2 grid

readGrid :: String -> Grid Char
readGrid = M.fromList . concat . zipWith (\i xs -> zipWith (\j x -> ((j, i), x)) [0 ..] xs) [0 ..] . lines

numberAt :: Grid Char -> (Int, Int) -> Maybe Int
numberAt grid (i, j) =
  let leftOf = takeWhile isDigit . map fromJust . takeWhile isJust . map (grid M.!?) $ zip [i - 1, i - 2 ..] (repeat j)
      rightOf = takeWhile isDigit . map fromJust . takeWhile isJust . map (grid M.!?) $ zip [i + 1, i + 2 ..] (repeat j)
      at = filter isDigit . maybeToList $ grid M.!? (i, j)
   in case at of
        [] -> Nothing
        center -> Just $ read (reverse leftOf <> center <> rightOf)

part1 :: Grid Char -> Int
part1 = sum . engineParts

part2 :: Grid Char -> Int
part2 = sum . map (uncurry (*)) . gears

-- | Because 'isSymbol' in base doesn't work here
isSymbol :: Char -> Bool
isSymbol c = not (isDigit c || c == '.')

isGearSymbol :: Char -> Bool
isGearSymbol = (== '*')

engineParts :: Grid Char -> [Int]
engineParts grid =
  let symbols = M.keys $ M.filter isSymbol grid
   in concatMap (neighbourNumbers grid) symbols

neighbours :: (Int, Int) -> [[(Int, Int)]]
neighbours (i, j) =
  [ [(i - 1, j - 1), (i, j - 1), (i + 1, j - 1)],
    [(i - 1, j), (i + 1, j)],
    [(i - 1, j + 1), (i, j + 1), (i + 1, j + 1)]
  ]

neighbourNumbers :: Grid Char -> (Int, Int) -> [Int]
neighbourNumbers grid pos = concat . map (map head . group . mapMaybe (numberAt grid)) . neighbours $ pos

gears :: Grid Char -> [(Int, Int)]
gears grid =
  let symbols = M.keys $ M.filter isGearSymbol grid
      numbers =
        mapMaybe
          ( \x ->
              case neighbourNumbers grid x of
                [a, b] -> Just (a, b)
                _ -> Nothing
          )
          symbols
   in numbers
