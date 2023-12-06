module Main where

main :: IO ()
main = do
  input <- readFile "inputs/day6"
  print $ parts . parse $ input
  print $ parts . parse . kern $ input

parts :: [Race] -> Int
parts = product . map solveRace

kern :: String -> String
kern = unlines . map (go . words) . lines
  where
    go [] = []
    go (x : xs) = unwords [x, concat xs]

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Show)

parse :: String -> [Race]
parse s =
  let ts = map read . drop 1 . words . head . lines $ s
      ds = map read . drop 1 . words . last . lines $ s
   in zipWith Race ts ds

solveRace :: Race -> Int
solveRace (Race t d) =
  let root = sqrt @Double $ realToFrac (t * t - 4 * d)
      upperR = (root + fromIntegral t) / 2
      upper
        | isInt upperR = truncate upperR - 1
        | otherwise = floor upperR
      lower = floor ((fromIntegral t - root) / 2)
   in upper - lower
  where
    isInt x = x == fromInteger (round x)
