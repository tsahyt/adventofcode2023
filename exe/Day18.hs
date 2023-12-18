module Main where

import Control.Monad (replicateM)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List (scanl')
import Numeric (readHex)
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day18"
  print $ part1 input
  print $ part2 input

data Direction = North | East | South | West
  deriving (Show)

data Instruction = Instruction Direction Int String
  deriving (Show)

decodeHex :: Instruction -> Instruction
decodeHex (Instruction _ _ s) =
  let s' = fst . last . readHex . init . tail $ s
      d = case last s of
        '0' -> East
        '1' -> South
        '2' -> West
        '3' -> North
        _ -> error "Invalid hex"
   in Instruction d s' s

type Pos = (Int, Int)

parse :: String -> [Instruction]
parse = map (fst . last . readP_to_S instruction) . lines
  where
    decimal = read <$> many1 (satisfy isDigit)

    direction = choice [North <$ char 'U', East <$ char 'R', South <$ char 'D', West <$ char 'L']

    instruction = do
      d <- direction <* char ' '
      s <- decimal <* char ' '
      c <- between (char '(') (char ')') (replicateM 7 get)
      pure $ Instruction d s c

dig :: [Instruction] -> [Pos]
dig = tail . scanl' go (0, 0)
  where
    go (i, j) (Instruction North s _) = (i - s, j)
    go (i, j) (Instruction East s _) = (i, j + s)
    go (i, j) (Instruction South s _) = (i + s, j)
    go (i, j) (Instruction West s _) = (i, j - s)

manhattan :: Pos -> Pos -> Int
manhattan (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

boundary :: [Pos] -> Int
boundary = fst . foldl' go (0, (0, 0))
  where
    go (z, p1) p2 = (z + manhattan p1 p2, p2)

area :: [Pos] -> Int
area xs = (sum . map trapezoid $ zip xs (tail . cycle $ xs)) `div` 2
  where
    trapezoid ((x1, y1), (x2, y2)) = (x1 + x2) * (y1 - y2)

interior :: Int -> Int -> Int
interior a b = a + 1 - b `div` 2

part1 :: [Instruction] -> Int
part1 is =
  let m = dig is
      a = area m
      b = boundary m
      i = interior a b
   in i + b

part2 :: [Instruction] -> Int
part2 is = let is' = map decodeHex is in part1 is'
