{-# LANGUAGE RecordWildCards #-}
module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Monad
import Data.Maybe
import Data.List (tails)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day24"
  -- print $ mapMaybe (\x -> (x,) <$> uncurry collideXY x) . pairs $ input
  print $ part1 input

data Hailstone = Hailstone
  {
    hsX :: Int,
    hsY :: Int,
    hsZ :: Int,
    hsDX :: Int,
    hsDY :: Int,
    hsDZ :: Int
  }
  deriving (Show, Eq, Ord)

parse :: String -> [Hailstone]
parse = fst . last . readP_to_S (hailstone `sepBy1` char '\n')
  where
    hailstone = do
      Hailstone
        <$> (decimal <* string ", ")
        <*> (decimal <* string ", ")
        <*> (decimal <* string " @ ")
        <*> (decimal <* string ", ")
        <*> (decimal <* string ", ")
        <*> decimal

    decimal = do
      negative <- option 1 ((-1) <$ char '-')
      (* negative) . read <$> munch1 isDigit

collideXY :: Hailstone -> Hailstone -> Maybe (Double, Double)
collideXY a b
  | t >= 0 && u >= 0 = Just (x1 + t * (x2 - x1), y1 + t * (y2 - y1))
  | otherwise = Nothing
  where
    (x1, y1) = (fromIntegral $ hsX a, fromIntegral $ hsY a)
    (x2, y2) = (fromIntegral $ hsX a + hsDX a, fromIntegral $ hsY a + hsDY a)
    (x3, y3) = (fromIntegral $ hsX b, fromIntegral $ hsY b)
    (x4, y4) = (fromIntegral $ hsX b + hsDX b, fromIntegral $ hsY b + hsDY b)
    tn = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    td = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    un = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
    ud = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    t = tn / td
    u = un / ud

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [ (x,y) | (x:rest) <- tails xs , y <- rest ]

inArea :: Double -> Double -> (Double, Double) -> Bool
inArea low high (px, py) = low <= px && px <= high && low <= py && py <= high

part1 :: [Hailstone] -> Int
part1 = length . filter (inArea low high) . mapMaybe (uncurry collideXY) . pairs
  where
    low = 200000000000000
    high = 400000000000000
