module Main where

import Data.Sequence (Seq)
import Data.Sequence qualified as S
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  sequences <- map (S.fromList . map (read @Int) . words) . lines <$> readFile "inputs/day9"
  print $ part1 sequences
  print $ part2 sequences

part1 :: [Seq Int] -> Int
part1 = sum . map futureElement

part2 :: [Seq Int] -> Int
part2 = sum . map pastElement

seqDiff :: Seq Int -> Seq Int
seqDiff xs = S.zipWith (flip (-)) xs (S.drop 1 xs)

tower :: Seq Int -> [Seq Int]
tower xs
  | all (== 0) xs = [xs]
  | otherwise = xs : tower (seqDiff xs)

futureElement :: Seq Int -> Int
futureElement xs =
  let
    lasts = reverse . mapMaybe lastS . tower $ xs
  in foldr1 (+) lasts

pastElement :: Seq Int -> Int
pastElement xs =
  let
    firsts = reverse . mapMaybe headS . tower $ xs
  in foldl1 (flip (-)) firsts

lastS :: Seq Int -> Maybe Int
lastS (_ S.:|> x) = Just x
lastS _ = Nothing

headS :: Seq Int -> Maybe Int
headS (x S.:<| _) = Just x
headS _ = Nothing
