module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Data.Set qualified as S
import Data.Map qualified as M

main :: IO ()
main = do
  input <- M.fromList . map parse . lines <$> readFile "inputs/day4"
  print $ part1 input
  print $ part2 input

type Card = ([Int], [Int])

type Table = M.Map Int Card

parse :: String -> (Int, Card)
parse = fst . last . readP_to_S go
  where
    decimal = read <$> many1 (satisfy isDigit)

    go = do
      cardId <- string "Card" *> skipSpaces *> decimal
      char ':' *> skipSpaces
      winning <- decimal `sepBy1` many1 (char ' ')
      skipSpaces *> char '|' *> skipSpaces
      have <- decimal `sepBy1` many1 (char ' ')
      pure (cardId, (winning, have))

part1 :: Table -> Int
part1 = sum . M.map (matchScore . matches)

matchScore :: Int -> Int
matchScore 0 = 0
matchScore x = 2 ^ (x - 1)

matches :: Card -> Int
matches (winning, have) =
  let
    winnings = S.fromList winning
  in length $ filter (`S.member` winnings) have

copyScore :: Int -> Card -> [Int]
copyScore cardId card =
  let
    ms = matches card
  in zipWith (+) (repeat cardId) [1 .. ms]

part2 :: Table -> Int
part2 table = sum . copy table (M.map (const 1) table) $ M.keys table

copy :: Table -> M.Map Int Int -> [Int] -> M.Map Int Int
copy _ cards [] = cards
copy table cards (x:xs) =
  let
    card = table M.! x
    draw = cards M.! x
    next = filter (`M.member` table) $ copyScore x card
    cards' = foldl (\cs i -> M.adjust (+ draw) i cs) cards next
  in copy table cards' xs
