module Main where

import Data.List
import Data.Ord (Down(..))
import Data.Bifunctor

main :: IO ()
main = do
  input <- readFile "inputs/day7"
  print $ winnings . parse (hand . map card) $ input
  print $ winnings . parse (hand . map (joker . card)) $ input

winnings :: Ord card => [(Hand card, Int)] -> Int
winnings = sum . map (uncurry (*)) . map (second snd) . zip [1 ..] . sortOn Down

parse :: (String -> Hand card) -> String -> [(Hand card, Int)]
parse mkHand = map (go . words) . lines
  where
    go x =
      let
        h = mkHand . head $ x
        v = read . last $ x
      in (h, v)

data Card = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
  deriving (Eq, Ord, Show)

data JokeredCard = Joker | Simple Card
  deriving (Show)

instance Eq JokeredCard where
  Joker == _ = True
  _ == Joker = True
  Simple x == Simple y = x == y

instance Ord JokeredCard where
  Joker <= Joker = True
  Joker <= Simple _ = True
  Simple _ <= Joker = False
  Simple x <= Simple y = x <= y

joker :: Card -> JokeredCard
joker Jack = Joker
joker x = Simple x

card :: Char -> Card
card 'A' = Ace
card 'K' = King
card 'Q' = Queen
card 'J' = Jack
card 'T' = Ten
card '9' = Nine
card '8' = Eight
card '7' = Seven
card '6' = Six
card '5' = Five
card '4' = Four
card '3' = Three
card '2' = Two
card _ = error "invalid card"

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard
  deriving (Eq, Ord, Show)

data Hand card = Hand HandType [card]
  deriving (Eq, Ord, Show)

hand :: (Show card, Eq card, Ord card) => [card] -> Hand card
hand cards
  | counted == [5] = Hand FiveOfAKind cards
  | counted == [4, 1] = Hand FourOfAKind cards
  | counted == [3, 2] = Hand FullHouse cards
  | counted == [3, 1, 1] = Hand ThreeOfAKind cards
  | counted == [2, 2, 1] = Hand TwoPair cards
  | counted == [2, 1, 1, 1] = Hand OnePair cards
  | counted == [1, 1, 1, 1, 1] = Hand HighCard cards
  | otherwise = error $ "invalid cards: " <> show cards
  where
    counted = sortOn Down . map length . group . sort $ cards
