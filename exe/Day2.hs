{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Foldable (fold)
import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

data Game = Game
  {
    gameId :: Int,
    cubes :: [Cubes]
  }
  deriving Show

data Cubes = Cubes
  {
    redCubes :: Int,
    greenCubes :: Int,
    blueCubes :: Int
  }
  deriving Show

instance Semigroup Cubes where
  Cubes a b c <> Cubes d e f = Cubes (max a d) (max b e) (max c f)

instance Monoid Cubes where
  mempty = Cubes 0 0 0

main :: IO ()
main = do
  input <- readFile "inputs/day2"
  let games = map parseGame . lines $ input
  print $ part1 (Cubes 12 13 14) games
  print $ part2 games

maxGame :: Game -> Cubes
maxGame = fold . cubes

fits :: Cubes -> Cubes -> Bool
fits (Cubes mr mg mb) (Cubes r g b) = r <= mr && g <= mg && b <= mb

parseGame :: String -> Game
parseGame = fst . last . readP_to_S parse
  where
    parse :: ReadP Game
    parse = do
      string "Game" *> skipSpaces
      gameId <- decimal
      _ <- char ':'
      skipSpaces
      cubes <- (mconcat <$> (cube `sepBy1` char ',')) `sepBy` char ';'
      pure $ Game{..}

    cube :: ReadP Cubes
    cube = do
      skipSpaces
      x <- decimal
      skipSpaces
      color <- choice [ string "red", string "green", string "blue" ]
      pure $ case color of
        "red" -> Cubes x 0 0
        "green" -> Cubes 0 x 0
        "blue" -> Cubes 0 0 x
        _ -> mempty

    decimal :: ReadP Int
    decimal = read <$> many1 (satisfy isDigit)

part1 :: Cubes -> [Game] -> Int
part1 limit games =
  let
    valid = filter (fits limit . maxGame) games
  in sum . map gameId $ valid

minPower :: Game -> Int
minPower = power . fold . cubes
  where
    power (Cubes r g b) = r * g * b

part2 :: [Game] -> Int
part2 = sum . map minPower
