{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/day5"
  print $ part1 input

part1 :: Almanac -> Int
part1 Almanac {..} = minimum . map (runMaps maps) $ seeds

data Almanac = Almanac
  { seeds :: [Int],
    maps :: [[Mapping]]
  }
  deriving (Show)

data Mapping = Mapping
  { destStart :: Int,
    sourceStart :: Int,
    length :: Int
  }
  deriving (Show)

maybeMap :: Mapping -> Int -> Maybe Int
maybeMap (Mapping d s l) x
  | x >= s && x < s + l = Just $ d + x - s
  | otherwise = Nothing

parse :: String -> Almanac
parse = fst . last . readP_to_S go
  where
    go = do
      seeds <- string "seeds: " *> sepBy1 decimal (char ' ')
      maps <- sepBy1 map' (char '\n' *> string "\n")
      pure $ Almanac {..}

    decimal :: ReadP Int
    decimal = read <$> many1 (satisfy isDigit)

    map' :: ReadP [Mapping]
    map' = do
      _ <- munch (/= ':') *> char ':' *> satisfy isSpace
      sepBy1 (Mapping <$> decimal <*> (char ' ' *> decimal) <*> (char ' ' *> decimal)) (satisfy isSpace)

runMaps :: [[Mapping]] -> Int -> Int
runMaps ms s = foldl' go s ms
  where
    go z m = fromMaybe z (getAlt . foldMap (Alt . flip maybeMap z) $ m)
