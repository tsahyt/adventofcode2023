{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Text.ParserCombinators.ReadP
import Data.Map (Map)
import Data.Map qualified as M
import Data.Char (isAlpha, isDigit)

main :: IO ()
main = do
  (wfs, ratings) <- parse <$> readFile "inputs/day19"
  print $ part1 wfs ratings

type Workflow = [Rule]

data RuleCondition = RuleCondition Char Ordering Int
  deriving (Show, Eq, Ord)

evalCondition :: Rating -> RuleCondition -> Bool
evalCondition (Rating x m a s) (RuleCondition f c z)  =
  let
    val = case f of
      'x' -> x
      'm' -> m
      'a' -> a
      's' -> s
  in compare val z == c

data Rule = Rule
  {
    ruleCondition :: Maybe RuleCondition,
    ruleInstruction :: Instruction
  }
  deriving (Show, Eq, Ord)

data Instruction
  = Goto String
  | Reject
  | Accept
  deriving (Show, Eq, Ord)

data Rating = Rating Int Int Int Int
  deriving (Show, Eq, Ord)

parse :: String -> (Map String Workflow, [Rating])
parse = fst . last . readP_to_S go
  where
    go = do
      wfs <- M.fromList <$> workflow `sepBy1` skipSpaces
      skipSpaces
      rts <- rating `sepBy1` skipSpaces
      pure (wfs, rts)

    workflow = (,) <$> munch1 isAlpha <*> between (char '{') (char '}') (rule `sepBy1` char ',')

    rule = Rule <$> option Nothing (Just <$> condition <* char ':') <*> instruction

    instruction = munch isAlpha >>= \case
      "A" -> pure Accept
      "R" -> pure Reject
      x -> pure $ Goto x

    condition = RuleCondition <$> satisfy (`elem` "xmas") <*> comparison <*> decimal

    comparison = satisfy (`elem` "<>") >>= \case
      '<' -> pure LT
      '>' -> pure GT
      _ -> error "impossible"

    rating = do
      x <- string "{x=" *> decimal
      m <- string ",m=" *> decimal
      a <- string ",a=" *> decimal
      s <- string ",s=" *> decimal <* string "}"
      pure $ Rating x m a s

    decimal = read <$> munch1 isDigit

eval :: Map String Workflow -> Rating -> Bool
eval wfs r = go (Goto "in")
  where
    go Accept = True
    go Reject = False
    go (Goto wf) =
      go . head . map ruleInstruction . filter (maybe True (evalCondition r) . ruleCondition) $ wfs M.! wf

score :: Rating -> Int
score (Rating x m a s) = x + m + a + s

part1 :: Map String Workflow -> [Rating] -> Int
part1 wfs = sum . map score . filter (eval wfs)
