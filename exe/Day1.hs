module Main where
import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  input <- readFile "inputs/day1"
  print $ part1 (lines input)
  print $ part2 (lines input)

part1 :: [String] -> Int
part1 = sum . map readDigits

part2 :: [String] -> Int
part2 = part1 . map convert

readDigits :: String -> Int
readDigits x =
  let
    digits = map (read . pure) . filter isDigit $ x
  in head digits * 10 + last digits

ex2 =
        ["two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen"]

convert :: String -> String
convert [] = []
convert x
  | "one" `isPrefixOf` x = '1' : convert (tail x)
  | "two" `isPrefixOf` x = '2' : convert (tail x)
  | "three" `isPrefixOf` x = '3' : convert (tail x)
  | "four" `isPrefixOf` x = '4' : convert (tail x)
  | "five" `isPrefixOf` x = '5' : convert (tail x)
  | "six" `isPrefixOf` x = '6' : convert (tail x)
  | "seven" `isPrefixOf` x = '7' : convert (tail x)
  | "eight" `isPrefixOf` x = '8' : convert (tail x)
  | "nine" `isPrefixOf` x = '9' : convert (tail x)
  | otherwise = head x : convert (tail x)
