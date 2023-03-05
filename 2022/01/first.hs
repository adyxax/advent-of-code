-- requires cabal install --lib split
module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

parseAndSum :: String -> [Int]
parseAndSum = map (sum . map read . lines) . splitOn "\n\n"

main :: IO ()
main = do
    input <- readFile "input"
    print . head . reverse . sort $ parseAndSum input
