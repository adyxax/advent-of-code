-- requires cabal install --lib split Unique
module Main (main) where

import Control.Monad (void, when)
import Data.List.Split (splitOn)
import Data.List.Unique (sortUniq)
import Data.Monoid (mconcat)
import System.Exit (die)

exampleExpectedOutput = 11

parseInput :: String -> IO [String]
parseInput filename = do
  input <- readFile filename
  return $ map (sortUniq . mconcat . lines) $ splitOn "\n\n" input

compute :: [String] -> Int
compute = sum . map length

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
