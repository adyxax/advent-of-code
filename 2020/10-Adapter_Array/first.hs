module Main (main) where
import Control.Monad (when)
import Data.List (foldl', sort)
import System.Exit (die)

exampleExpectedOutput = 35
exampleExpectedOutput2 = 220

parseInput :: String -> IO [Int]
parseInput filename = do
  input <- readFile filename
  return $ map read $ lines input

compute :: [Int] -> Int
compute jolts = result $ foldl' compute' (0, 0, 0) $ sort jolts
  where
    compute' (prev, ones, threes) jolt
      | jolt - prev == 1 = (jolt, ones + 1, threes)
      | jolt - prev == 3 = (jolt, ones, threes + 1)
    result (_, ones, threes) = ones * (threes + 1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput2 = compute example2
  when  (exampleOutput2 /= exampleExpectedOutput2)  (die $ "example2 failed: got " ++ show exampleOutput2 ++ " instead of " ++ show exampleExpectedOutput2)
  input <- parseInput "input"
  print $ compute input
