module Main (main) where
import Control.Monad (when)
import Data.List (group, sort)
import System.Exit (die)

exampleExpectedOutput = 8
exampleExpectedOutput2 = 19208

parseInput :: String -> IO [Int]
parseInput filename = do
  input <- readFile filename
  return $ map read $ lines input

compute :: [Int] -> Int
compute jolts = product $ map (([0, 1, 2, 4, 7] !!) . length) $ filter ((==) 1 . head) $  group $ zipWith (-) s (0 : s)
  where
    s = sort jolts

main :: IO ()
main = do
  example <- parseInput "example"
  let s = sort example
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput2 = compute example2
  when  (exampleOutput2 /= exampleExpectedOutput2)  (die $ "example2 failed: got " ++ show exampleOutput2 ++ " instead of " ++ show exampleExpectedOutput2)
  input <- parseInput "input"
  print $ compute input
