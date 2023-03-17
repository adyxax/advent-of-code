module Main (main) where
import Control.Monad (when)
import Data.List (foldl')
import System.Exit (die)

exampleExpectedOutput = 127

parseInput :: String -> IO [Int]
parseInput filename = do
  input <- readFile filename
  return $ map read $ lines input

validate :: [Int] -> Int -> Bool
validate list n = processInput list
  where
    processInput :: [Int] -> Bool
    processInput (_:[]) = False
    processInput (x:xs) = (foldl' (\acc i -> acc || (x + i == n && x /= i)) False xs) || processInput xs

compute :: Int -> [Int] -> Int
compute len list
  | validate (take len list) (list !! len) = compute len $ tail list
  | otherwise = list !! len

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 5 example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute 25 input
