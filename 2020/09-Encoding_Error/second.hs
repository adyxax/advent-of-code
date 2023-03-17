module Main (main) where
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (fromJust, isNothing)
import System.Exit (die)

exampleExpectedOutput = 62

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

isContiguousSum :: Int -> [Int] -> Int -> Maybe Int
isContiguousSum target list counter
  | sum' == target = Just $ (minimum lst) + (maximum lst)
  | sum' < target = isContiguousSum target list (counter + 1)
  | otherwise = Nothing
  where
    lst = take counter list
    sum' = sum lst

compute :: Int -> [Int] -> Int
compute len list
  | validate (take len list) (list !! len) = compute len $ tail list
  | otherwise = list !! len

compute' :: Int -> [Int] -> Int
compute' target list = case isContiguousSum target list 2 of
    Nothing -> compute' target (tail list)
    Just a -> a

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 5 example
      exampleOutput' = compute' exampleOutput example
  when  (exampleOutput' /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput' ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute' (compute 25 input) input
