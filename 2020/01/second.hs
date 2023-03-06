module Main where

import Control.Monad (when)
import Data.Maybe (catMaybes, fromJust)
import System.Exit (die)

exampleExpectedOutput = 241861950

compute2020 :: Int -> Int -> Int -> Maybe Int
compute2020 a b c
  | a + b + c == 2020 = Just (a * b * c)
  | otherwise = Nothing

compute :: String -> Int
compute input = head . catMaybes $ processInput inputList
  where
    inputList :: [Int]
    inputList = map read $ lines input
    processInput :: [Int] -> [Maybe Int]
    processInput (_:[]) = [Nothing]
    processInput (x:xs) = (processA $ compute2020 x) xs ++ processInput xs
    processA :: (Int -> Int -> Maybe Int) -> [Int] -> [Maybe Int]
    processA f (_:[]) = [Nothing]
    processA f (x:xs) = map (f x) xs ++ processA f xs

main :: IO ()
main = do
  example <- readFile "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- readFile "input"
  print $ compute input
