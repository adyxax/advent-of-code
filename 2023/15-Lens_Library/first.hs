-- requires cabal install --lib megaparsec parser-combinators split
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.List.Split qualified as DLS
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 1320

compute :: String -> Int
compute input = sum $ map (hash 0) items
  where
    items :: [String]
    items = DLS.endByOneOf ",\n" input
    hash :: Int -> String -> Int
    hash i [] = i
    hash i (x:xs) = hash (((i + C.ord x) * 17) `rem` 256) xs

main :: IO ()
main = do
  example <- readFile "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- readFile "input"
  print $ compute input
