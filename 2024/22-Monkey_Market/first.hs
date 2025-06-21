-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 37327623

type Input = [Int]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (read <$> some digitChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = sum $ map (nextN 2000) input
  where
    nextN :: Int -> Int -> Int
    nextN 0 n = n
    nextN i n = nextN (i-1) $ next n
    next :: Int -> Int
    next s = let s' = prune $ mix (shift s 6) s
                 s'' = prune $ mix (shift s' (-5)) s'
                 s''' = prune $ mix (shift s'' 11) s''
             in s'''
    mix :: Int -> Int -> Int
    mix = xor
    prune :: Int -> Int
    prune n = n `mod` 16777216

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
