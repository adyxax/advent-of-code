-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 71503

data Input = Input [Int] [Int] deriving Show -- time distance

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some (digitChar <* many (char ' '))

parseInput' :: Parser Input
parseInput' = Input <$> (string "Time:" *> many (char ' ') *> some parseNumber <* eol)
                    <*> (string "Distance:" *> many (char ' ') *> some parseNumber <* eol <* eof)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

race :: Int -> Int -> Int
race limit record = limit + 1 - 2 * threshold
  where
    threshold = minTimeToBeat 0
    minTimeToBeat :: Int -> Int
    minTimeToBeat t | (limit - t) * t > record = t
                    | otherwise = minTimeToBeat (t+1)

compute :: Input -> Int
compute (Input times distances) = product $ zipWith race times distances

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
