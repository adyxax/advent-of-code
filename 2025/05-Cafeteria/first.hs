-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 3

type Interval = (Int, Int)
data Input = Input [Interval] [Int]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseInterval :: Parser Interval
parseInterval = (,) <$> parseNumber <* char '-'
                    <*> parseNumber

parseIntervals :: Parser [Interval]
parseIntervals = some (parseInterval <* eol)

parseInput' :: Parser Input
parseInput' = Input <$> parseIntervals <* eol
                    <*> some (parseNumber <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input intervals ingredients) = length $ filter fresh ingredients
  where
    fresh :: Int -> Bool
    fresh i = or $ map (fresh' i) intervals
    fresh' :: Int -> Interval -> Bool
    fresh' i (a, b) = i >= a && i <= b

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
