-- requires cabal install --lib megaparsec parser-combinators
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
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 405

data Tile = Ash | Rock deriving (Eq)
instance Show Tile where
  show Ash = "."
  show Rock = "#"
type Row = [Tile]
type Pattern = [Row]
type Input = [Pattern]

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '.' $> Ash
        <|> char '#' $> Rock

parseRow :: Parser Row
parseRow = some parseTile <* eol

parsePattern :: Parser Pattern
parsePattern = some parseRow <* eol

parseInput' :: Parser Input
parseInput' = some parsePattern <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum . map findMirror
  where
    findMirror :: Pattern -> Int
    findMirror xs = 100 * findMirrors xs 1 + findMirrors (L.transpose xs) 1
    findMirrors :: Pattern -> Int -> Int
    findMirrors xs i | i == length xs = 0
                     | not identical = next
                     | otherwise = i
      where
        (is, ts) = L.splitAt i xs
        next = findMirrors xs (i+1)
        is' = reverse is -- we need to flip the left side
        identical = and $ zipWith (==) is' ts

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
