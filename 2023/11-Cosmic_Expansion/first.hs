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

exampleExpectedOutput = 374

type Position = (Int, Int)
type Input = [Position]

type Parser = Parsec Void String

parseLine :: Parser Position
parseLine = do
  (SourcePos _ y x) <- getSourcePos
  void $ char '#' <* some (void (char '.') <|> void eol)
  return (unPos x, unPos y)

parseInput' :: Parser Input
parseInput' = do
  some (void (char '.') <|> void eol)
  some parseLine <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = distances $ map expand input
  where
    distances :: Input -> Int
    distances [] = 0
    distances (x:xs) = distances' x xs + distances xs
    distances' :: Position -> Input -> Int
    distances' a [] = 0
    distances' a (x:xs) = distance a x + distances' a xs
    distance :: Position -> Position -> Int
    distance (x1, y1) (x2, y2) = abs (x2-x1) + abs (y2-y1)
    expand :: Position -> Position
    expand (x, y) = (x', y')
      where
        x' = x + S.size (S.filter (< x) emptyCols)
        y' = y + S.size (S.filter (< y) emptyRows)
    cols = S.fromList $ map fst input
    rows = S.fromList $ map snd input
    allCols = S.fromList [0..S.findMax cols]
    allRows = S.fromList [0..S.findMax rows]
    emptyCols = allCols S.\\ cols
    emptyRows = allRows S.\\ rows

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
