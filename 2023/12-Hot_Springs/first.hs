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

exampleExpectedOutput = 21

data Tile = Broken | Operational | Unknown deriving Eq
instance Show Tile where
  show Broken = "#"
  show Operational = "."
  show Unknown = "?"
data Row = Row [Tile] [Int] deriving Show
type Input = [Row]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseTile :: Parser Tile
parseTile = char '#' $> Broken
        <|> char '.' $> Operational
        <|> char '?' $> Unknown

parseRow :: Parser Row
parseRow = Row <$> some parseTile <* space
               <*> some parseNumber <* eol

parseInput' :: Parser Input
parseInput' = some parseRow <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

valid :: [Int] -> [Tile] -> Bool
valid record tiles = map length (L.filter (L.any (/= Operational)) $ L.group tiles) == record

permutations :: [Tile] -> [[Tile]]
permutations [] = [[]]
permutations (Unknown:xs) = permutations (Operational:xs) ++ permutations (Broken:xs)
permutations (x:xs) = map (x:) $ permutations xs

compute :: Input -> Int
compute = sum . map compute'
  where
    compute' :: Row -> Int
    compute' (Row tiles record) = length . filter (valid record) $ permutations tiles

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
