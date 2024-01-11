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

exampleExpectedOutput = 136

data Tile = Cube | Empty | Round deriving (Eq)
instance Show Tile where
  show Cube = "#"
  show Empty = "."
  show Round = "O"
type Row = [Tile]
type Input = [Row]

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '#' $> Cube
        <|> char '.' $> Empty
        <|> char 'O' $> Round

parseRow :: Parser Row
parseRow = some parseTile <* eol

parseInput' :: Parser Input
parseInput' = some parseRow <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = sum $ map (fst . L.foldr weight (0, 1)) shiftedInput
  where
    transposedInput = L.transpose input
    shiftedInput = map (shift 0) transposedInput
    shift :: Int -> Row -> Row
    shift n [] = replicate n Empty
    shift n (Cube:xs) = replicate n Empty ++ Cube : shift 0 xs
    shift n (Empty:xs) = shift (n+1) xs
    shift n (Round:xs) = Round : shift n xs
    weight :: Tile -> (Int, Int) -> (Int, Int)
    weight Round (acc, i) = (acc + i, i+1)
    weight _ (acc, i) = (acc, i+1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
