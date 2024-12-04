-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 18

type Line = [Char]
type Input = [Line]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (some letterChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

rotate90 = reverse . L.transpose
rotate180 = rotate90 . rotate90

diagonals = (++) <$> L.transpose . zipWith drop [0..]
                 <*> L.transpose . zipWith drop [1..] . rotate180

countInString :: String -> Int
countInString []                    = 0
countInString s@('X':'M':'A':'S':_) = 1 + (countInString $ tail s)
countInString s@('S':'A':'M':'X':_) = 1 + (countInString $ tail s)
countInString s                     = countInString $ tail s

compute :: Input -> Int
compute lines = sum $ map countInString combinations
  where
    columns = rotate90 lines
    combinations = lines ++ columns ++ diagonals lines ++ diagonals columns

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
