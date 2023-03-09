-- requires cabal install --lib megaparsec
module Main (main) where

import Control.Monad (void, when)
import Data.List (foldl')
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 7

type Line = [Bool]
type Field = [Line]

type Parser = Parsec Void String

parseElt :: Parser Bool
parseElt = do
  c <- (char '#') <|> (char '.')
  return $ c == '#'

parseLine :: Parser Line
parseLine = some parseElt <* char '\n'

parseField :: Parser Field
parseField = some parseLine <* eof

parseInput :: String -> IO Field
parseInput filename = do
  input <- readFile filename
  case runParser parseField filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right field -> return field

compute :: Field -> Int
compute field = snd $ foldl' tree (0, 0) field
  where
    tree :: (Int, Int) -> Line -> (Int, Int)
    tree (index, trees) l = (next index, trees + if l !! index then 1 else 0)
    next n = (n + 3) `mod` fieldLen
    fieldLen = length $ field !! 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
