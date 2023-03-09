-- requires cabal install --lib megaparsec
module Main (main) where

import Control.Monad (void, when)
import Data.List (foldl')
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 336

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

computeOne :: Field -> Int-> Int
computeOne field step = snd $ foldl' tree (0, 0) field
  where
    tree :: (Int, Int) -> Line -> (Int, Int)
    tree (index, trees) l = (next index, trees + if l !! index then 1 else 0)
    next n = (n + step) `mod` fieldLen
    fieldLen = length $ field !! 0

steps = [1, 3, 5, 7]

compute :: Field -> Int
compute field = foldr (*) 1 (computeOne (skipEveryOtherLine field) 1 : map (computeOne field) steps)
  where
    skipEveryOtherLine :: Field -> Field
    skipEveryOtherLine [] = []
    skipEveryOtherLine (x:[]) = []
    skipEveryOtherLine (x:y:xs) = x : skipEveryOtherLine xs

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
