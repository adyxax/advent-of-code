-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Monad (void, when)
import Data.List (foldl', sort)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

type BoardingPass = Int

type Parser = Parsec Void String

parseColumn :: Parser Int
parseColumn = do
  colStr <- some (char 'L' <|> char 'R')
  return $ foldl' (\acc c -> acc * 2 + if c == 'R' then 1 else 0) 0 colStr

parseRow :: Parser Int
parseRow = do
  rowStr <- some (char 'B' <|> char 'F')
  return $ foldl' (\acc c -> acc * 2 + if c == 'B' then 1 else 0) 0 rowStr

parseBP :: Parser BoardingPass
parseBP = do
  row <- parseRow
  column <- parseColumn
  void (char '\n')
  return $ row * 8 + column

parseBPs :: Parser [BoardingPass]
parseBPs = some parseBP <* eof

parseInput :: String -> IO [BoardingPass]
parseInput filename = do
  input <- readFile filename
  case runParser parseBPs filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right bps -> return bps

compute :: [BoardingPass]-> Int
compute bps = snd . head $ filter (\(x, y) -> x /= y) $ zip (sort bps) [48..]

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
