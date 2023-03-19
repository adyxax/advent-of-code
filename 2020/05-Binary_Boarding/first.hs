-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Monad (void, when)
import Data.List (foldl')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 820

data BoardingPass = BoardingPass { row :: Int
                                 , column :: Int
                                 } deriving (Show)

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
  return $ BoardingPass { row=row, column=column }

parseBPs :: Parser [BoardingPass]
parseBPs = some parseBP <* eof

parseInput :: String -> IO [BoardingPass]
parseInput filename = do
  input <- readFile filename
  case runParser parseBPs filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right bps -> return bps

compute :: [BoardingPass]-> Int
compute = foldl' (\max BoardingPass{row=r, column=c} -> let n = r * 8 + c in if max >= n then max else n) 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
