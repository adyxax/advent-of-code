-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 4174379265

type Range = (Int, Int)

type Input = [Range]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseRotation :: Parser Range
parseRotation = (,) <$> parseNumber <* char '-'
                    <*> parseNumber

parseInput' :: Parser Input
parseInput' = some (parseRotation <* optional (char ',')) <* optional eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum . map compute'
  where
    compute' :: (Int, Int) -> Int
    compute' (l, h) = sum $ map computeOne [l..h]
    computeOne :: Int -> Int
    computeOne n | invalid n = n
                 | otherwise = 0
    invalid :: Int -> Bool
    invalid n = let s = show n
                    l = length s
                in or [invalid' s i | i <- [1..l `div` 2], l `mod` i == 0]
    invalid' :: String -> Int -> Bool
    invalid' s i = s `L.isPrefixOf` (cycle $ take i s)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
