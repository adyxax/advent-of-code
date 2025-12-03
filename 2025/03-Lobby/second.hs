-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 3121910778619

type Input = [String]
type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (some digitChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

maxWithIndex :: String -> (Int, Char)
maxWithIndex (x:xs) = let (_, i, m) = L.foldl' step (1, 0, x) xs in (i, m)
  where
    step :: (Int, Int, Char) -> Char -> (Int, Int, Char)
    step (index, maxIndex, max) c | max < c   = (index + 1, index, c)
                                  | otherwise = (index + 1, maxIndex, max)

compute :: Input -> Int
compute = sum . map compute'
  where
    compute' :: String -> Int
    compute' n = read . fst $ L.foldl' compute'' ("", 0) $ drop (length n - 11) $ L.inits n
    compute'' :: (String, Int) -> String -> (String, Int)
    compute'' (acc, i) s = let (ai, a) = maxWithIndex (drop i s)
                           in (acc ++ [a], i + ai + 1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
