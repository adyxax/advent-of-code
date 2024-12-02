-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 4

type Report = [Int]
type Input = [Report]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseReport :: Parser Report
parseReport = some parseNumber

parseInput' :: Parser Input
parseInput' = some (parseReport <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = length . filter id $ map (any (valid . differences) . candidates) input
  where
    candidates :: Report -> [Report]
    candidates report = report : [splitThis i|i<-[1..length report]]
      where
        splitThis i = let (l, r) = L.splitAt i report
                      in init l ++ r
    differences report = zipWith (-) (init report) (tail report)
    valid :: Report -> Bool
    valid rs@(r:_) | r > 0 = all (\v -> v > 0 && v <= 3) rs
                   | r < 0 = all (\v -> v < 0 && v >= (-3)) rs
                   | otherwise = False

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
