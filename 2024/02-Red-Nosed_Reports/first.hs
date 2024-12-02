-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 2

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
compute input = length . filter id $ map (valid . differences) input
  where
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
