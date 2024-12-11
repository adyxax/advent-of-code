-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 55312

type Input = [Int]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseInput' :: Parser Input
parseInput' = some parseNumber <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = sum $ map (blink 25) input
  where
    blink :: Int -> Int -> Int
    blink 0 n = 1
    blink i n = sum $ map (blink (i-1)) (blinkOne n)
    blinkOne :: Int -> [Int]
    blinkOne 0 = [1]
    blinkOne n | m == 0 = [l, r]
               | otherwise = [n * 2024]
      where
        (d, m) = length (show n) `divMod` 2
        (l, r) = n `divMod` (10^d)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
