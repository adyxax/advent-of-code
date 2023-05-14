-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 67384529

type Cup = Int
type Input = [Cup]
type History = [Input]

type Parser = Parsec Void String

parseCup :: Parser Cup
parseCup = do
  n <- digitChar
  return $ read [n] - 1 -- so that there is a 0 in there and then we can use modulo

parseInput' :: Parser Input
parseInput' = some parseCup <* optional (char '\n') <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

score :: Input -> Int
score (0:xs) = read . concat $ map (\i -> show (i+1)) xs
score (x:xs) = score $ xs ++ [x]

compute' :: Input -> Int -> Int
compute' input 0 = score input
compute' (current:one:two:three:xs) remainingMoves = compute' step (remainingMoves - 1)
  where
    l = length xs + 4
    destinationIndex :: Int
    destinationIndex = case L.elemIndex ((current - 1) `mod` l) xs of
      Just i -> i
      Nothing -> case L.elemIndex ((current - 2) `mod` l) xs of
        Just i -> i
        Nothing -> case L.elemIndex ((current - 3) `mod` l) xs of
          Just i -> i
          Nothing -> fromJust $ L.elemIndex ((current - 4) `mod` l) xs
    destinationIndex' = destinationIndex + 1
    step :: Input
    step = concat [take destinationIndex' xs, [one, two, three], drop destinationIndex' xs, [current]]

compute :: Input -> Int
compute input = compute' input 100

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
