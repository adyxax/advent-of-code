-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 30

data Card = Card Int [Int] [Int] deriving Show
type Input = [Card]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* many (char ' ')

parseCard :: Parser Card
parseCard = do
  id <- string "Card" *> many (char ' ') *> parseNumber <* char ':' <* many (char ' ')
  winnings <- some parseNumber
  void $ char '|' <* many (char ' ')
  numbers <- some parseNumber
  return $ Card id winnings numbers

parseInput' :: Parser Input
parseInput' = some (parseCard <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Integer
compute = fst . L.foldl' compute' (0, L.repeat 1) . map score'
  where
    compute' :: (Integer, [Integer]) -> Int -> (Integer, [Integer])
    compute' (acc, stack) score = (acc', stack')
      where
        copies = head stack
        acc' = acc + copies
        (scored, remain) = L.splitAt score $ tail stack
        stack' = map (+ copies) scored ++ remain
    score' :: Card -> Int
    score' (Card _ winnings numbers) | wins == 0 = 0
                                     | otherwise = wins
      where
        wins = length $ winnings `L.intersect` numbers

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input

