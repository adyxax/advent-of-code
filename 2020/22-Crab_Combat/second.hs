-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 291

type Card = Int
type Deck = [Card]
type Input = (Deck, Deck)
type History = [Input]

type Parser = Parsec Void String

parseCard :: Parser Card
parseCard = do
  n <- some digitChar
  void $ optional (char '\n')
  return $ read n

parseDeck :: Parser Deck
parseDeck = string "Player " *> digitChar *> string ":\n" *> some parseCard

parseInput' :: Parser Input
parseInput' = do
  p1 <- parseDeck
  void $ char '\n'
  p2 <- parseDeck
  void $ eof
  return (p1, p2)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

computeScore :: Deck -> Int
computeScore d = sum $ zipWith (*) d (reverse $ take (length d) [1..])

recursing :: Input -> History -> Bool
recursing i = or . map (== i)

computeRound :: Input -> Bool
computeRound (a:as, b:bs)
  | (length as >= a) && (length bs >= b) = computeSubGame (take a as, take b bs) []
  | otherwise = a > b

computeSubGame :: Input -> History -> Bool
computeSubGame ([], d) _ = False
computeSubGame (d, []) _ = True
computeSubGame (a:as, b:bs) h
  | recursing (a:as, b:bs) h = True
  | computeRound (a:as, b:bs) = computeSubGame (as ++ [a, b], bs) newH
  | otherwise = computeSubGame (as, bs ++ [b, a]) newH
  where
    newH = (a:as, b:bs) : h

compute' :: Input -> History -> Int
compute' ([], d) _ = computeScore d
compute' (d, []) _ = computeScore d
compute' (a:as, b:bs) h
  | recursing (a:as, b:bs) h = computeScore (a:as)
  | computeRound (a:as, b:bs) = compute' (as ++ [a, b], bs) newH
  | otherwise = compute' (as, bs ++ [b, a]) newH
  where
    newH = (a:as, b:bs) : h

compute :: Input -> Int
compute input = compute' input []

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
