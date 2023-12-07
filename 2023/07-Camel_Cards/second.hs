-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 5905

data Card = J | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | Q | K | A

instance Eq Card where
  J == _ = True
  _ == J = True
  a == b = show a == show b

instance Ord Card where
  a `compare` b = show a `compare` show b
  a <= b = show a <= show b

-- Dirty hack so that it sorts nicely in ascii
instance Show Card where
  show J = "0"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show T = "A"
  show Q = "B"
  show K = "C"
  show A = "D"

data Rank = HighCard
          | Pair
          | Pairs
          | Brelan
          | FullHouse
          | Quartet
          | Quintet
          deriving (Eq, Ord, Show)

data Hand = Hand Rank [Card] Int deriving (Eq, Show)

compareCards :: [Card] -> [Card] -> Ordering
compareCards x y = concatMap show x `compare` concatMap show y

instance Ord Hand where
  (Hand a x _) `compare` (Hand b y _) | a == b = compareCards x y
                                      | otherwise = a `compare` b

type Input = [Hand]

type Parser = Parsec Void String

parseCard :: Parser Card
parseCard = char '2' $> Two
        <|> char '3' $> Three
        <|> char '4' $> Four
        <|> char '5' $> Five
        <|> char '6' $> Six
        <|> char '7' $> Seven
        <|> char '8' $> Eight
        <|> char '9' $> Nine
        <|> char 'T' $> T
        <|> char 'J' $> J
        <|> char 'Q' $> Q
        <|> char 'K' $> K
        <|> char 'A' $> A

evalRank :: [Card] -> Rank
evalRank [J, J, J, J, _]  = Quintet
evalRank [J, J, J, d, e] | d==e = Quintet
                         | otherwise = Quartet
evalRank [J, J, c, d, e] | c==d && d==e = Quintet
                         | c==d || d==e = Quartet
                         | otherwise = Brelan
evalRank [J, b, c, d, e] | b==c && c==d && d==e = Quintet
                         | (b==c || d==e) && c==d = Quartet
                         | b==c && d==e = FullHouse
                         | b==c || c==d || d==e = Brelan
                         | otherwise = Pair
evalRank [a, b, c, d, e] | a==b && a==c && a==d && a==e = Quintet
                         | (a==b && a==c && a==d) || (b==c && b==d && b==e) = Quartet
                         | a==b && (b==c || c==d) && d==e = FullHouse
                         | (a==b && b==c) || (b==c && c==d) || (c==d && d==e) = Brelan
                         | (a==b && (c==d || d==e)) || (b==c && d==e) = Pairs
                         | a==b || b==c || c==d || d==e = Pair
                         | otherwise = HighCard

parseHand :: Parser Hand
parseHand = do
  cards <- some parseCard <* char ' '
  bid <- read <$> (some digitChar <* eol)
  return $ Hand (evalRank $ L.sort cards) cards bid

parseInput' :: Parser Input
parseInput' = some parseHand <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum . zipWith (*) [1..] . map (\(Hand _ _ bid) -> bid) . L.sort

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
