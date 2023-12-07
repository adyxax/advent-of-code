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

exampleExpectedOutput = 6440

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Eq, Ord)

instance Show Card where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show T = "T"
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"

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
compareCards (x:xs) (y:ys) | x == y = compareCards xs ys
                           | otherwise = x `compare` y
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
evalRank n@(a:b:c:d:e:_) | not (a<=b && b<=c && c<=d && d<=e) = evalRank $ L.sort n
                         | a==b && b==c && c==d && d==e = Quintet
                         | (a==b && b==c && c==d) || (b==c && c==d && d==e) = Quartet
                         | a==b && (b==c || c==d) && d==e = FullHouse
                         | (a==b && b==c) || (b==c && c==d) || (c==d && d==e) = Brelan
                         | (a==b && (c==d || d==e)) || (b==c && d==e) = Pairs
                         | a==b || b==c || c==d || d==e = Pair
                         | otherwise = HighCard

parseHand :: Parser Hand
parseHand = do
  cards <- some parseCard <* char ' '
  bid <- read <$> (some digitChar <* eol)
  return $ Hand (evalRank cards) cards bid

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
