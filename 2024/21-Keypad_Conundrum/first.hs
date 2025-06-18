-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 126384

type Input = [String]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (some alphaNumChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

data Key = U | D | L | R deriving (Eq)
instance Ord Key where
  compare R _ = LT
  compare U _ = LT
  compare D _ = LT
  compare _ _ = LT
type Coord = (Int, Int)
type Memo = M.Map (Char, Char) String

keyPad :: Char -> Coord
keyPad '7' = (0, 0)
keyPad '8' = (1, 0)
keyPad '9' = (2, 0)
keyPad '4' = (0, 1)
keyPad '5' = (1, 1)
keyPad '6' = (2, 1)
keyPad '1' = (0, 2)
keyPad '2' = (1, 2)
keyPad '3' = (2, 2)
keyPad '0' = (1, 3)
keyPad 'A' = (2, 3)
keyPad '^' = (1, 0)
keyPad 'B' = (2, 0)
keyPad '<' = (0, 1)
keyPad 'v' = (1, 1)
keyPad '>' = (2, 1)

pairCharacters :: String -> [(Char, Char)]
pairCharacters []       = []
pairCharacters [_]      = []
pairCharacters (x:y:xs) = (x, y) : pairCharacters (y:xs)

paths :: (Char, Char) -> [String]
paths (a, b) | a == '<' || x1 == 0 && y2 == 3 = [two]
             | b == '<' || y1 == 3 && x2 == 0 = [one]
             | otherwise = L.nub [one, two]
  where
    one = moves ++ "B"
    two = reverse moves ++ "B"
    moves = replicate (abs $ y2 - y1) (if y2 > y1 then 'v' else '^') ++ replicate (abs $ x2 - x1) (if x2 > x1 then '>' else '<')
    (x1, y1) = keyPad a
    (x2, y2) = keyPad b

compute :: Input -> Int
compute codes = sum $ map complexity $ zip (fst $ L.foldl' computeCode ([], M.empty) codes) codes
  where
    complexity :: (String, String) -> Int
    complexity (output, code) = (length output) * (read $ init code)
    computeCode :: ([String], Memo) -> String -> ([String], Memo)
    computeCode (acc, memo) code = let (s, memo') = L.foldl' computeMemoPair ("", memo) $ pairCharacters ('A' : code)
                                   in (acc ++ [s], memo')
    computeMemoPair :: (String, Memo) -> (Char, Char) -> (String, Memo)
    computeMemoPair (acc, memo) ab = case M.lookup ab memo of
      Just s  -> (acc ++ s, memo)
      Nothing -> let s = computePair ab in (acc ++ s, M.insert ab s memo)
    computePair :: (Char, Char) -> String
    computePair ab = head . L.sortBy (comparing L.length) $ iter . iter $ paths ab
    iter :: [String] -> [String]
    iter s = concatMap (transition) s
    transition :: String -> [String]
    transition s = L.foldl' aggregate [] $ pairCharacters ('B' : s)
    aggregate :: [String] -> (Char, Char) -> [String]
    aggregate [] ab  = paths ab
    aggregate acc ab = concatMap (\x -> map (\a -> a ++ x) acc) $ paths ab

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
