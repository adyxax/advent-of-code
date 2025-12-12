-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 7

-- Let's represent lights as binary numbers and buttons as XOR masks
type Light = Int
type Button = Int
type Joltage = [Int]
data Machine = Machine Light [Button] Joltage deriving (Show)
type Input = [Machine]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseLight :: Parser Light
parseLight = do
  bits <- char '[' *> some (char '.' $> '0' <|> char '#' $> '1') <* string "] "
  -- we need to reverse the bits for them to be in the right order for the masks
  pure . read $ "0b" ++ reverse bits

parseButton :: Parser Button
parseButton = do
  bits <- char '(' *> some parseNumber <* string ") "
  pure . L.foldl' (.|.) 0 $ map (2^) bits

parseJoltage :: Parser Joltage
parseJoltage = char '{' *> some parseNumber <* char '}'

parseMachine :: Parser Machine
parseMachine = Machine <$> parseLight <*> some parseButton <*> parseJoltage <* eol

parseInput' :: Parser Input
parseInput' = some parseMachine <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

compute :: Input -> Int
compute = sum . map compute'
  where
    compute' :: Machine -> Int
    compute' (Machine light buttons _) = press 1
      where
        press :: Int -> Int
        press n | or $ map pressStep $ combinations n buttons = n
                | otherwise = press $ n + 1
        pressStep :: [Int] -> Bool
        pressStep bs = light == L.foldl' xor 0 bs

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
