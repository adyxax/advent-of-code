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
exampleExpectedOutput = 2

-- Let's represent lights as binary numbers and buttons as XOR masks
type Line = [Bool]
type Shape = [Line]
type Region = (Int, Int, [Int])
type Input = ([Shape], [Region])
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseTile :: Parser Bool
parseTile = char '#' $> True <|> char '.' $> False

parseLine :: Parser Line
parseLine = some parseTile <* eol

parseShape :: Parser Shape
parseShape = do
  void parseNumber <* char ':' <* eol
  some parseLine <* eol

parseRegion :: Parser Region
parseRegion = (,,) <$> parseNumber <* char 'x'
                   <*> parseNumber <* string ": "
                   <*> some parseNumber <* eol

parseInput' :: Parser Input
parseInput' = (,) <$> some (try parseShape) <*> some parseRegion <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (shapes, regions) = length $ filter id $ map fit regions
  where
    fit :: (Int, Int, [Int]) -> Bool
    fit (x, y, ss) = (x * y) > (sum $ map (\(s, a) -> s * a) $ zip ss sizes)
    sizes = map (length . filter id . concat) shapes

main :: IO ()
main = do
  --example <- parseInput "example"
  --print example
  --let exampleOutput = compute example
  --when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
