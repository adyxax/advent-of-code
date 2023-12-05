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

exampleExpectedOutput = 46

data Range = Range Int Int deriving Show -- start range
data Map = Map Int Range deriving Show -- destination range
data Mapping = Mapping String String [Map] deriving Show -- sourceStr destStr
data Input = Input [Range] [Mapping] deriving Show -- seeds mappings

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* many (char ' ')

parseRange :: Parser Range
parseRange = Range <$> parseNumber
                   <*> parseNumber

parseMap :: Parser Map
parseMap = Map <$> parseNumber
               <*> parseRange <* eol

parseMapping :: Parser Mapping
parseMapping = Mapping <$> (some letterChar <* string "-to-")
                       <*> (some letterChar <* string " map:" <* eol)
                       <*> some parseMap <* optional eol

parseInput' :: Parser Input
parseInput' = Input <$> (string "seeds: " *> some parseRange <* some eol)
                    <*> some parseMapping <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

step :: [Map] -> Range -> [Range]
step [] n = [n]
step (Map dest (Range src range):ms) r@(Range n l) | n+l<=src || n>=src+range = step ms r
                                                   | n>=src && n+l<=src+range = [Range (n-src+dest) l]
                                                   | n>=src = Range (n-src+dest) (range+src-n) : step ms (Range (src+range) (l-range-src+n))
                                                   | otherwise = Range dest (l-src+n) : step ms (Range n (src-n))

stepMapping :: [Range] -> Mapping -> [Range]
stepMapping ranges (Mapping _ _ ms) = concatMap (step ms) ranges

compute :: Input -> Int
compute (Input ranges mappings) = minimum $ map (\(Range start _) -> start) $ L.foldl' stepMapping ranges mappings

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
