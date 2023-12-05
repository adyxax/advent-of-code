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

exampleExpectedOutput = 35

data Map = Map Int Int Int deriving Show -- destination source range
data Mapping = Mapping String String [Map] deriving Show -- sourceStr destStr
data Input = Input [Int] [Mapping] deriving Show -- seeds mappings

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* many (char ' ')

parseMap :: Parser Map
parseMap = Map <$> parseNumber
               <*> parseNumber
               <*> parseNumber <* eol

parseMapping :: Parser Mapping
parseMapping = Mapping <$> (some letterChar <* string "-to-")
                       <*> (some letterChar <* string " map:" <* eol)
                       <*> some parseMap <* optional eol

parseInput' :: Parser Input
parseInput' = Input <$> (string "seeds: " *> some parseNumber <* some eol)
                    <*> some parseMapping <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

step :: [Map] -> Int -> Int
step [] n = n
step (Map dest src range:ms) n | n>=src && n<src+range = n - src + dest
                               | otherwise = step ms n

stepMapping :: [Int] -> Mapping -> [Int]
stepMapping values (Mapping _ _ ms) = map (step ms) values

compute :: Input -> Int
compute (Input values mappings) = minimum $ L.foldl' stepMapping values mappings

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
