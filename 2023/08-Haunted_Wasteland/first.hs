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

exampleExpectedOutput = 2

data Direction = L | R deriving (Eq, Show)
type Branch = (String, String)
type Map = M.Map String Branch
data Input = Input [Direction] Map deriving Show

type Parser = Parsec Void String

parseDirection :: Parser Direction
parseDirection = char 'L' $> L
             <|> char 'R' $> R

parseNode :: Parser String
parseNode = some letterChar

parseBranch :: Parser Branch
parseBranch = (,) <$> (char '(' *> parseNode)
                  <*> (string ", " *> parseNode <* char ')')

parseMapElt :: Parser (String, Branch)
parseMapElt = (,) <$> (parseNode <* string " = ")
                  <*> (parseBranch <* eol)

parseInput' :: Parser Input
parseInput' = Input <$> some parseDirection <* eol <* eol
                    <*> (M.fromList <$> some parseMapElt <* eof)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input directions m) = compute' "AAA" 0
  where
    compute' :: String -> Int -> Int
    compute' node i | node == "ZZZ" = i
                    | otherwise = compute' (nextOne node) (i+1)
      where
        nextOne :: String -> String
        nextOne = next . (m M.!)
        next :: Branch -> String
        next | directions L.!! (i `mod` l) == L = fst
             | otherwise = snd
    l = length directions

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
