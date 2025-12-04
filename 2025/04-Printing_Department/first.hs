-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 13

type Coord = (Int, Int)
data Tile = Floor | Roll deriving (Eq, Show)
type Line = V.Vector Tile
type Input = V.Vector Line
type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '.' $> Floor
        <|> char '@' $> Roll

parseLine :: Parser Line
parseLine = do
  line <- some parseTile <* eol
  return $ V.generate (length line) (line !!)

parseInput' :: Parser Input
parseInput' = do
  line <- some parseLine <* eof
  return $ V.generate (length line) (line !!)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = V.ifoldl' compute' 0 input
  where
    compute' :: Int -> Int -> Line -> Int
    compute' acc y = V.ifoldl' (compute'' y) acc
    compute'' :: Int -> Int -> Int -> Tile -> Int
    compute'' y acc x t | t == Roll && liftable x y = acc + 1
                        | otherwise = acc
    liftable :: Int -> Int -> Bool
    liftable x y = length (filter (== Roll) [tile i j|i<-[x-1..x+1], j<-[y-1..y+1]]) <=4
    tile :: Int -> Int -> Tile
    tile x y = case input V.!? y of
      Just line -> case line V.!? x of
        Just t -> t
        Nothing -> Floor
      Nothing -> Floor

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
