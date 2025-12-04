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
exampleExpectedOutput = 43

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
compute = step 0
  where
    step :: Int -> Input -> Int
    step acc input | removed == [] = acc
                   | otherwise = step (acc + length removed) input'
      where
        input' :: Input
        input' = L.foldl' remove input removed
        remove :: Input -> Coord -> Input
        remove v (x, y) = v V.// [(y, v V.! y V.// [(x, Floor)])]
        removed :: [Coord]
        removed = V.ifoldl' step' [] input
        step' :: [Coord] -> Int -> Line -> [Coord]
        step' acc y = V.ifoldl' (step'' y) acc
        step'' :: Int -> [Coord] -> Int -> Tile -> [Coord]
        step'' y acc x t | t == Roll && liftable x y = (x, y):acc
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
