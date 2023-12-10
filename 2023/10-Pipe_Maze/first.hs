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
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 4

data Direction = N | S | E | W deriving (Eq, Show)
data Tile = NS | NE | NW | EW | ES | WS | Floor | Start deriving (Eq, Show)
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '|' $> NS
        <|> char 'L' $> NE
        <|> char 'J' $> NW
        <|> char '-' $> EW
        <|> char 'F' $> ES
        <|> char '7' $> WS
        <|> char '.' $> Floor
        <|> char 'S' $> Start

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
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compatible :: Tile -> Direction -> Bool
compatible NS N = True
compatible NS S = True
compatible NE N = True
compatible NE E = True
compatible NW N = True
compatible NW W = True
compatible EW E = True
compatible EW W = True
compatible ES E = True
compatible ES S = True
compatible WS W = True
compatible WS S = True
compatible _ _ = False

type Position = (Int, Int)
type Heading = (Position, Direction)

compute :: Input -> Int
compute input = walk start 0 `div` 2
  where
    walk :: Heading -> Int -> Int
    walk h i | tile (fst h) == Just Start = i+1
             | otherwise = walk h' (i+1)
      where
        h' = step h
    step :: Heading -> Heading
    step (p@(x, y), N) | tile p == Just NS = ((x, y-1), N)
                       | tile p == Just ES = ((x+1, y), E)
                       | tile p == Just WS = ((x-1, y), W)
    step (p@(x, y), S) | tile p == Just NS = ((x, y+1), S)
                       | tile p == Just NE = ((x+1, y), E)
                       | tile p == Just NW = ((x-1, y), W)
    step (p@(x, y), E) | tile p == Just NW = ((x, y-1), N)
                       | tile p == Just EW = ((x+1, y), E)
                       | tile p == Just WS = ((x, y+1), S)
    step (p@(x, y), W) | tile p == Just NE = ((x, y-1), N)
                       | tile p == Just EW = ((x-1, y), W)
                       | tile p == Just ES = ((x, y+1), S)
    start = head $ catMaybes [startn, starte, starts, startw]
    startn = nearStart (sx, sy-1) N
    starts = nearStart (sx, sy+1) S
    starte = nearStart (sx+1, sy) E
    startw = nearStart (sx-1, sy) W
    nearStart :: Position -> Direction -> Maybe Heading
    nearStart p d = case tile p of
      Just t -> if compatible t d then Just (p, d) else Nothing
      Nothing -> Nothing
    (sx, sy) = (x, y) -- start
      where
        hasNoStart :: Line -> Bool
        hasNoStart = V.all (/= Start)
        y = length $ V.takeWhile hasNoStart input
        x = length $ V.takeWhile (/= Start) (input V.! y)
    tile :: Position -> Maybe Tile
    tile (x, y) = case input V.!? y of
      Just line -> line V.!? x
      Nothing -> Nothing

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput2 = compute example2
  when  (exampleOutput2 /= 8)  (error $ "example failed: got " ++ show exampleOutput2 ++ " instead of " ++ show 8)
  input <- parseInput "input"
  print $ compute input
