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
data Tile = NS | NE | NW | EW | ES | WS | Floor | Start | Loop deriving (Eq, Show)
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
compatible NE S = True
compatible NE W = True
compatible NW S = True
compatible NW E = True
compatible EW E = True
compatible EW W = True
compatible ES W = True
compatible ES N = True
compatible WS E = True
compatible WS N = True
compatible _ _ = False

type Position = (Int, Int)
type Heading = (Position, Direction)
data Where = Outside | Inside

compute :: Input -> Int
compute input = V.sum $ V.zipWith (count 0 False) inputWithStartReplaced inputLooped
  where
    count :: Int -> Bool -> Line -> Line -> Int
    count i w line line' | V.length line == 0 = i
                         | u /= Loop = count (if w then i+1 else i) w r s
                         | t == NS = count i (not w) r s
                         | (t == NE && t'== NW) || (t == ES && t' == WS) = count i w r'' s'
                         | otherwise = count i (not w) r'' s'
      where
        (t, u) = (line V.! 0, line' V.!0)
        (r, s) = (V.tail line, V.tail line')
        r' = V.dropWhile (== EW) r
        s' = V.drop (1 + V.length s - V.length r') s
        t' = r' V.! 0
        r'' = V.tail r'
    inputWithStartReplaced = input V.// [(sy, (input V.! sy) V.// [(sx, startingPipe startn starte starts startw)])]
    startingPipe Nothing Nothing _ _ = WS
    startingPipe Nothing _ Nothing _ = EW
    startingPipe Nothing _ _ Nothing = ES
    startingPipe _ Nothing Nothing _ = NW
    startingPipe _ Nothing _ Nothing = NS
    startingPipe _ _ Nothing Nothing = NE
    inputLooped = walk start input
    walk :: Heading -> Input -> Input -- walk the loop, marking each point in the loop to a Loop tile
    walk h@((x, y), _) i | tile (fst h) == Just Start = i'
                         | otherwise = walk h' i'
      where
        h' = step h
        i' = i V.// [(y, (i V.! y) V.// [(x, Loop)])]
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
  example <- parseInput "example3"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example4 <- parseInput "example4"
  let exampleOutput4 = compute example4
  when  (exampleOutput4 /= 8)  (error $ "example failed: got " ++ show exampleOutput4 ++ " instead of " ++ show 8)
  example5 <- parseInput "example5"
  let exampleOutput5 = compute example5
  when  (exampleOutput5 /= 10)  (error $ "example failed: got " ++ show exampleOutput5 ++ " instead of " ++ show 10)
  input <- parseInput "input"
  print $ compute input
