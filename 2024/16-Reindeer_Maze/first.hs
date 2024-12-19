-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.Heap            as H
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 11048

data Tile = Wall | Floor | Start | End deriving (Eq, Show)
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '#' $> Wall
        <|> char '.' $> Floor
        <|> char 'E' $> End
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
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Cost = Int
data Heading = N | S | E | W deriving (Eq, Show)
type Coord = (Int, Int)
data Position = Position Coord Heading Cost deriving Show
instance Ord Position where
  compare (Position _ _ c1) (Position _ _ c2) = c1 `compare` c2
instance Eq Position where
 (Position _ _ c1) == (Position _ _ c2) = c1 == c2
type Visited = M.Map Coord Int

type Candidates = H.MinHeap Position

compute :: Input -> Int
compute input = walk M.empty $ H.singleton (Position start E 0)
  where
    walk :: Visited -> Candidates -> Int
    walk v h | t == End = c
             | otherwise = walk v' $ H.union h' $ H.fromList $ nexts v' $ Position p d c
      where
        ([(Position p@(x, y) d c)], h') = H.splitAt 1 h
        t = input V.! y V.! x
        v' = case M.lookup p v of
          Just c' -> if c < c' then M.insert p c v else v
          Nothing -> M.insert p c v
    nexts :: Visited -> Position -> [Position]
    nexts v p = L.filter (valid v) $ candidates p
    valid :: Visited -> Position -> Bool
    valid v (Position p@(x, y) _ c) = input V.! y V.! x /= Wall && case M.lookup p v of
      Just c' -> c < c'
      Nothing -> True
    candidates :: Position -> [Position]
    candidates (Position (x, y) N c) = [ Position (x-1, y) W (c+1001), Position (x+1, y) E (c+1001), Position (x, y-1) N (c+1) ]
    candidates (Position (x, y) S c) = [ Position (x-1, y) W (c+1001), Position (x+1, y) E (c+1001), Position (x, y+1) S (c+1) ]
    candidates (Position (x, y) E c) = [ Position (x, y-1) N (c+1001), Position (x, y+1) S (c+1001), Position (x+1, y) E (c+1) ]
    candidates (Position (x, y) W c) = [ Position (x, y-1) N (c+1001), Position (x, y+1) S (c+1001), Position (x-1, y) W (c+1) ]
    height = V.length input
    width = V.length (input V.! 0)
    start = (1, height - 2)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
