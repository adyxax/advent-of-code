-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.Heap            as H
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 64

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
data Heading = N | S | E | W deriving (Eq, Ord, Show)
type Coord = (Int, Int, Heading)
type Path = S.Set Coord
data Position = Position Coord Cost Path deriving (Eq, Show)
instance Ord Position where
  compare (Position _ c1 _) (Position _ c2 _) = c1 `compare` c2
type Visited = M.Map Coord Int

type Candidates = H.MinHeap Position

compute :: Input -> Int
compute input = S.size $ S.map (\(x, y, _) -> (x, y)) $ walk infinity M.empty $ H.singleton (Position start 0 S.empty)
  where
    walk :: Int -> Visited -> Candidates -> S.Set Coord
    walk l v h | H.size h == 0 = S.empty
               | c > l = walk l v' h'
               | t == End = S.union s' $ walk c v' h'
               | otherwise = walk l v' $ H.union h' $ H.fromList $ nexts v' $ Position p c s'
      where
        ([(Position p@(x, y, d) c s)], h') = H.splitAt 1 h
        t = input V.! y V.! x
        v' = case M.lookup p v of
          Just c' -> if c < c' then M.insert p c v else v
          Nothing -> M.insert p c v
        s' = S.insert p s
    nexts :: Visited -> Position -> [Position]
    nexts v p = L.filter (valid v) $ candidates p
    valid :: Visited -> Position -> Bool
    valid v (Position p@(x, y, _) c _) = input V.! y V.! x /= Wall && case M.lookup p v of
      Just c' -> c <= c'
      Nothing -> True
    candidates :: Position -> [Position]
    candidates (Position (x, y, N) c s) = [ Position (x-1, y, W) (c+1001) s, Position (x+1, y, E) (c+1001) s, Position (x, y-1, N) (c+1) s ]
    candidates (Position (x, y, S) c s) = [ Position (x-1, y, W) (c+1001) s, Position (x+1, y, E) (c+1001) s, Position (x, y+1, S) (c+1) s ]
    candidates (Position (x, y, E) c s) = [ Position (x, y-1, N) (c+1001) s, Position (x, y+1, S) (c+1001) s, Position (x+1, y, E) (c+1) s ]
    candidates (Position (x, y, W) c s) = [ Position (x, y-1, N) (c+1001) s, Position (x, y+1, S) (c+1001) s, Position (x-1, y, W) (c+1) s ]
    height = V.length input
    width = V.length (input V.! 0)
    start = (1, height - 2, E)
    infinity = maxBound :: Int

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
