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

exampleExpectedOutput = 22

type Coord = (Int, Int)
type Input = [Coord]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseCoord :: Parser Coord
parseCoord = (,) <$> parseNumber <* char ','
                 <*> parseNumber <* eol

parseInput' :: Parser Input
parseInput' = some parseCoord <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Cost = Int
data Position = Position Coord Cost deriving Show
instance Ord Position where
  compare (Position _ c1) (Position _ c2) = c1 `compare` c2
instance Eq Position where
 (Position p1 _ ) == (Position p2 _ ) = p1 == p2
type Visited = M.Map Coord Cost
type Maze = M.Map Coord ()

type Candidates = H.MinHeap Position

compute :: Int -> Int -> Input -> Int
compute size cutoff input = walk (M.singleton (0, 0) 0) $ H.singleton (Position (0, 0) 0)
  where
    walk :: Visited -> Candidates -> Int
    walk v h | x == size && y == size = c
             | otherwise = walk v' $ H.union h' $ H.fromList n
      where
        ([pos@(Position p@(x, y) c)], h') = H.splitAt 1 h
        n = nexts v pos
        v' = L.foldl' (\acc (Position a b) -> M.insert a b acc) v n
    nexts :: Visited -> Position -> [Position]
    nexts v (Position p c) = L.filter (valid v) . map (\p' -> Position p' (c+1)) $ candidates p
    valid :: Visited -> Position -> Bool
    valid v (Position p@(x, y) c) = x >= 0 && x <= size && y >= 0 && y <= size && not (M.member p maze) && case M.lookup p v of
      Just c' -> c < c'
      Nothing -> True
    candidates :: Coord -> [Coord]
    candidates (x, y) = [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]
    maze = M.fromList $ zip (take cutoff input) (L.repeat ())

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 6 12 example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute 70 1024 input
