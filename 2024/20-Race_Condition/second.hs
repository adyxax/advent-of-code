-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 3

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

type Coord = (Int, Int)
type Visited = M.Map Coord Int
type Cost = Int

compute :: Input -> Int -> Int
compute input minGain = findAllShortcut
  where
    findAllShortcut :: Int
    findAllShortcut = M.foldrWithKey findShortcutFrom 0 costs
    findShortcutFrom :: Coord -> Int -> Int -> Int
    findShortcutFrom (x, y) c acc = M.foldrWithKey validShortcut acc candidates
      where
        candidates = M.filterWithKey (\(x', y') _ -> (abs $ x - x') + (abs $ y - y') <= 20) costs
        validShortcut :: Coord -> Int -> Int -> Int
        validShortcut (x', y') c' acc' = let gain = c' - c - (abs $ x - x') - (abs $ y - y')
                                         in if gain >= minGain then acc' + 1 else acc'
    (costs, len) = walk M.empty 0 (sx, sy)
      where
        walk :: Visited -> Int -> Coord -> (Visited, Int)
        walk m c (x, y) | t == End = (m', c)
                        | otherwise = walk m' (c+1) next
          where
            [next] = filter (\p@(i, j) -> and [input V.! j V.! i /= Wall, not $ M.member p m']) candidates
            candidates = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
            m' = M.insert (x, y) c m
            t = input V.! y V.! x
    height = V.length input
    width = V.length (input V.! 0)
    sx = fst $ (V.filter (\(_, t) -> t == Start) $ V.indexed $ input V.! sy) V.! 0
    sy = fst $ (V.filter (\(_, v) -> V.length (V.filter (== Start) v) == 1) $ V.indexed input) V.! 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example 76
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  let exampleOutput2 = compute example 74
  when  (exampleOutput2 /= 7)  (error $ "example2 failed: got " ++ show exampleOutput2 ++ " instead of " ++ show 7)
  input <- parseInput "input"
  print $ compute input 100
