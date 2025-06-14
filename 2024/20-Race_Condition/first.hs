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

exampleExpectedOutput = 5

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
compute input minGains = trace (show findAllShortcutCosts) $ length $ filter (>= minGains) findAllShortcutCosts
  where
    findAllShortcutCosts :: [Int]
    findAllShortcutCosts = M.foldrWithKey findShortcutCostsFrom [] costs
    findShortcutCostsFrom :: Coord -> Int -> [Int] -> [Int]
    findShortcutCostsFrom (x, y) c acc = acc ++ catMaybes [ if and [input V.! y V.! (x+1) == Wall, M.member (x+2, y) costs] then Just (costs M.! (x+2, y) - c - 2) else Nothing
                                                          , if and [input V.! y V.! (x-1) == Wall, M.member (x-2, y) costs] then Just (costs M.! (x-2, y) - c - 2) else Nothing
                                                          , if and [input V.! (y+1) V.! x == Wall, M.member (x, y+2) costs] then Just (costs M.! (x, y+2) - c - 2) else Nothing
                                                          , if and [input V.! (y-1) V.! x == Wall, M.member (x, y-2) costs] then Just (costs M.! (x, y-2) - c - 2) else Nothing]
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
  let exampleOutput = compute example 20
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input 100
