-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

example1ExpectedOutput = 140
example2ExpectedOutput = 772
example3ExpectedOutput = 1930

type Line = VU.Vector Char
type Input = V.Vector Line

type Parser = Parsec Void String

parseLine :: Parser Line
parseLine = do
  line <- some letterChar <* eol
  return $ VU.generate (length line) (line !!)

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
type Visited = M.Map Coord ()

compute :: Input -> Int
compute input = fst $ V.ifoldl' compute' (0, M.empty) input
  where
    compute' :: (Int, Visited) -> Int -> Line -> (Int, Visited)
    compute' (acc, visited) y line = VU.ifoldl' (compute'' y) (acc, visited) line
    compute'' :: Int -> (Int, Visited) -> Int -> Char -> (Int, Visited)
    compute'' y acc@(cost, visited) x c = case M.lookup (x, y) visited of
      Just _  -> acc  -- already processed
      Nothing -> let (area, perimeter, visited') = enclose c (0, 0, visited) (x, y) in (cost + area * perimeter, M.union visited visited')
    enclose :: Char -> (Int, Int, Visited) -> Coord -> (Int, Int, Visited)
    enclose c acc@(area, perimeter, visited) (x, y) = case M.lookup (x, y) visited of
      Just _ -> acc
      Nothing -> case input V.!? y of
        Just line -> case line VU.!? x of
          Just c' -> let n = neighbors visited c x y in L.foldl' (enclose c) (1 + area, 4 - length n + perimeter, M.insert (x, y) () visited) n
          Nothing -> (0, 0, visited)
        Nothing -> (0, 0, visited)
    neighbors :: Visited -> Char -> Int -> Int -> [Coord]
    neighbors visited c x y = filter (valid visited c) [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]
    valid :: Visited -> Char -> Coord -> Bool
    valid visited c (x, y) = case input V.!? y of
      Just line -> case line VU.!? x of
        Just c' -> c == c'
        Nothing -> False
      Nothing -> False

main :: IO ()
main = do
  example1 <- parseInput "example1"
  let exampleOutput = compute example1
  when  (exampleOutput /= example1ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example1ExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput = compute example2
  when  (exampleOutput /= example2ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example2ExpectedOutput)
  example3 <- parseInput "example3"
  let exampleOutput = compute example3
  when  (exampleOutput /= example3ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example3ExpectedOutput)
  input <- parseInput "input"
  print $ compute input
