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

example1ExpectedOutput = 80
example2ExpectedOutput = 436
example3ExpectedOutput = 1206
example4ExpectedOutput = 236
example5ExpectedOutput = 368

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
type Region = [Coord]

compute :: Input -> Int
compute input = sum $ map (\r -> corners r * length r) regions
  where
    (regions, _) = V.ifoldl' regions' ([], M.empty) input
    regions' :: ([Region], Visited) -> Int -> Line -> ([Region], Visited)
    regions' acc y line = VU.ifoldl' (regions'' y) acc line
    regions'' :: Int -> ([Region], Visited) -> Int -> Char -> ([Region], Visited)
    regions'' y acc@(rs, visited) x c = case M.lookup (x, y) visited of
      Just _  -> acc  -- already processed
      Nothing -> let (r, visited') = flood c ([], visited) (x, y) in (r:rs, visited')
    flood :: Char -> (Region, Visited) -> (Int, Int) -> (Region, Visited)
    flood c acc@(rs, visited) (x, y) = case M.lookup (x, y) visited of
      Just _ -> acc
      Nothing -> L.foldl' (flood c) ((x, y):rs, M.insert (x, y) () visited) $ neighbors visited c x y
    neighbors :: Visited -> Char -> Int -> Int -> [Coord]
    neighbors visited c x y = filter (valid visited c) [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]
    valid :: Visited -> Char -> (Int, Int) -> Bool
    valid visited c (x, y) = case input V.!? y of
      Just line -> case line VU.!? x of
        Just c' -> c == c'
        Nothing -> False
      Nothing -> False
    corners :: Region -> Int
    corners r = sum $ map corners' r
      where
        inside c = L.elem c r
        outside = not . inside
        corners' (x, y) = length $ filter id [ outside (x-1, y) && outside (x, y-1)   -- top left in
                                             , outside (x+1, y) && outside (x, y-1)   -- top right in
                                             , outside (x-1, y) && outside (x, y+1)   -- bottom left in
                                             , outside (x+1, y) && outside (x, y+1)   -- bottom right in
                                             , inside (x-1, y) && outside (x-1, y-1) && inside (x, y-1)   -- top left out
                                             , inside (x+1, y) && outside (x+1, y-1) && inside (x, y-1)   -- top right out
                                             , inside (x-1, y) && outside (x-1, y+1) && inside (x, y+1)   -- bottom left out
                                             , inside (x+1, y) && outside (x+1, y+1) && inside (x, y+1) ] -- bottom right out

main :: IO ()
main = do
  example1 <- parseInput "example1"
  let exampleOutput = compute example1
  when  (exampleOutput /= example1ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example1ExpectedOutput)
  example2 <- parseInput "example2"
  let exampleOutput = compute example2
  when  (exampleOutput /= example2ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example2ExpectedOutput)
  example4 <- parseInput "example4"
  let exampleOutput = compute example4
  when  (exampleOutput /= example4ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example4ExpectedOutput)
  example5 <- parseInput "example5"
  let exampleOutput = compute example5
  when  (exampleOutput /= example5ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example5ExpectedOutput)
  example3 <- parseInput "example3"
  let exampleOutput = compute example3
  when  (exampleOutput /= example3ExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show example3ExpectedOutput)
  input <- parseInput "input"
  print $ compute input
