-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 9021

data Tile = Wall | Box | Lbox | Rbox | Floor | Robot deriving (Eq, Show)
type Line = V.Vector Tile
type Warehouse = V.Vector Line
data Op = N | S | E | W deriving (Eq, Show)
data Input = Input Warehouse [Op] deriving Show

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '#' $> Wall
        <|> char 'O' $> Box
        <|> char '.' $> Floor
        <|> char '@' $> Robot

parseLine :: Parser Line
parseLine = do
  line <- some parseTile <* eol
  return $ V.generate (length line) (line !!)

parseOp :: Parser Op
parseOp = char '^' $> N
      <|> char 'v' $> S
      <|> char '>' $> E
      <|> char '<' $> W

parseInput' :: Parser Input
parseInput' = do
  line <- some parseLine <* eol
  ops <- some (parseOp <* optional eol) <* eof
  return $ Input (V.generate (length line) (line !!)) ops

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Coord = (Int, Int)

next :: Coord -> Op -> Coord
next (x, y) N = (x, y-1)
next (x, y) S = (x, y+1)
next (x, y) E = (x+1, y)
next (x, y) W = (x-1, y)

showWarehouse :: Warehouse -> String
showWarehouse w = V.foldl' showOne [] w
showOne acc line = acc ++ (V.foldl' showTile [] line) ++ "\n"
showTile acc Wall  = acc ++ "#"
showTile acc Lbox  = acc ++ "["
showTile acc Rbox  = acc ++ "]"
showTile acc Floor = acc ++ "."
showTile acc Robot = acc ++ "@"
showTile acc Box   = acc ++ "O"

compute :: Input -> Int
compute (Input warehouse ops) = V.ifoldl' scoreBoxes 0 warehouse''
  where
    scoreBoxes :: Int -> Int -> Line -> Int
    scoreBoxes acc y line = V.ifoldl' (scoreBox y) acc line
    scoreBox :: Int -> Int -> Int -> Tile -> Int
    scoreBox y acc x Lbox = acc + 100 * y + x
    scoreBox _ acc _ _    = acc
    warehouse'' = fst $ L.foldl' step (warehouse', start) ops
    step :: (Warehouse, Coord) -> Op -> (Warehouse, Coord)
    step a@(w, r@(x, y)) op | t == Wall = a
                            | t == Lbox = case push w r' op of
                                Just w' -> (w', r')
                                Nothing -> a
                            | (op == N || op == S) && t == Rbox = case push w (x'-1, y') op of -- we want to always push boxes from their left side to reduce push cases
                                Just w' ->(w', r')
                                Nothing -> a
                            | t == Rbox = case push w (x', y') op of
                                Just w' -> (w', r')
                                Nothing -> a
                            | otherwise = (w, (x', y'))
      where
        r'@(x', y') = next r op
        t = w V.! y' V.! x'
    push :: Warehouse -> Coord -> Op -> Maybe Warehouse
    push w r@(x, y) op | t == Wall = Nothing
                       | (op == N || op == S) && tr == Wall = Nothing
                       | (op == N || op == S) && t == Lbox = case push w (x, y') op of -- pushing a boxes that matches ours
                           Just w' -> let l1 = w' V.! y
                                          l1' = l1 V.// [(x, Floor), (x+1, Floor)]
                                          l2 = w' V.! y'
                                          l2' = l2 V.// [(x, Lbox), (x+1, Rbox)]
                                      in Just (w' V.// [(y, l1'), (y', l2')])
                           Nothing -> Nothing
                       | (op == N || op == S) && t == Rbox = case push w (x-1, y') op of
                           Just w' -> if tr == Lbox then case push w' (x+1, y') op of -- are we pushing two boxes?
                                                           Just w'' -> let l1 = w'' V.! y
                                                                           l1' = l1 V.// [(x, Floor), (x+1, Floor)]
                                                                           l2 = w'' V.! y'
                                                                           l2' = l2 V.// [(x, Lbox), (x+1, Rbox)]
                                                                       in Just (w'' V.// [(y, l1'), (y', l2')])
                                                           Nothing -> Nothing
                                                    else let l1 = w' V.! y -- or just one on our left
                                                             l1' = l1 V.// [(x, Floor), (x+1, Floor)]
                                                             l2 = w' V.! y'
                                                             l2' = l2 V.// [(x, Lbox), (x+1, Rbox)]
                                                         in Just (w' V.// [(y, l1'), (y', l2')])
                           Nothing -> Nothing
                       | (op == N || op == S) && tr == Lbox = case push w (x+1, y') op of -- or just one on our right
                           Just w' -> let l1 = w' V.! y
                                          l1' = l1 V.// [(x, Floor), (x+1, Floor)]
                                          l2 = w' V.! y'
                                          l2' = l2 V.// [(x, Lbox), (x+1, Rbox)]
                                      in Just (w' V.// [(y, l1'), (y', l2')])
                           Nothing -> Nothing
                       | (op == N || op == S) = let l1 = w V.! y -- free space
                                                    l1' = l1 V.// [(x, Floor), (x+1, Floor)]
                                                    l2 = w V.! y'
                                                    l2' = l2 V.// [(x, Lbox), (x+1, Rbox)]
                                                in Just (w V.// [(y, l1'), (y', l2')])
                       | t == Lbox || t == Rbox = case push w (x', y) op of -- East-West movements are simpler
                           Just w' -> let l = w' V.! y
                                          l' = l V.// [(x, Floor), (x', to)]
                                      in Just (w' V.// [(y, l')])
                           Nothing -> Nothing
                       | otherwise = let l = w V.! y -- free space
                                         l' = l V.// [(x, Floor), (x', to)]
                                     in Just (w V.// [(y, l')])
      where
        (x', y') = next r op
        t = w V.! y' V.! x'
        tr = w V.! y' V.! (x'+1)
        to = w V.! y V.! x
    start = V.ifoldl' findRobot (0, 0) warehouse'
    findRobot :: (Int, Int) -> Int -> Line -> (Int, Int)
    findRobot (0, _) y line = (V.ifoldl' findRobotInLine 0 line, y)
    findRobot a _ _         = a
    findRobotInLine :: Int -> Int -> Tile -> Int
    findRobotInLine 0 x Robot = x
    findRobotInLine a _ _     = a
    wideWidth = 2 * V.length (warehouse V.! 0)
    warehouse' = V.map widen warehouse
    widen line = V.ifoldl' widenOne (V.replicate wideWidth Floor) line
    widenOne acc x Wall  = acc V.// [(2*x, Wall), (2*x+1, Wall)]
    widenOne acc x Box   = acc V.// [(2*x, Lbox), (2*x+1, Rbox)]
    widenOne acc x Robot = acc V.// [(2*x, Robot)]
    widenOne acc _ _     = acc

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
