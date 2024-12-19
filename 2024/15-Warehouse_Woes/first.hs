-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 2028
example2ExpectedOutput = 10092

data Tile = Wall | Box | Floor | Robot deriving (Eq, Show)
type Line = V.Vector Tile
type Warehouse = V.Vector Line
data Op = N | S | E | W deriving Show
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

compute :: Input -> Int
compute (Input warehouse ops) = V.ifoldl' scoreBoxes 0 warehouse'
  where
    scoreBoxes :: Int -> Int -> Line -> Int
    scoreBoxes acc y line = V.ifoldl' (scoreBox y) acc line
    scoreBox :: Int -> Int -> Int -> Tile -> Int
    scoreBox y acc x Box = acc + 100 * y + x
    scoreBox _ acc _ _   = acc
    warehouse' = fst $ L.foldl' step (warehouse, start) ops
    step :: (Warehouse, Coord) -> Op -> (Warehouse, Coord)
    step a@(w, r@(x, y)) op | t == Wall = a
                            | t == Box = case push w r' op of
                                Just w' -> let line = w' V.! y'
                                               line' = line V.// [(x', Floor)]
                                               w'' = w' V.// [(y', line')]
                                           in (w'', r')
                                Nothing -> a
                            | otherwise = (w, (x', y'))
      where
        r'@(x', y') = next r op
        t = w V.! y' V.! x'
    push :: Warehouse -> Coord -> Op -> Maybe Warehouse
    push w r@(x, y) op | t == Wall = Nothing
                       | t == Box = push w (x', y') op
                       | otherwise = Just w'
      where
        (x', y') = next r op
        t = w V.! y' V.! x'
        line = w V.! y'
        line' = line V.// [(x', Box)]
        w' = w V.// [(y', line')]
    start = V.ifoldl' findRobot (0, 0) warehouse
    findRobot :: (Int, Int) -> Int -> Line -> (Int, Int)
    findRobot (0, _) y line = (V.ifoldl' findRobotInLine 0 line, y)
    findRobot a _ _         = a
    findRobotInLine :: Int -> Int -> Tile -> Int
    findRobotInLine 0 x Robot = x
    findRobotInLine a _ _     = a

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let example2Output = compute example2
  when  (example2Output /= example2ExpectedOutput)  (error $ "example2 failed: got " ++ show example2Output ++ " instead of " ++ show example2ExpectedOutput)
  input <- parseInput "input"
  print $ compute input
