-- requires cabal install --lib megaparsec parser-combinators vector
module Main (main) where
import Control.Monad (void, when)
import Data.Functor
import Data.List qualified as L
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 6032

type Line = V.Vector Char
type Map = V.Vector Line
data Instruction = Move Int | L | R deriving Show
data Input = Input Map [Instruction] deriving Show

type Parser = Parsec Void String

parseMapLine :: Parser Line
parseMapLine = do
  line <- some (char '.' <|> char ' ' <|> char '#') <* char '\n'
  return $ V.generate (length line) (line !!)

parseMap :: Parser Map
parseMap = do
  lines <- some parseMapLine <* char '\n'
  return $ V.generate (length lines) (lines !!)

parseInstruction :: Parser Instruction
parseInstruction = (Move . read <$> some digitChar)
               <|> (char 'L' $> L)
               <|> (char 'R' $> R)

parseInput' :: Parser Input
parseInput' = do
  m <- parseMap
  i <- some parseInstruction
  void $ optional (char '\n') <* eof
  return $ Input m i

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

data Heading = N | S | E | W deriving Show
data Cursor = Cursor Int Int Heading

step :: Map -> Cursor -> Instruction -> Cursor
step _ (Cursor x y N) L = Cursor x y W
step _ (Cursor x y S) L = Cursor x y E
step _ (Cursor x y E) L = Cursor x y N
step _ (Cursor x y W) L = Cursor x y S
step _ (Cursor x y N) R = Cursor x y E
step _ (Cursor x y S) R = Cursor x y W
step _ (Cursor x y E) R = Cursor x y S
step _ (Cursor x y W) R = Cursor x y N
step m c (Move 0) = c
step m (Cursor x y h) (Move i) = case (m V.! y'') V.! x'' of
  '.' -> step m (Cursor x'' y'' h) (Move $ i - 1)
  _ -> Cursor x y h
  where
    (x', y') = case h of
      N -> (x, y-1)
      S -> (x, y+1)
      E -> (x+1, y)
      W -> (x-1, y)
    line = m V.! y
    xmax = length line - 1
    xmin = length (V.filter (== ' ') line)
    x'' | x' < xmin = xmax
        | x' > xmax = xmin
        | otherwise = x'
    ymaxLookup :: Int -> Int
    ymaxLookup ym = case (m V.! ym) V.!? x of
      Just '.' -> ym
      Just '#' -> ym
      _ -> ymaxLookup $ ym - 1
    yminLookup :: Int -> Int
    yminLookup ym = case (m V.! ym) V.!? x of
      Just ' ' -> yminLookup $ ym + 1
      _ -> ym
    ymax = ymaxLookup $ length m - 1
    ymin = yminLookup 0
    y'' | y' < ymin = ymax
        | y' > ymax = ymin
        | otherwise = y'

compute :: Input -> Int
compute (Input m i) = 1000 * (y+1) + 4 * (x+1) + hv
  where
    xmin = length (V.filter (== ' ') (m V.! 0))
    startingCursor = Cursor xmin 0 E
    Cursor x y h = L.foldl' (step m) startingCursor i
    hv = case h of
      E -> 0
      S -> 1
      W -> 2
      N -> 3

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
