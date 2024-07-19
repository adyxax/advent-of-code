-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

data Tile = Start | Plot | Rock deriving Eq
instance Show Tile where
  show Start = "S"
  show Plot  = "."
  show Rock  = "#"
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char 'S' $> Start
        <|> char '.' $> Plot
        <|> char '#' $> Rock

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

type Steps = M.Map (Int, Int) ()

-- 26501365 = 202300 * 131 + 65
compute :: Input -> Integer
compute input = let steps = compute' 65 start
                    steps' = compute' 131 steps
                    steps'' = compute' 131 steps'
                    steps''' = compute' 131 steps''
                    -- lagrange polynomial interpolation for the quadratic function
                    f :: Integer -> Integer
                    f x = let lamb xi  = product (map (\xj -> (x-xj)) (L.delete xi xs)) `div` product (map (\xj -> (xi-xj)) (L.delete xi xs))
                          in sum $ zipWith (*) ys (map lamb xs)
                    xs = toInteger <$> [x*131+65|x<-[0..3]]
                    ys = toInteger . M.size <$> [steps, steps', steps'', steps''']
                in f 26501365
  where
    compute' :: Int -> Steps -> Steps
    compute' 0 steps = steps
    compute' i steps = compute' (i-1) next
      where
        next :: Steps
        next = M.foldrWithKey nextSteps M.empty steps
        nextSteps :: (Int, Int) -> () -> Steps -> Steps
        nextSteps (x, y) _ = nextOne (x-1, y) . nextOne (x+1, y) . nextOne (x, y-1) . nextOne (x, y+1)
        nextOne :: (Int, Int) -> Steps -> Steps
        nextOne (x, y) acc = case input V.!? (y `mod` len) of
          Just line -> case line V.!? (x `mod` len) of
            Just Rock -> acc
            Just _    -> M.insert (x, y) () acc
            _         -> acc
          Nothing -> acc
    start = M.singleton (mid, mid) ()
    mid = len `div` 2
    len = V.length input

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
