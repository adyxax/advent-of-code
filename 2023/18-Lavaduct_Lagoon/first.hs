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

exampleExpectedOutput = 62

data Heading = U | D | L | R deriving (Eq, Show)
data Line = Line Heading Int String
type Input = [Line]

type Parser = Parsec Void String

parseHeading :: Parser Heading
parseHeading = char 'U' $> U
           <|> char 'D' $> D
           <|> char 'L' $> L
           <|> char 'R' $> R

parseLine :: Parser Line
parseLine = Line <$> parseHeading <* space
                 <*> (read <$> some digitChar) <* space
                 <*> (string "(#" *> some alphaNumChar <* char ')' <* eol)

parseInput' :: Parser Input
parseInput' = some parseLine <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Map = M.Map (Int, Int) Bool
type Plan = (Map, Int, Int)

compute :: Input -> Int
compute input = xlen * ylen - M.size (M.filter not floodedPit) -- L.foldl' countLine 0 [ymin..ymax]
  where
    -- filling
    (xs, ys) = unzip $ M.keys trenches
    xmin = minimum xs - 1
    xmax = maximum xs + 1
    xlen = xmax - xmin + 1
    ymin = minimum ys - 1
    ymax = maximum ys + 1
    ylen = ymax - ymin + 1
    floodedPit = flood (xmin, ymin) trenches
    flood (x, y) m | x < xmin || x > xmax || y < ymin || y > ymax = m
                   | M.member (x, y) m = m
                   | otherwise = flood (x-1, y) $ flood (x+1, y) $ flood (x, y-1) $ flood (x, y+1) $ M.insert (x, y) False m
    ---- digging
    (trenches, _, _) = L.foldl' digOne (M.singleton (0, 0) True, 0, 0) input
    digOne :: Plan-> Line -> Plan
    digOne (m, x, y) (Line h l _) = (digTrench m h l, x', y')
      where
        digTrench :: Map -> Heading -> Int -> Map
        digTrench m _ (-1) = m
        digTrench m U i    = digTrench (M.insert (x, y-i) True m) U (i - 1)
        digTrench m D i    = digTrench (M.insert (x, y+i) True m) D (i - 1)
        digTrench m L i    = digTrench (M.insert (x-i, y) True m) L (i - 1)
        digTrench m R i    = digTrench (M.insert (x+i, y) True m) R (i - 1)
        x' | h == U = x
           | h == D = x
           | h == L = x - l
           | h == R = x + l
        y' | h == U = y - l
           | h == D = y + l
           | h == L = y
           | h == R = y

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
