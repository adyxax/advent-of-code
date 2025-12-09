-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 24

type Point = (Int, Int)
type Input = [Point]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parsePoint :: Parser Point
parsePoint = (,) <$> parseNumber <*> parseNumber

parseInput' :: Parser Input
parseInput' = some (parsePoint <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

compute :: Input -> Int
compute input@(p:ps) = maximum [area a b | a<-input, b<-input, inside a b] -- we are calculating the areas twice, but though wasteful this is fast enough
  where
    segments = zip input (ps ++ [p])
    -- our rectangle is inside the polygon if none of the segments that make the polygon are inside the rectangle
    inside :: Point -> Point -> Bool
    inside (x, y) (x', y') = all (not . strictlyContains) segments
      where
        strictlyContains ((sx, sy), (sx', sy')) = and [ min x x' < max sx sx'
                                                      , max x x' > min sx sx'
                                                      , min y y' < max sy sy'
                                                      , max y y' > min sy sy']

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
