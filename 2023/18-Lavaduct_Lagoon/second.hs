-- requires cabal install --lib megaparsec parser-combinators heap vector text
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import           Data.Bifunctor                   (bimap)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.Text                        as T
import qualified Data.Text.Read                   as T
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 952408144115

data Heading = U | D | L | R deriving (Eq, Show)
data Line = Line Int Heading
type Input = [Line]

type Parser = Parsec Void String

parseHeading :: Parser Heading
parseHeading = char '3' $> U
           <|> char '1' $> D
           <|> char '2' $> L
           <|> char '0' $> R

parseLine :: Parser Line
parseLine = Line <$> (letterChar *> space *> some digitChar *> string " (#" *> (fst . fromRight (0, T.pack "") . T.hexadecimal . T.pack <$> count 5 alphaNumChar))
                 <*> (parseHeading <* char ')' <* eol)

parseInput' :: Parser Input
parseInput' = some parseLine <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

pick_theorem :: [(Int, Int)] -> Int
pick_theorem vertices = 1 + perimeter `div` 2 + (abs . (`div` 2) . sum . zipWith crossProduct vertices $ tail vertices)
    where crossProduct (x1, y1) (x2, y2) = y1 * x2 - x1 * y2
          dist         (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
          perimeter = sum . zipWith dist vertices $ tail vertices

compute :: Input -> Int
compute input = pick_theorem $ (0, 0) : L.foldl' coords [(0, 0)] input
  where
    coords :: [(Int, Int)] -> Line -> [(Int, Int)]
    coords acc@((x, y):_) (Line l h) | h == U = (x, y - l):acc
                                     | h == D = (x, y + l):acc
                                     | h == L = (x - l, y):acc
                                     | h == R = (x + l, y):acc

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
