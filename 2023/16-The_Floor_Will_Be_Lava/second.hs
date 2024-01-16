-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 51

data Tile = Backslash | Empty | Horizontal | Slash | Vertical deriving Eq
instance Show Tile where
  show Backslash = "\\"
  show Empty = "."
  show Horizontal = "-"
  show Slash = "/"
  show Vertical = "|"
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '\\' $> Backslash
        <|> char '.' $> Empty
        <|> char '-' $> Horizontal
        <|> char '/' $> Slash
        <|> char '|' $> Vertical

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
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

type LineOutput = VU.Vector Bool
type Output = V.Vector LineOutput
data Heading = N | S | E | W deriving (Eq, Ord, Show)
data Cursor = Cursor Int Int Heading deriving (Eq, Ord, Show)

type Visited = M.Map Cursor ()

next :: Cursor -> Cursor
next (Cursor x y N) = Cursor x (y-1) N
next (Cursor x y S) = Cursor x (y+1) S
next (Cursor x y W) = Cursor (x-1) y W
next (Cursor x y E) = Cursor (x+1) y E

compute' :: Input -> Cursor -> Int
compute' input startingCursor = V.foldl' count 0 . fst $ walk startingOutput M.empty startingCursor
  where
    count :: Int -> LineOutput -> Int
    count i l = i + VU.length (VU.filter id l)
    startingOutput = V.replicate size $ VU.replicate size False
    size = V.length input
    walk :: Output -> Visited -> Cursor -> (Output, Visited)
    walk output v c | isNothing nextTile = (output, v)
                    | justNextTile == Empty = walk output' v c'
                    | visited = (output, v)
                    | justNextTile == Horizontal = if h == W || h == E then walk output' v c'
                                                                       else let (output'', v'') = walk output' v' (Cursor x y E) in walk output'' v'' $ Cursor x y W
                    | justNextTile == Vertical = if h == N || h == S then walk output' v c'
                                                                     else let (output'', v'') = walk output' v' (Cursor x y N) in walk output'' v'' $ Cursor x y S
                    | justNextTile == Backslash = case h of
                        N -> walk output' v' $ Cursor x y W
                        S -> walk output' v' $ Cursor x y E
                        W -> walk output' v' $ Cursor x y N
                        E -> walk output' v' $ Cursor x y S
                    | justNextTile == Slash = case h of
                        N -> walk output' v' $ Cursor x y E
                        S -> walk output' v' $ Cursor x y W
                        W -> walk output' v' $ Cursor x y S
                        E -> walk output' v' $ Cursor x y N
      where
        c'@(Cursor x y h) = next c
        nextTile = case input V.!? y of
          Just l -> l V.!? x
          Nothing -> Nothing
        justNextTile = fromJust nextTile
        line = output V.! y
        visited = M.member c' v
        v' = M.insert c' () v
        output' = if visited then output
                             else output V.// [(y, line VU.// [(x, True)])]

compute :: Input -> Int
compute input = maximum $ map (compute' input) cursors
  where
    cursors = [Cursor (-1) y E | y <- [0..l]]
           ++ [Cursor l y W | y <- [0..l]]
           ++ [Cursor x (-1) S | x <- [0..l]]
           ++ [Cursor x l N | x <- [0..l]]
    l = V.length input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
