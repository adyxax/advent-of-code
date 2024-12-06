-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 41

data Tile = Floor | Wall | Guard deriving (Eq, Show)
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseTile :: Parser Tile
parseTile = char '.' $> Floor
          <|> char '#' $> Wall
          <|> char '^' $> Guard

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

type Visited = M.Map (Int, Int) ()
data Direction = N | S | E | W
next (x, y) N = (x, y-1)
next (x, y) S = (x, y+1)
next (x, y) W = (x-1, y)
next (x, y) E = (x+1, y)
nextRot N = E
nextRot E = S
nextRot S = W
nextRot W = N

compute :: Input -> Int
compute input = M.size $ step (startx, starty) N M.empty
  where
    Just (starty, Just startx) = let startx (y, v) = (y, V.elemIndex Guard v)
                                 in V.find (isJust . snd) $ V.map startx $ V.indexed input
    step :: (Int, Int) -> Direction -> Visited -> Visited
    step (x, y) h v | ahead == Nothing = v'
                    | ahead == Just Wall = step (x, y) (nextRot h) v'
                    | otherwise = step (x', y') h v'
      where
        v' = M.insert (x, y) () v
        (x', y') = next (x, y) h
        ahead = case input V.!? y' of
          Just l  -> l V.!? x'
          Nothing -> Nothing

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
