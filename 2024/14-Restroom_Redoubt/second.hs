-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Pair = (Int, Int)
type Robot = (Pair, Pair)
type Input = [Robot]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some (digitChar <|> char '-')

parsePair :: Parser Pair
parsePair = (,) <$> parseNumber <* char ','
                <*> parseNumber

parseRobot :: Parser Robot
parseRobot = (,) <$> (string "p=" *> parsePair)
                 <*> (string " v=" *> parsePair)

parseInput' :: Parser Input
parseInput' = some (parseRobot <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Int -> Int -> Input -> Int -- the minimum safety score is the answer
compute width height input = fst $ L.foldl' (\acc@(_, a) n@(_, b) -> if a > b then n else acc) (0, 1000000000) safeties
  where
    safeties = [(i, safety i)|i<-[0..width*height]]
    mx = width `div` 2
    my = height `div` 2
    safety i = product $ L.foldl' score [0, 0, 0, 0] $ map (step i) input
    step i ((x, y), (v, w)) = (((x + v * i) `mod` width), ((y + w * i) `mod` height))
    score acc@[a, b, c, d] (x, y) | x < mx && y < my = [a+1, b, c, d]
                                  | x > mx && y < my = [a, b+1, c, d]
                                  | x < mx && y > my = [a, b, c+1, d]
                                  | x > mx && y > my = [a, b, c, d+1]
                                  | otherwise = acc

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute 101 103 input
