-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 12

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

compute :: Int -> Int -> Input -> Int
compute width height input = product $ L.foldl' safety [0, 0, 0, 0] input'
  where
    mx = width `div` 2
    my = height `div` 2
    input' = map step input
    step ((x, y), (v, w)) = (((x + v * 100) `mod` width), ((y + w * 100) `mod` height))
    safety acc@[a, b, c, d] (x, y) | x < mx && y < my = [a+1, b, c, d]
                                   | x > mx && y < my = [a, b+1, c, d]
                                   | x < mx && y > my = [a, b, c+1, d]
                                   | x > mx && y > my = [a, b, c, d+1]
                                   | otherwise = acc

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 11 7 example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute 101 103 input
