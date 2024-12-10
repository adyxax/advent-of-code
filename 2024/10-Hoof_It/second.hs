-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 81

type Line = VU.Vector Int
type Input = V.Vector Line

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read . pure <$> digitChar

parseLine :: Parser Line
parseLine = do
  line <- some parseNumber <* eol
  return $ VU.generate (length line) (line !!)

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

compute :: Input -> Int
compute input = sum $ V.imap scoreLine input
  where
    scoreLine :: Int -> VU.Vector Int -> Int
    scoreLine y line = VU.sum $ VU.imap (\x c -> score x y c) line
    score :: Int -> Int -> Int -> Int
    score x y c | c == 0 = length $ reachableSummits 0 (x, y)
                | otherwise = 0
    reachableSummits :: Int -> (Int, Int) -> [(Int, Int)]
    reachableSummits h (x, y) | h == 9 = [(x, y)]
                              | otherwise = concatMap (reachableSummits (h+1)) succ
      where
        succ = filter valid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        valid (x, y) = case input V.!? y of
          Just line -> case line VU.!? x of
            Just h' -> h+1 == h'
            _       -> False
          _ -> False

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
