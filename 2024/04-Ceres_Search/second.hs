-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as VU
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 9

type Line = VU.Vector Char
type Input = V.Vector Line

type Parser = Parsec Void String

parseLine :: Parser Line
parseLine = do
  line <- some letterChar <* eol
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
compute g = length $ filter xmas as
  where
    l = V.length g
    as = [(x, y) | x<-[1..l-2], y<-[1..l-2], g V.! x VU.! y == 'A']
    xmas :: (Int, Int) -> Bool
    xmas (x, y) = d1 && d2
      where
        tl = g V.! (x-1) VU.! (y-1)
        tr = g V.! (x+1) VU.! (y-1)
        bl = g V.! (x-1) VU.! (y+1)
        br = g V.! (x+1) VU.! (y+1)
        d1 = tl == 'M' && br == 'S' || tl == 'S' && br == 'M'
        d2 = bl == 'M' && tr == 'S' || bl == 'S' && tr == 'M'

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
