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
import           Data.Ratio
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 2

type Hail = (Rational, Rational, Rational, Rational, Rational, Rational)
type Input = [Hail]

type Parser = Parsec Void String

parseNumber :: Parser Integer
parseNumber = read <$> some (char '-' <|> digitChar) <* optional (char ',') <* optional hspace <* optional (char '@' <* hspace)

parseHail :: Parser Hail
parseHail = (,,,,,) <$> (fromInteger <$> parseNumber)
                    <*> (fromInteger <$> parseNumber)
                    <*> (fromInteger <$> parseNumber)
                    <*> (fromInteger <$> parseNumber)
                    <*> (fromInteger <$> parseNumber)
                    <*> (fromInteger <$> parseNumber)

parseInput' :: Parser Input
parseInput' = some (parseHail <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Rational -> Rational -> Input -> Int
compute mini maxi = compute'
  where
    compute' :: Input -> Int
    compute' (_:[]) = 0
    compute' ((x1, y1, _, a1, b1, _):hs) = L.foldl' computeOne 0 hs + compute' hs
      where
        (x2, y2) = (x1 + a1, y1 + b1)
        computeOne :: Int -> Hail -> Int
        computeOne acc (x3, y3, _, a3, b3, _) | valid = acc + 1
                                              | otherwise = acc
          where
            valid | denominator == 0 || t < 0 || u < 0 = False
                  | x >= mini && x <= maxi && y >= mini && y <= maxi = True
                  | otherwise = False
            (x4, y4) = (x3 + a3, y3 + b3)
            denominator = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
            a = x1 * y2 - y1 * x2
            b = x3 * y4 - y3 * x4
            numeratorX = b * a1 - a * a3
            numeratorY = b * b1 - a * b3
            (x, y) = (numeratorX / denominator, numeratorY / denominator)
            (t, u) = ((x - x1) / a1, (x - x3) / a3)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 7 27 example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute 200000000000000 400000000000000 input
