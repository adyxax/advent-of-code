-- requires cabal install --lib megaparsec parser-combinators heap vector matrix
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import qualified Data.Matrix                      as MTX
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 47

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

-- rock in (x, y, z, a, b, c)
-- with a known hail (x1, y1, z1, a1, b1, x1) we have
-- x + a * t1 = x1 + a1 * t1
-- => t1 = (x - x1) / (a1 - a) = (y - y1) / (b1 - b)
-- => (x - x1)(b1 - b) = (y - y1)(a1 - a)
-- => xb1 - xb - x1b1 + x1b = ya1 - ya - y1a1 + y1a
-- => ya - xb = ya1 - y1a1 + y1a - xb1 + x1b1 - x1b
-- With a second hail we also get:
--    ya - xb = ya2 - y2a2 + y2a - xb2 + x2b2 - x2b
-- By substracting the two equations, we get:
-- => y(a1-a2) + a(y1-y2) - x(b1-b2) - b(x1-x2) = y1a1 - y2a2 - x1b1 + x2b2
-- => a(y1-y2) + b(x2-x1) + x(b2-b1) + y(a1-a2) = y1a1 - y2a2 - x1b1 + x2b2
-- We build 4 equations on this template and populate a matrix to triangularise
-- and solve for x and y, then repeat with z.
compute :: Input -> Rational
compute input = x + y + z
  where
    (x1, y1, z1, a1, b1, c1)
      :(x2, y2, z2, a2, b2, c2)
      :(x3, y3, z3, a3, b3, c3)
      :(x4, y4, z4, a4, b4, c4)
      :(x5, y5, z5, a5, b5, c5)
      :_ = input
    (Right mxy) = MTX.rref $ MTX.fromList 4 5 [ y1-y2, x2-x1, b2-b1, a1-a2, y1*a1-y2*a2-x1*b1+x2*b2
                                              , y1-y3, x3-x1, b3-b1, a1-a3, y1*a1-y3*a3-x1*b1+x3*b3
                                              , y1-y4, x4-x1, b4-b1, a1-a4, y1*a1-y4*a4-x1*b1+x4*b4
                                              , y1-y5, x5-x1, b5-b1, a1-a5, y1*a1-y5*a5-x1*b1+x5*b5 ]
    (Right mxz) = MTX.rref $ MTX.fromList 4 5 [ z1-z2, x2-x1, c2-c1, a1-a2, z1*a1-z2*a2-x1*c1+x2*c2
                                              , z1-z3, x3-x1, c3-c1, a1-a3, z1*a1-z3*a3-x1*c1+x3*c3
                                              , z1-z4, x4-x1, c4-c1, a1-a4, z1*a1-z4*a4-x1*c1+x4*c4
                                              , z1-z5, x5-x1, c5-c1, a1-a5, z1*a1-z5*a5-x1*c1+x5*c5 ]
    y = mxy MTX.! (4, 5)
    x = mxy MTX.! (3, 5)
    z = mxz MTX.! (4, 5)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
