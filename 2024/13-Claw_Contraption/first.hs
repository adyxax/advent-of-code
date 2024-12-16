-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Either
import qualified Data.Matrix          as MTX
import           Data.Ratio
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 480

type Pair = (Rational, Rational) -- X Y
type Entry = (Pair, Pair, Pair) -- ButtonA ButtonB Prize
type Input = [Entry]

type Parser = Parsec Void String

parseNumber :: Parser Rational
parseNumber = fromInteger . read <$> some digitChar

parseButton :: Parser Pair
parseButton = (,) <$> (string "Button " *> letterChar *> string ": X+" *> parseNumber)
                  <*> (string ", Y+" *> parseNumber <* eol)

parsePrize :: Parser Pair
parsePrize = (,) <$> (string "Prize: X=" *> parseNumber)
                 <*> (string ", Y=" *> parseNumber <* eol)

parseEntry :: Parser Entry
parseEntry = (,,) <$> parseButton
                  <*> parseButton
                  <*> parsePrize

parseInput' :: Parser Input
parseInput' = some (parseEntry <* optional eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

-- ax + bx = c
-- ay + by = d
compute :: Input -> Rational
compute = sum . map compute'
  where
    compute' ((ax, ay), (bx, by), (px, py)) = let (Right sol) = MTX.rref $ MTX.fromList 2 3 [ ax, bx, px
                                                                                            , ay, by, py ]
                                                  x = sol MTX.! (1, 3)
                                                  y = sol MTX.! (2, 3)
                                              in if (denominator x == 1 && denominator y == 1) then 3 * x + y else 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
