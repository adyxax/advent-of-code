-- requires cabal install --lib megaparsec parser-combinators heap vector
-- very slow with runghc, use ghc -O3 -o second second.hs and get the result in seconds
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 11387

data Equation = Equation Int [Int] deriving Show
type Input = [Equation]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseEquation :: Parser Equation
parseEquation = Equation <$> parseNumber <* string ": "
                         <*> some parseNumber

parseInput' :: Parser Input
parseInput' = some (parseEquation <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum . map total . filter valid
  where
    total (Equation t _) = t
    valid (Equation total numbers) = elem total $ combinations numbers
    combinations [x] = [x]
    combinations l = let x = last l
                         xs = init l
                     in concatMap (\n -> [x+n, x*n, read (show n ++ show x)]) $ combinations xs

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
