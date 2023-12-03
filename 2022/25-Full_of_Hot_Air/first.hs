-- requires cabal install --lib megaparsec parser-combinators unordered-containers
module Main (main) where
import Control.Monad (void, when)
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

import Debug.Trace

exampleExpectedOutput = [Two, Equal, Minus, One, Equal, Zero] --"2=-1=0"

data Digit = Equal | Minus | Zero | One | Two deriving (Eq, Ord)
instance Show Digit where
  show Equal = "="
  show Minus = "-"
  show Zero = "0"
  show One = "1"
  show Two = "2"
type Snafu = [Digit]
type Input = [Snafu]

type Parser = Parsec Void String

parseDigit :: Parser Digit
parseDigit = (char '2' $> Two)
         <|> (char '1' $> One)
         <|> (char '0' $> Zero)
         <|> (char '-' $> Minus)
         <|> (char '=' $> Equal)

parseInput' :: Parser Input
parseInput' = some (some parseDigit <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

snafuToInt :: Snafu -> Int
snafuToInt = L.foldl' snafuToInt' 0
  where
    snafuToInt' :: Int -> Digit -> Int
    snafuToInt' acc Equal = acc * 5 - 2
    snafuToInt' acc Minus = acc * 5 - 1
    snafuToInt' acc Zero  = acc * 5
    snafuToInt' acc One   = acc * 5 + 1
    snafuToInt' acc Two   = acc * 5 + 2

intToSnafu :: Int -> Snafu
intToSnafu = intToSnafu' []
  where
    intToSnafu' :: Snafu -> Int -> Snafu
    intToSnafu' acc 0 = acc
    intToSnafu' acc num = let (d, m) = (num + 2) `divMod` 5
                              next = case m of
                                0 -> Equal
                                1 -> Minus
                                2 -> Zero
                                3 -> One
                                4 -> Two
                          in intToSnafu' (next : acc) d

compute :: Input -> Snafu
compute = intToSnafu . sum . map snafuToInt

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print . concatMap show $ compute input
