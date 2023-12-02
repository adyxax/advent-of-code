-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

exampleExpectedOutput = 281

type Input = [(Int, Int)]

type Parser = Parsec Void String

parseDigit :: Parser Int
parseDigit = (C.digitToInt <$> digitChar)
         <|> (string "one" $> 1)
         <|> (string "two" $> 2)
         <|> (string "three" $> 3)
         <|> (string "four" $> 4)
         <|> (string "five" $> 5)
         <|> (string "six" $> 6)
         <|> (string "seven" $> 7)
         <|> (string "eight" $> 8)
         <|> (string "nine" $> 9)
         <|> (letterChar *> parseDigit)

parseDigitReversed :: Parser Int
parseDigitReversed = (C.digitToInt <$> digitChar)
                 <|> (string "eno" $> 1)
                 <|> (string "owt" $> 2)
                 <|> (string "eerht" $> 3)
                 <|> (string "ruof" $> 4)
                 <|> (string "evif" $> 5)
                 <|> (string "xis" $> 6)
                 <|> (string "neves" $> 7)
                 <|> (string "thgie" $> 8)
                 <|> (string "enin" $> 9)
                 <|> (letterChar *> parseDigitReversed)

parseFirstAndLast :: String -> (Int, Int)
parseFirstAndLast str = (f, l)
  where
    f = case runParser parseDigit "parseDigit" str of
      Left bundle -> error $ errorBundlePretty bundle
      Right input' -> input'
    l = case runParser parseDigitReversed "parseDigit" (reverse str) of
      Left bundle -> error $ errorBundlePretty bundle
      Right input' -> input'

parseInput' :: Parser Input
parseInput' = fmap parseFirstAndLast <$> some (some alphaNumChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

process :: Int -> (Int, Int) -> Int
process acc l = acc + fst l * 10 + snd l

compute :: Input -> Int
compute = L.foldl' process 0

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
