-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 3263827

data Op = Add | Mul deriving (Eq, Show)
type Input = [Int]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> (some digitChar <* optional hspace)

parseOp' :: Parser Op
parseOp' = char '+' $> Add
       <|> char '*' $> Mul

parseOp :: Parser Int
parseOp = do
  n <- optional hspace *> parseNumber
  op <- parseOp' <* eol
  ns <- some (optional hspace *> parseNumber <* eol <* optional hspace)
  pure $ case op of
    Add -> sum $ n:ns
    Mul -> product $ n:ns

parseInput' :: Parser Input
parseInput' = some (parseOp <* optional eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- lines <$> readFile filename
  let len = maximum $ map length input
      input' = map complete input
      complete s = s ++ take (len - length s) (repeat ' ')
      input'' = unlines $ L.transpose input'
  case runParser parseInput' filename input'' of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
