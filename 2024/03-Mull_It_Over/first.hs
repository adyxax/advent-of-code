-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import           Data.Maybe
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 161

data Mul = Mul Int Int
type Input = [Mul]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseMul :: Parser (Maybe Mul)
parseMul = try (Just <$> (Mul <$> (string "mul(" *> parseNumber)
                              <*> (char ',' *> parseNumber <* char ')')))
       <|> anySingle $> Nothing

parseInput' :: Parser Input
parseInput' = catMaybes <$> some parseMul <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = sum . map (\(Mul x y) -> x * y)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
