-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import           Data.Maybe
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 48

data Op = Mul Int Int | Do | Dont | Nop deriving (Eq, Show)
type Input = [Op]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseMul :: Parser Op
parseMul = Mul <$> (string "mul(" *> parseNumber)
               <*> (char ',' *> parseNumber <* char ')')

parseOp :: Parser Op
parseOp = try parseMul
      <|> string "do()" $> Do
      <|> string "don't()" $> Dont
      <|> anySingle $> Nop

parseInput' :: Parser Input
parseInput' = filter (/= Nop) <$> some parseOp <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute = fst . L.foldl' compute' (0, True)
  where
    compute' :: (Int, Bool) -> Op -> (Int, Bool)
    compute' (acc, False) Do        = (acc, True)
    compute' (acc, True)  Dont      = (acc, False)
    compute' (acc, True)  (Mul x y) = (acc + (x * y), True)
    compute' acc _                  = acc

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
