-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 143

type Rule = (Int, Int)
type Update = [Int]
data Input = Input [Rule] [Update] deriving Show

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar

parseRule :: Parser Rule
parseRule = (,) <$> parseNumber <* char '|'
                <*> parseNumber <* eol

parseUpdate :: Parser Update
parseUpdate = some (parseNumber <* optional (char ',')) <* eol

parseInput' :: Parser Input
parseInput' = Input <$> some parseRule <* eol
                    <*> some parseUpdate <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input rules updates) = L.foldl' compute' 0 updates
  where
    compute' acc update | valid update = acc + update !! (length update `div` 2)
                        | otherwise = acc
    valid update = all (valid' update) rules
    valid' update (x, y) = case (L.elemIndices x update, L.elemIndices y update) of
      ([i], [j]) -> i < j
      _          -> True

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
