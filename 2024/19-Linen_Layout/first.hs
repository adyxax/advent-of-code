-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import           Data.Maybe
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 6

type Input = ([String], [String]) -- patterns, designs

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = (,) <$> some (some letterChar <* optional (string ", ")) <* eol <* eol
                  <*> some (some letterChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (stripes, designs) = length . filter id $ map compute' designs
  where
    compute' :: String -> Bool
    compute' [] = True
    compute' design = or $ map compute' $ catMaybes $ map (\s -> L.stripPrefix s design) stripes

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
