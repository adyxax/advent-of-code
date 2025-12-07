-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 21

type Input = [S.Set Int]
type Parser = Parsec Void String

parseTile :: Parser (Maybe Int)
parseTile = do
  SourcePos _ _ x <- getSourcePos
  char '.' $> Nothing <|> (char '^' <|> char 'S') $> Just (unPos x)

parseLine :: Parser (S.Set Int)
parseLine = S.fromList . catMaybes <$> some parseTile

parseInput' :: Parser Input
parseInput' = some (parseLine <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (s:ls) = fst $ foldl' compute' (0, s) ls
  where
    compute' :: (Int, S.Set Int) -> S.Set Int -> (Int, S.Set Int)
    compute' (acc, beams) splitters = let splits = S.intersection beams splitters
                                          newBeams = S.foldl' split S.empty splits
                                          continuingBeams = S.difference beams splitters
                                      in (acc + S.size splits, S.union continuingBeams newBeams)
    split :: S.Set Int -> Int -> S.Set Int
    split acc i = S.union acc $ S.fromList [i-1, i+1]

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
