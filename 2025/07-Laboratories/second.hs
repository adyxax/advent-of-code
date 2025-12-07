-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 40

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
compute (s:ls) = total $ foldl' compute' (M.fromList $ zip (S.toList s) [1]) ls
  where
    total :: M.Map Int Int -> Int
    total = M.foldl' (+) 0
    compute' :: M.Map Int Int -> S.Set Int -> M.Map Int Int
    compute' acc splitters = let splits = S.intersection (S.fromList $ M.keys acc) splitters
                                 continuingBeams = M.difference acc $ M.fromList $ zip (S.toList splits) $ repeat 0
                             in S.foldl' (split acc) continuingBeams splits
    split :: M.Map Int Int -> M.Map Int Int -> Int -> M.Map Int Int
    split origin acc i = let v = origin M.! i
                             acc' = case M.lookup (i+1) acc of
                                      Just n -> M.insert (i+1) (n+v) acc
                                      Nothing -> M.insert (i+1) v acc
                         in case M.lookup (i-1) acc of
                              Just n -> M.insert (i-1) (n+v) acc'
                              Nothing -> M.insert (i-1) v acc'

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
