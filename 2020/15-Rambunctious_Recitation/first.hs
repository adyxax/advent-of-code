-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 436

type Input = [Int]

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  void $ optional (char ',')
  return $ read n

parseOps :: Parser Input
parseOps = some parseInt <* char '\n' <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

compute :: Input -> Int
compute input = compute'  (length input)  (foldl' prepare M.empty $ zip (init input) [1..])  (last input)
  where
    compute' :: Int -> M.Map Int (Int, Int) -> Int -> Int
    compute' 2020 _ n = n
    compute' t m n = case M.lookup n m of
      Just (t', i) -> compute'  (t+1)  (M.insert n (t, i+1) m)  (t - t')
      Nothing -> compute'  (t+1)  (M.insert n (t, 0) m)  0
    prepare :: M.Map Int (Int, Int) -> (Int, Int) -> M.Map Int (Int, Int)
    prepare acc (n, t) = case M.lookup n acc of
      Just (_, i) -> M.insert n (t, i+1) acc
      Nothing -> M.insert n (t, 0) acc

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
