-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 55312

type Input = [Int]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseInput' :: Parser Input
parseInput' = some parseNumber <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Memo = M.Map (Int, Int) Int

compute :: Input -> Int
compute input = fst $ L.foldl' (blink (75)) (0, M.empty) input
  where
    blink :: Int -> (Int, Memo) -> Int -> (Int, Memo)
    blink 0 (acc, m) _ = (acc + 1, m)
    blink i (acc, m) n = case M.lookup (n, i) m of
      Just n' -> (acc + n', m)
      Nothing -> let (n', m') = L.foldl' (blink (i-1)) (0, m) $ blinkOne n
                 in (acc + n', M.insert (n, i) n' m')
    blinkOne :: Int -> [Int]
    blinkOne 0 = [1]
    blinkOne n | m == 0 = [l, r]
               | otherwise = [n * 2024]
      where
        (d, m) = length (show n) `divMod` 2
        (l, r) = n `divMod` (10^d)

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
