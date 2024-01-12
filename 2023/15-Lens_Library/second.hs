-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 145

data Op = Equal Int | Minus deriving (Eq, Show)
data Step = Step String Op deriving (Eq, Show)
type Input = [Step]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseOp :: Parser Op
parseOp = char '-' $> Minus
      <|> char '=' *> (Equal <$> parseNumber)

parseStep :: Parser Step
parseStep = Step <$> some letterChar
                 <*> parseOp <* optional (char ',')

parseInput' :: Parser Input
parseInput' = some parseStep <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

data Lens = Lens String Int deriving (Eq, Show)
type Box = [Lens]

compute :: Input -> Int
compute input = sum $ zipWith (curry score) [1..] $ L.foldl' step initialBoxes input
  where
    score :: (Int, Box) -> Int
    score (i, lenses) = sum $ zipWith (curry $ scoreLens i) [1 .. ] lenses
    scoreLens :: Int -> (Int, Lens) -> Int
    scoreLens i (j, Lens _ k) = i * j * k
    initialBoxes = replicate 256 []
    step :: [Box] -> Step -> [Box]
    step boxes (Step label Minus) = take i boxes
                                 ++ remove [] (boxes L.!! i)
                                  : drop (i+1) boxes
      where
        i = hash 0 label
        remove :: Box -> Box -> Box
        remove out [] = out
        remove out (l@(Lens ll _):ls) | ll == label = remove out ls
                                      | otherwise = remove (out ++ [l]) ls
    step boxes (Step label (Equal f)) = take i boxes
                                     ++ process [] (boxes L.!! i)
                                      : drop (i+1) boxes
      where
        i = hash 0 label
        process :: Box -> Box -> Box
        process out [] = out ++ [Lens label f]
        process out (l@(Lens ll _):ls) | ll == label = out ++ Lens label f : ls
                                       | otherwise = process (out ++ [l]) ls
    hash :: Int -> String -> Int
    hash i [] = i
    hash i (x:xs) = hash (((i + C.ord x) * 17) `rem` 256) xs

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
