-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

exampleExpectedOutput = 2286

data Draw = Draw Int Int Int deriving (Eq, Show)
data Game = Game Int [Draw] deriving Show
type Input = [Game]

type Parser = Parsec Void String

parseColor :: String -> Parser Int
parseColor color = read <$> try (some digitChar <* char ' ' <* string color <* optional (string ", "))

parseDraw :: Parser Draw
parseDraw = do
  (blue, green, red) <- runPermutation $
    (,,) <$> toPermutationWithDefault 0 (parseColor "blue")
         <*> toPermutationWithDefault 0 (parseColor "green")
         <*> toPermutationWithDefault 0 (parseColor "red")
  void . optional $ string "; "
  return $ Draw blue green red

parseGame :: Parser Game
parseGame = do
  id <- read <$> (string "Game " *> some digitChar <* optional (string ": "))
  Game id <$> someTill parseDraw eol

parseInput' :: Parser Input
parseInput' = some parseGame <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

power :: [Int] -> Draw -> [Int]
power [b, g, r] (Draw b' g' r') = [max b b', max g g', max r r']

process :: Int -> Game -> Int
process acc (Game id draws) = acc + product (L.foldl' power [0, 0, 0] draws)

compute :: Input -> Int
compute = L.foldl' process 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input

