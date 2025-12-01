-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Counter
exampleExpectedOutput = 6

type Counter = Int
data Direction = L | R deriving Show
type Position = Int
data Rotation = Rotation Direction Position

type Input = [Rotation]
type Parser = Parsec Void String

parseDirection :: Parser Direction
parseDirection = char 'L' *> pure L
             <|> char 'R' *> pure R

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional hspace

parseRotation :: Parser Rotation
parseRotation = Rotation <$> parseDirection
                         <*> parseNumber

parseInput' :: Parser Input
parseInput' = some (parseRotation <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Counter
compute = snd . L.foldl' step (50, 0)
  where
    step :: (Position, Counter) -> Rotation -> (Position, Counter)
    step (p, c) r = let (p', clicks) = next p r in (p', c + clicks)
    next :: Position -> Rotation -> (Position, Counter)
    next p (Rotation L n) = ((p - n) `mod` 100, (n + (if n > 0 && p /= 0 then 100 - p else p)) `div` 100)
    next p (Rotation R n) = ((p + n) `mod` 100, (n + p) `div` 100)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
