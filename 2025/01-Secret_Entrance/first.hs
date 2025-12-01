-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Counter
exampleExpectedOutput = 3

newtype Counter = Counter Int deriving (Eq, Num, Show)
data Direction = L | R deriving Show
newtype Position = Position Int deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
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
                         <*> (Position <$> parseNumber)

parseInput' :: Parser Input
parseInput' = some (parseRotation <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Counter
compute = snd . L.foldl' step (Position 50, Counter 0)
  where
    step :: (Position, Counter) -> Rotation -> (Position, Counter)
    step (p, c) r = let p' = next p r in (p', if p' == 0 then c + 1 else c)
    next :: Position -> Rotation -> Position
    next p (Rotation L n) = (p - n) `mod` 100
    next p (Rotation R n) = (p + n) `mod` 100

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
