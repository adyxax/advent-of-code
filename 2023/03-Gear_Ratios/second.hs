-- very slow with runghc, use ghc -O3 -o second second.hs
-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

exampleExpectedOutput = 467835

data Number = Number Int Int Int Int deriving Show -- Value x1 x2 y1
data Symbol = Symbol Int Int deriving (Eq, Ord, Show) -- x y
data Input = Input [Number] [Symbol] deriving Show

type Parser = Parsec Void String

parseNumber :: Parser Number
parseNumber = do
  (SourcePos _ y' x1') <- getSourcePos
  value <- read <$> some digitChar
  (SourcePos _ _ x2') <- getSourcePos
  let (x1, x2, y) = (unPos x1', unPos x2' - 1, unPos y')
  return $ Number value x1 x2 y

parseSymbol :: Parser Symbol
parseSymbol = do
  (SourcePos _ y' x') <- getSourcePos
  void $ char '*'
  let (x, y) = (unPos x', unPos y')
  return $ Symbol x y

parseNumbersAndSymbols :: Parser (Either Number Symbol)
parseNumbersAndSymbols = do
  many $ void (char '.' <|> symbolChar <|> char '#' <|> char '%' <|> char '@' <|> char '-' <|> char '/' <|> char '&') <|> void eol
  something <- (Left <$> parseNumber) <|> (Right <$> parseSymbol)
  many $ void (char '.') <|> void eol
  return something

parseInput' :: Parser Input
parseInput' = do
  things <- some parseNumbersAndSymbols <* eof
  return $ Input (lefts things) (rights things)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute (Input nums syms) = sum $ map process syms
  where
    process :: Symbol -> Int
    process sym = case L.foldl' process' [] nums of
      [a, b] -> a * b
      _ -> 0
      where
        process' :: [Int] -> Number -> [Int]
        process' acc (Number v x1 x2 y') | not . S.null $ S.fromList [Symbol x y|x<-[x1-1..x2+1],y<-[y'-1..y'+1]] `S.intersection` S.singleton sym = v:acc
                                         | otherwise = acc

main :: IO ()
main = do
  example <- parseInput "example"
  print example
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input

