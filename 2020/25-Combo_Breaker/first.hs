-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Math.NumberTheory.Powers.Modular
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 14897079

type Input = (Integer, Integer)

type Parser = Parsec Void String

parseInteger :: Parser Integer
parseInteger = do
  n <- some digitChar
  void $ optional (char '\n')
  return $ read n

parseInput' :: Parser Input
parseInput' = do
  a <- parseInteger
  b <- parseInteger
  void eof
  return (a, b)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

transform :: Integer -> Int -> Integer
transform subjectNum loopSize = powMod subjectNum loopSize 20201227

compute :: Input -> Integer
compute (cardPubKey, doorPubKey) = encryptionKey
  where
    cardLoopSize = fromJust . L.elemIndex cardPubKey $ map (transform 7) [0..]
    doorLoopSize = fromJust . L.elemIndex doorPubKey $ map (transform 7) [0..]
    encryptionKey = transform doorPubKey cardLoopSize

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  print "OK"
  input <- parseInput "input"
  print $ compute input
