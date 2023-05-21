{-# LANGUAGE DataKinds #-}
-- requires cabal install --lib megaparsec parser-combinators mod
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Mod
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 14897079

type I = Mod 20201227
type Input = (I, I)

type Parser = Parsec Void String

parseInt :: Parser (I)
parseInt = do
  n <- some digitChar
  void $ optional (char '\n')
  return $ read n

parseInput' :: Parser Input
parseInput' = do
  a <- parseInt
  b <- parseInt
  void eof
  return (a, b)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

transform :: I -> Int -> I
transform subjectNum loopSize = subjectNum ^% loopSize

compute :: Input -> I
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
  input <- parseInput "input"
  print $ compute input
