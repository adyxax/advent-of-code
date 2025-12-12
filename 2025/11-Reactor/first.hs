-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
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
exampleExpectedOutput = 5

-- Let's represent lights as binary numbers and buttons as XOR masks
type Label = String
type Device = (Label, [Label])
type Input = M.Map Label [Label]
type Parser = Parsec Void String

parseLabel :: Parser Label
parseLabel = some letterChar <* optional (char ' ')

parseDevice :: Parser Device
parseDevice = (,) <$> parseLabel <* string ": "
                  <*> some parseLabel <* eol

parseInput' :: Parser Input
parseInput' = M.fromList <$> some parseDevice <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = explore "you"
  where
    explore :: Label -> Int
    explore "out" = 1
    explore l = sum $ map explore $ input M.! l

main :: IO ()
main = do
  example <- parseInput "example"
  print example
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
