-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

exampleExpectedOutput = 142

type Input = [String]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (some alphaNumChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

process :: Int -> String -> Int
process acc str = acc + f * 10 + l
  where
    f = C.digitToInt . head $ dropWhile C.isLetter str
    l = C.digitToInt . head . dropWhile C.isLetter $ reverse str

compute :: Input -> Int
compute = L.foldl' process 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
