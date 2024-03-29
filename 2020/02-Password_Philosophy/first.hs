-- requires cabal install --lib megaparsec
module Main (main) where

import Control.Monad (void, when)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 2

data Rule = Rule { lower  :: Int
                 , higher :: Int
                 , elt    :: Char
                 , pass   :: String
                 } deriving (Show)

type Parser = Parsec Void String

parseRule :: Parser Rule
parseRule = do
  l <- try (some digitChar)
  void (char '-')
  h <- try (some digitChar)
  void (char ' ')
  e <- anySingle
  void (string ": ")
  pass <- (some letterChar)
  void (char '\n')
  return Rule { lower = read l, higher = read h, elt = e, pass = pass }

parseRules :: Parser [Rule]
parseRules = some parseRule <* eof

parseInput :: String -> IO [Rule]
parseInput filename = do
  input <- readFile filename
  case runParser parseRules filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right rules -> return rules

validateRule :: Rule -> Int
validateRule Rule{lower=l, higher=h, elt=e, pass=pass} = if (n >= l && n <= h) then 1 else 0
  where
    n = length $ filter (== e) pass

compute :: [Rule] -> Int
compute = sum . map validateRule

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
