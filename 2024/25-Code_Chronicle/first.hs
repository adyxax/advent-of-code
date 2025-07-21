-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 3

data Schematic = Lock [Int] | Key [Int] deriving Show
type Input = [Schematic]

type Parser = Parsec Void String

parseOne :: Parser Int
parseOne = char '#' *> pure 1
       <|> char '.' *> pure 0

parseLine :: Parser [Int]
parseLine = some parseOne

encode :: [Int] -> [Int] -> [Int]
encode [] []            = []
encode (a:acc) (l:line) = a+l:encode acc line

parseSchematic :: Parser Schematic
parseSchematic = do
  lines <- some (parseLine <* eol)
  let code = L.foldl' encode [0, 0, 0, 0, 0] lines
  pure $ (if (head $ head lines) == 1 then Lock else Key) code

parseInput' :: Parser Input
parseInput' = some (parseSchematic <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = L.foldl' compute' 0 locks
  where
    keys  = [ k | k@(Key _)  <- input ]
    locks = [ l | l@(Lock _) <- input ]
    compute' :: Int -> Schematic -> Int
    compute' i l = L.foldl' (computeOne l) i keys
    computeOne :: Schematic -> Int -> Schematic -> Int
    computeOne (Lock ls) i (Key ks) = i + if all (< 8) (encode ls ks) then 1 else 0

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
