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

type Input = [String]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (some alphaNumChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

data Key = U | D | L | R deriving (Eq)
instance Ord Key where
  compare R _ = LT
  compare U _ = LT
  compare D _ = LT
  compare _ _ = LT
type Coord = (Int, Int)
type Memo = M.Map (Int, Char, Char) Int

keyPad :: Char -> Coord
keyPad '7' = (0, 0)
keyPad '8' = (1, 0)
keyPad '9' = (2, 0)
keyPad '4' = (0, 1)
keyPad '5' = (1, 1)
keyPad '6' = (2, 1)
keyPad '1' = (0, 2)
keyPad '2' = (1, 2)
keyPad '3' = (2, 2)
keyPad '0' = (1, 3)
keyPad 'A' = (2, 3)
keyPad '^' = (1, 0)
keyPad 'B' = (2, 0)
keyPad '<' = (0, 1)
keyPad 'v' = (1, 1)
keyPad '>' = (2, 1)

paths :: (Char, Char) -> [String]
paths (a, b) | a == '<' || x1 == 0 && y2 == 3 = [two]
             | b == '<' || y1 == 3 && x2 == 0 = [one]
             | otherwise = L.nub [one, two]
  where
    one = moves ++ "B"
    two = reverse moves ++ "B"
    moves = replicate (abs $ y2 - y1) (if y2 > y1 then 'v' else '^') ++ replicate (abs $ x2 - x1) (if x2 > x1 then '>' else '<')
    (x1, y1) = keyPad a
    (x2, y2) = keyPad b

compute :: Input -> Int
compute codes = sum $ map complexity $ zip (map (computeOne 25) codes) codes
  where
    complexity :: (Int, String) -> Int
    complexity (output, code) = output * (read $ init code)
    computeOne :: Int -> String -> Int
    computeOne n = fst . computeOne' M.empty (n + 1) 'A'
      where
        computeOne' :: Memo -> Int -> Char -> String -> (Int, Memo)
        computeOne' memo 0 _ s = (length s, memo)
        computeOne' memo n start s = L.foldl' aggregate (0, memo) $ zip (start : s) s
          where
            aggregate :: (Int, Memo) -> (Char, Char) -> (Int, Memo)
            aggregate (total, memo) (a, b) = case (n, a, b) `M.lookup` memo of
              Just l -> (total + l, memo)
              Nothing ->
                let (l, memo') = case paths (a, b) of
                      [one] -> computeOne' memo (n - 1) 'B' one
                      [one, two] ->
                        let (l1, memo'') = computeOne' memo (n - 1) 'B' one
                            (l2, memo''') = computeOne' memo'' (n - 1) 'B' two
                         in if l1 < l2 then (l1, memo''') else (l2, memo''')
                 in (total + l, M.insert (n, a, b) l memo')

main :: IO ()
main = do
  input <- parseInput "input"
  print $ compute input
