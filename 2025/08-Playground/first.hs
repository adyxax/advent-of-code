-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
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
exampleExpectedOutput = 40

data Point = Point Int Int Int deriving (Eq, Ord, Show)
type Input = [Point]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parsePoint :: Parser Point
parsePoint = Point <$> parseNumber <*> parseNumber <*> parseNumber

parseInput' :: Parser Input
parseInput' = some (parsePoint <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

distance2 :: Point -> Point -> Int
distance2 (Point x1 y1 z1) (Point x2 y2 z2) = let dx = x2 - x1
                                                  dy = y2 - y1
                                                  dz = z2 - z1
                                              in dx * dx + dy * dy + dz * dz

type Circuit = [Point]
type Pairing = (Point, Point)

compute :: Int -> Input -> Int
compute iterations input = product $ take 3 $ L.sortBy (flip compare) $ map length $ L.foldl' connect (map (\p -> [p]) input) pairings
  where
    connect :: [Circuit] -> Pairing -> [Circuit]
    connect acc (p1, p2) | connected = acc
                         | otherwise = connect
      where
        [c1] = filter (elem p1) acc
        [c2] = filter (elem p2) acc
        connected = c1 == c2
        connect = (c1 ++ c2) : (acc L.\\ [c1, c2])
    pairings :: [Pairing]
    pairings = take iterations $ M.elems distancesMap
    -- we are calculating the distances twice, but though wasteful this is fast enough
    distancesMap :: M.Map Int Pairing
    distancesMap = let distances' acc p = M.union acc $ M.fromList [(distance2 p p', (p, p'))|p'<-input, p /= p']
                   in L.foldl' distances' M.empty input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute 10 example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute 1000 input
