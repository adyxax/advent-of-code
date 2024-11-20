-- requires cabal install --lib megaparsec parser-combinators heap vector fgl
-- very slow with runghc, use ghc -O3 -o first first.hs
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Graph.Inductive
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set                         as S
import           Data.Tuple
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 54

type Input = M.Map String (S.Set String)

type Parser = Parsec Void String

parseLabel :: Parser String
parseLabel = some letterChar <* optional (char ':') <* optional hspace

parseLine :: Parser (String, S.Set String)
parseLine = (,) <$> parseLabel
                <*> (S.fromList <$> some parseLabel)

parseInput' :: Parser Input
parseInput' = M.fromList <$> (some (parseLine <* eol) <* eof)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = size1 * (order g - size1)
  where
    n = L.nub $ M.keys input ++ (S.elems . S.unions $ M.elems input)
    e = [(from, to) | (from, tos) <- M.assocs input, to <- S.elems tos]
    n' = zip n [1..]
    e' = map (\(from, to) -> (fromJust $ L.lookup from n', fromJust $ L.lookup to n', from ++ "-" ++ to)) e
    g :: Gr String String
    g = undir $ mkGraph (map swap n') e'
    size1 = length group1
    group1 = compute' (nodes g)
    -- Karger's algorithm to compute a minimum cut
    -- implementation from https://github.com/jrp2014/AoC2023/blob/main/Day25/Day25.hs#L60
    compute' :: [Node] -> [Node]
    compute' s | sum (map count s) == 3 = s
               | otherwise = compute' (L.delete (L.maximumBy (compare `on` count) s) s)
      where
        count :: Node -> Int
        count ns = length $ suc g ns L.\\ s

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
