-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 23

type Input = [Int]

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = some (read <$> some digitChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Seq = (Int, Int, Int, Int)
type PriceMap = M.Map Seq Int

compute :: Input -> Int
compute input = trace (show bestSequence) maxBananas
  where
    (bestSequence, maxBananas) = S.foldl (\(s, n) s' -> let n' = fromJust $ M.lookup s' bananas
                                                        in if n >= n' then (s, n)
                                                                      else (s', n')
                                         ) ((0, 0, 0, 0), 0) allSeqs
    bananas :: PriceMap
    bananas = S.foldl (\acc s -> M.insert s (sum $ catMaybes $ map (M.lookup s) allPriceMaps) acc) M.empty allSeqs
    allSeqs :: S.Set Seq
    allSeqs = L.foldl' (\acc p -> S.union acc $ S.fromList $ M.keys p) S.empty allPriceMaps
    allPriceMaps :: [PriceMap]
    allPriceMaps = map (seqPrices M.empty) allPrices
    seqPrices :: PriceMap -> [Int] -> PriceMap
    seqPrices m s@(a:b:c:d:e:_) = seqPrices (let seq = (b-a, c-b, d-c, e-d) in case M.lookup seq m of
                                                Just _  -> m
                                                Nothing -> M.insert seq e m
                                            ) (tail s)
    seqPrices m _               = m
    allPrices = map prices input
    prices :: Int -> [Int]
    prices s = fst $ L.foldl' next ([s `mod` 10], s) [1..2000]
    next :: ([Int], Int) -> Int -> ([Int], Int)
    next (acc, s) _ = let s' = prune $ mix (shift s 6) s
                          s'' = prune $ mix (shift s' (-5)) s'
                          s''' = prune $ mix (shift s'' 11) s''
                      in (acc ++ [s''' `mod` 10], s''')
    mix :: Int -> Int -> Int
    mix = xor
    prune :: Int -> Int
    prune n = n `mod` 16777216

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
