-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 94

type Line = VU.Vector Int
type Input = V.Vector Line

type Parser = Parsec Void String

parseLine :: Parser Line
parseLine = do
  line <- some (C.digitToInt <$> digitChar) <* eol
  return $ VU.generate (length line) (line !!)

parseInput' :: Parser Input
parseInput' = do
  line <- some parseLine <* eof
  return $ V.generate (length line) (line !!)

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

data Heading = NS | EW deriving (Eq, Show)
data Position = Position Int Int Int Heading deriving (Show) -- cost x y heading
instance Ord Position where
  compare (Position c1 _ _ _) (Position c2 _ _ _) = c1 `compare` c2
instance Eq Position where
  (Position c1 _ _ _) == (Position c2 _ _ _) = c1 == c2
type Candidates = H.MinHeap Position
type CostsState = M.Map (Int,Int) (Int, Int) -- (x, y) (NSCost, EWCost)

step :: Input -> CostsState -> Position -> [Position]
step input costs (Position cost x y h) = L.concat [next 1 1 0, next (-1) (-1) 0]
  where
    next :: Int -> Int -> Int -> [Position] -- diff increment costchange
    next 11 _ _ = []
    next (-11) _ _ = []
    next d i costChange = case heatLoss of
      Just hl -> if improvement hl then (Position (cost + costChange + hl) x' y' h') : next (d+i) i (costChange + hl)
                                   else next (d+i) i (costChange + hl)
      Nothing -> []
      where
        h' | h == NS = EW
           | otherwise = NS
        x' | h' == NS = x
           | otherwise = x + d
        y' | h' == NS = y + d
           | otherwise = y
        improvement :: Int -> Bool
        improvement hl | d < 4 && d > -4 = False
                       | otherwise = case M.lookup (x', y') costs of
          Just (h1, h2) -> case h' of
            NS -> cost + hl < h1
            EW -> cost + hl < h2
          Nothing -> undefined -- should not happen, we catch out of bound when looking for heatLoss
        heatLoss :: Maybe Int
        heatLoss = case input V.!? y' of
          Just line -> line VU.!? x'
          Nothing   -> Nothing

compute :: Input -> Int
compute input = compute' startingCosts startingCandidates
  where
    compute' :: CostsState -> Candidates -> Int
    compute' costs candidates | x == size - 1 && y == size - 1 = cost
                              | improvement = compute' costs' (H.union candidates' (H.fromList $ step input costs position))
                              | otherwise = compute' costs candidates'
      where
        candidates' = H.drop 1 candidates
        costs' = M.insert (x, y) (if h == NS then (cost, snd state) else (fst state, cost)) costs
        improvement | h == NS = cost < fst state
                    | otherwise = cost < snd state
        position@(Position cost x y h) = head $ H.take 1 candidates
        state = costs M.! (x, y)
    infinity = maxBound :: Int
    startingCandidates = H.fromList [Position 0 0 0 h|h <- [NS, EW]] :: Candidates
    startingCosts = M.fromList [((x, y), (infinity, infinity))|x<-[0..size], y<-[0..size]]
    size = V.length input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
