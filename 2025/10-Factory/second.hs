-- requires cabal install --lib megaparsec parser-combinators heap vector matrix
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Matrix          as MTX
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 33

type Light = Int
type Button = [Int]
type Joltage = Int
data Machine = Machine [Button] [Joltage] deriving (Show)
type Input = [Machine]
type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseLight :: Parser Light
parseLight = do
  bits <- char '[' *> some (char '.' $> '0' <|> char '#' $> '1') <* string "] "
  pure . read $ "0b" ++ reverse bits

parseButton :: Parser Button
parseButton = char '(' *> some parseNumber <* string ") "

parseJoltages :: Parser [Joltage]
parseJoltages = char '{' *> some parseNumber <* string "}"

parseMachine :: Parser Machine
parseMachine = do
  void parseLight
  Machine <$> some parseButton <*> parseJoltages <* eol

parseInput' :: Parser Input
parseInput' = some parseMachine <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type IntM = MTX.Matrix Int
type IntV = V.Vector Int

deleteRow :: (IntM, IntV) -> Int -> (IntM, IntV)
deleteRow a@(m, v) i -- | nrows /= V.length v = error "size mismatch"
                     | nrows == 0 || ncols == 0 = a
                     | nrows == 1 = (MTX.zero 0 0, V.empty)
                     | i == 1     = (MTX.submatrix 2 nrows 1 ncols m, V.tail v)
                     | i == nrows = (MTX.submatrix 1 (nrows - 1) 1 ncols m, V.init v)
                     | otherwise  = let bottom = MTX.submatrix (i + 1) nrows 1 ncols m
                                        top    = MTX.submatrix 1 (i - 1) 1 ncols m
                                    in (top MTX.<-> bottom, (V.take (i-1) v) V.++ (V.drop i v))
  where
    (ncols, nrows) = (MTX.ncols m, MTX.nrows m)

deleteCol :: (IntM, IntV) -> Int -> (IntM, IntV)
deleteCol a@(m, v) i -- | nrows /= V.length v = error "size mismatch"
                     | ncols == 0 || nrows == 0 = a
                     | ncols == 1 && nrows > 1 = (MTX.fromList 1 1 [1], V.fromList [1_000_000])
                     | ncols == 1 = (MTX.zero 0 0, V.empty)
                     | i == 1 = (MTX.submatrix 1 nrows 2 ncols m, v)
                     | i == ncols = (MTX.submatrix 1 nrows 1 (ncols - 1) m, v)
                     | otherwise  = let left  = MTX.submatrix 1 nrows 1 (i - 1) m
                                        right = MTX.submatrix 1 nrows (i + 1) ncols m
                                    in (left MTX.<|> right, v)
  where
    (ncols, nrows) = (MTX.ncols m, MTX.nrows m)

removeZeroRow :: (IntM, IntV) -> Int -> (IntM, IntV)
removeZeroRow a@(m, v) i = deleteRow (L.foldl' deleteCol a idxs) i
  where
    r = MTX.getRow i m
    idxs = reverse [i | i <- [1..MTX.ncols m], (r V.! (i-1) /= 0)]

removeOneCoeffRow :: (IntM, IntV) -> Int -> (IntM, IntV)
removeOneCoeffRow a@(m, v) j = deleteRow (m', v') j
  where
    r = MTX.getRow j m
    i:_ = [i | i <- [1..MTX.ncols m], (r V.! (i-1) /= 0)]
    n = v V.! (j-1)
    (m', _) = deleteCol a i
    v' = V.zipWith (\a c -> a - (c * n)) v (MTX.getCol i m)

eliminate :: (IntM, IntV) -> (Int, Int, Int) -> (IntM, IntV)
eliminate (m, v) (i, j, n) -- | nrows /= V.length v = error "size mismatch"
                           | otherwise = a'
  where
    nrows = MTX.nrows m
    a'@(m', v') = (MTX.combineRows i (-1) j m, v V.// [(i-1, n)])

valid :: (IntM, IntV) -> Bool
valid (m, v) = and [not ((all (== 0) r) && (n /= 0)) && n >= 0 && any (\c -> c == 0 || c == 1) r | i <- [1..MTX.nrows m], let r = MTX.getRow i m, let n = v V.! (i-1)]

solve :: (IntM, IntV) -> Int
solve a@(m, v) -- | nrows /= V.length v = error "size mismatch"
               | nrows == 0 = 0
               | not (valid a) = 1_000_000
               | nrows == 1 = V.head v
               | length zeroRows > 0 = solve $ removeZeroRow a zeroRow
               | length eliminationss > 0 = solve $ eliminate a elimination
               | length oneCoeffRows > 0 = (v V.! (oneCoeffRow - 1)) + (solve $ removeOneCoeffRow a oneCoeffRow)
               | otherwise = bruteForce a
  where
    nrows = MTX.nrows m
    zeroRows = [i | i <- [1..MTX.nrows m], (v V.! (i-1)) == 0, all (== 0) (MTX.getRow i m)]
    zeroRow:_ = zeroRows
    eliminationss = [ ds | j <- [1..nrows]
                         , let ds = [( i, j, n) | i <- [1..nrows], i /= j
                                                , let r = V.zipWith (-) (MTX.getRow i m) (MTX.getRow j m)
                                                , all (>=0) r
                                                , let n = (v V.! (i-1)) - (v V.! (j-1))
                                                , n >= 0]
                         , length ds > 0 ]
    eliminations:_ = eliminationss
    elimination:_ = eliminations
    oneCoeffRows = [i | i <- [1..nrows], (sum (MTX.getRow i m)) == 1]
    oneCoeffRow:_ = oneCoeffRows

guess :: (Int, Int) -> (IntM, IntV) -> (IntM, IntV)
guess (i, n) a@(m, v) = (m', v')
  where
    (m', _) = deleteCol a i
    v' = V.zipWith (\a c -> a - (c * n)) v (MTX.getCol i m)

bruteForce :: (IntM, IntV) -> Int
bruteForce a@(m, v) = minimum [n + solve g | n <- [0..rn], let g = guess (cid, n) a, valid g]
  where
    -- get the id of the column with the most coeffs
    cid = fst $ L.foldl' (\a@(i, n) i' -> let n' = sum (MTX.getCol i' m) in if n' > n then (i', n') else a) (-1, -1) [1..(MTX.ncols m)]
    -- get the minimum of the target vector components for which coeffs of the chosen column are not null
    rn = minimum [v V.! (j-1) | j <- [1..(MTX.nrows m)], MTX.getElem j cid m == 1]

compute :: Input -> Int
compute = sum . map compute'
  where
    compute' :: Machine -> Int
    compute' (Machine bs js) = let n = solve (m, v) in trace ("-----" ++ show n ++ "\n" ++ show m ++ "\n" ++ show v) n
      where
        columns = length bs
        rows = length js
        m = MTX.fromList rows columns $ concat [[if L.elem y (bs L.!! x) then 1 else 0 | x<-[0..columns-1]] | y<-[0..rows-1]]
        v = V.fromList js

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
