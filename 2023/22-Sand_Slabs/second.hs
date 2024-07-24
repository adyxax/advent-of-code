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

exampleExpectedOutput = 7

type Coord = (Int, Int, Int)
data Brick = Brick Coord Coord deriving (Eq, Show)
instance Ord Brick where
  (Brick (_, _, z1) (_, _, z2)) `compare` (Brick (_, _, c1) (_, _, c2)) = min z1 z2 `compare` min c1 c2
type Input = [Brick]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseCoord :: Parser Coord
parseCoord = (,,) <$> parseNumber
                  <*> parseNumber
                  <*> parseNumber

parseBrick :: Parser Brick
parseBrick = Brick <$> parseCoord <* char '~'
                   <*> parseCoord <* eol

parseInput' :: Parser Input
parseInput' = some parseBrick <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Height = (Int, Maybe Int) -- (Height, BrickId that achieved that height - 1)
type HeightMap = V.Vector (V.Vector Height)
type SupportMap = M.Map Int [Int] -- BrickId -> [supports brickIds]

compute :: Input -> Int
compute input = sum $ map howManyWouldFallIf $ L.filter (isSupportBrick supportMap) $ [0..(length input - 1)]
  where
    howManyWouldFallIf :: Int -> Int
    howManyWouldFallIf brickId = length . snd $ removeFalling (supportMap, []) [brickId]
      where
        removeFalling :: (SupportMap, [Int]) -> [Int] -> (SupportMap, [Int])
        removeFalling acc@(sm, l) f | supportBricks == [] = acc
                                    | otherwise = removeFalling (sm', l ++ supportBricks) supportBricks
          where
            removeOne :: SupportMap -> Int -> SupportMap
            removeOne sm i = M.map (L.delete i) $ M.delete i sm
            sm' :: SupportMap
            sm' = L.foldl' removeOne sm f
            supportBricks :: [Int]
            supportBricks = L.filter unsupported $ M.keys sm'
              where
                unsupported :: Int -> Bool
                unsupported i = sm' M.! i == [] && let (Brick (_, _, z) _) = settledInput L.!! i in z > 1
    isSupportBrick :: SupportMap -> Int -> Bool
    isSupportBrick sm brickId = L.any isSoleSupport $ M.elems sm
      where
        isSoleSupport :: [Int] -> Bool
        isSoleSupport [l] = brickId == l
        isSoleSupport _   = False
    (_, settledInput, heightMap, supportMap) = L.foldl' settle (0, [], startingHeightMap, M.empty) startingInput
      where
        settle :: (Int, Input, HeightMap, SupportMap) -> Brick -> (Int, Input, HeightMap, SupportMap)
        settle (brickId, acc, hm, sm) (Brick (x1, y1, z1) (x2, y2, z2)) = (brickId+1, acc ++ [(Brick (a1, b1, height) (a2, b2, height-c1+c2))], hm', sm')
          where
            (a1, a2, b1, b2, c1, c2) = (min x1  x2, max x1 x2, min y1 y2, max y1 y2, min z1 z2, max z1 z2)
            (height, supports) = V.foldl' heightLine (0, []) (V.ifilter (\i _ -> i>= b1 && i <= b2) hm)
              where
                heightLine :: (Int, [Int]) -> V.Vector Height -> (Int, [Int]) -- height, [support]
                heightLine acc line = V.foldl' heightElt acc (V.ifilter (\i _ -> i >= a1 && i <= a2) line)
                  where
                    heightElt :: (Int, [Int]) -> Height -> (Int, [Int])
                    heightElt a@(hacc, _) (1, Nothing) | hacc > 1 = a
                                                       | otherwise = (1, [])
                    heightElt a@(hacc, sacc) (h, Just p) | hacc == h = (hacc, if elem p sacc then sacc else p:sacc)
                                                         | hacc > h = a
                                                         | otherwise = (h, [p])
            hm' = V.imap updateLine hm
              where
                updateLine :: Int -> V.Vector Height -> V.Vector Height
                updateLine y line | y < b1 || y > b2 = line
                                  | otherwise = V.imap updateElt line
                  where
                    updateElt :: Int -> Height -> Height
                    updateElt x z | x < a1 || x > a2 = z
                                  | otherwise = (height+c2-c1+1, Just brickId)
            sm' = M.insert brickId supports sm
        startingHeightMap = V.replicate (ymax+1) (V.replicate (xmax+1) (1, Nothing))
          where
            (xmax, ymax) = L.foldl' findBounds (xs, ys) input -- xmin and ymin are 0 for both example and input
              where
                Brick (xs, ys, _) _ = head input
                findBounds :: (Int, Int) -> Brick -> (Int, Int)
                findBounds (a, b) (Brick (x1, y1, _) (x2, y2, _)) = (maximum [a, x1, x2], maximum [b, y1, y2])
        startingInput = L.sort input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
