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

exampleExpectedOutput = Just 94

data Direction = N | S | E | W deriving (Eq, Show)
data Tile = Floor | Wall | Slope Direction deriving (Eq, Show)
type Line = V.Vector Tile
type Input = V.Vector Line

type Parser = Parsec Void String

parseDirection :: Parser Direction
parseDirection = char '^' $> N
             <|> char 'v' $> S
             <|> char '>' $> E
             <|> char '<' $> W

parseTile :: Parser Tile
parseTile = char '#' $> Wall
        <|> char '.' $> Floor
        <|> Slope <$> parseDirection

parseLine :: Parser Line
parseLine = do
  line <- some parseTile <* eol
  return $ V.generate (length line) (line !!)

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

newtype Cost = Cost Int deriving (Eq, Num, Ord, Show)
newtype NodeId = NodeId Int deriving (Eq, Num, Ord, Show)
newtype X = X Int deriving (Eq, Num, Ord, Show)
newtype Y = Y Int deriving (Eq, Num, Ord, Show)
type Adjacencies = M.Map NodeId [(NodeId, Cost)] -- keys are nodeIds and values are a list of (NodeId, cost)
type Nodes = M.Map (X, Y) NodeId -- keys are (x, y) and values are nodeIds
type Visited = M.Map (X, Y) ()

compute :: Input -> Maybe Cost
compute input = longuestPath adjacencies (let Just (a:[]) = M.lookup 0 adjacencies in a)
  where
    longuestPath :: Adjacencies -> (NodeId, Cost) -> Maybe Cost
    longuestPath adj (n, c) | n == 1 = Just $ c + 1
                            | l' == [] = Nothing
                            | otherwise = Just $ c + maximum l'
      where
        Just l = M.lookup n adj
        l' =  catMaybes $ L.map (longuestPath  adj') l
        adj' = M.delete n $ M.map (L.filter (\(i, _) -> n /= i)) adj
    (adjacencies, nodes, _) = explore 0 (M.fromList [(0, []), (1, [])]) (M.fromList [((startx, 0), 0), ((finishx, finishy), 1)]) (M.fromList [((startx, 0), ()), ((finishx, finishy), ())]) startx 1 S
    explore :: NodeId -> Adjacencies -> Nodes -> Visited -> X -> Y -> Direction -> (Adjacencies, Nodes, Visited)
    explore node adjacencies nodes visited x y d = L.foldl' explore' (adjacencies, nodes, visited) $ nextSteps x y d
      where
        explore' :: (Adjacencies, Nodes, Visited) -> (X, Y, Direction, Bool) -> (Adjacencies, Nodes, Visited)
        explore' acc@(adjacencies, nodes, visited) (x, y, d, u) | isNothing destination = acc
                                                                | otherwise = case M.lookup (x', y') nodes of
                                                                    Nothing -> explore node' adjacencies'' nodes' visited' x' y' d
                                                                    Just id -> (adjacencies'', nodes', visited')
          where
            destination = let s = goDownAPath visited False x y 1 d in s
            Just (visited', x', y', cost, u') = destination
            adjacencies'' = M.adjust (\l -> (node', cost):l) node $ M.adjust (\l -> if u || u' then l else (node, cost):l) node' adjacencies'
            nodes' = M.insert (x', y') node' nodes
            (node', adjacencies') = case M.lookup (x', y') nodes of
              Nothing    -> let s = NodeId (M.size nodes) in (s, M.insert s [] adjacencies)
              Just node' -> (node', adjacencies)
            goDownAPath :: Visited -> Bool -> X -> Y -> Cost -> Direction -> Maybe (Visited, X, Y, Cost, Bool) -- returns the next intersection's coordinates and cost, and if it is unidirectional
            goDownAPath visited u x y c d | M.member (x, y) nodes = Just (visited, x, y, c, u) -- we reached an already known intersection
                                          | M.member (x, y) visited = Nothing -- this tile has already been visited
                                          | isImpossibleSlope = Nothing
                                          | ns == [] = Nothing -- we hit a deadend
                                          | L.length ns > 1 = Just (visited', x, y, c, u'') -- we hit a crossroads
                                          | otherwise = goDownAPath visited' u'' x' y' (c+1) d'
              where
                (x', y', d', u') = head ns
                u'' = u || u'
                ns = nextSteps x y d
                visited' = M.insert (x, y) () visited
                isImpossibleSlope = case getTile (x, y) of
                  Slope s   -> s /= d
                  otherwise -> False
    getTile :: (X, Y) -> Tile
    getTile (X x, Y y) = input V.! y V.! x
    nextSteps :: X -> Y -> Direction -> [(X, Y, Direction, Bool)] -- get the list of possible next steps at a point, given where we came from
    nextSteps x y d = L.map augmentWithUnidirectionality $ L.filter possible [(x-1, y, W), (x+1, y, E), (x, y-1, N), (x, y+1, S)]
      where
        augmentWithUnidirectionality :: (X, Y, Direction) -> (X, Y, Direction, Bool)
        augmentWithUnidirectionality (x, y, d) = (x, y, d, isSlope $ getTile (x, y))
        isSlope :: Tile -> Bool
        isSlope (Slope _) = True
        isSlope _         = False
        possible :: (X, Y, Direction) -> Bool
        possible (x', y', d') | t == Wall = False
                              | d == opposite d' = False -- no going back
                              -- | t == Floor = True
                              | otherwise = True -- o == d' -- our direction must match the slope <- NO, this prevents us from properly finding intersections
          where
            t = getTile (x', y')
            Slope o = t
    Just start = V.findIndex (== Floor) $ input V.! 0
    startx = X start
    Just finish = V.findIndex (== Floor) $ input V.! finishyy
    finishx = X finish
    finishyy = V.length input - 1
    finishy = Y finishyy
    xydToxy :: (a, b, c) -> (a, b)
    xydToxy (x, y, _) = (x, y)
    opposite :: Direction -> Direction
    opposite N = S
    opposite S = N
    opposite E = W
    opposite W = E

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
