module Main (main) where
import Control.Monad (when)
import Data.List (foldl')
import Data.Map qualified as M
import System.Exit (die)

exampleExpectedOutput = 112

type Coord = (Int, Int, Int)
type Input = M.Map Coord Bool

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  return . M.fromList . fst $ foldl' process ([], (0, 0, 0)) input
  where
    process :: ([(Coord, Bool)], Coord) -> Char -> ([(Coord, Bool)], Coord)
    process (acc, (x, y, z)) c = case c of
      '.' -> (acc, (x+1, y, z))
      '#' -> (((x, y, z), True):acc, (x+1, y, z))
      '\n' -> (acc, (0, y+1, z))

complete :: Input -> Input
complete input = M.foldrWithKey process M.empty input
  where
    process :: Coord -> Bool -> Input -> Input
    process (x, y, z) False acc = acc
    process (x, y, z) True acc = M.unions [M.singleton (x, y, z) True, acc, M.fromList [((x+a, y+b, z+c), False) | a<-[-1..1], b<-[-1..1], c<-[-1..1]]]

step :: Int -> Input -> Input
step 0 input = input
step n input = step (n-1) (M.foldrWithKey evolve M.empty input')
  where
    input' = complete input
    evolve :: Coord -> Bool -> Input -> Input
    evolve (x, y, z) s acc = case s of
      False -> M.insert (x, y, z) (actives == 3) acc
      True -> M.insert (x, y, z) (actives == 3 || actives == 4) acc
      where
        actives = length . filter id $ [lookup (x+a, y+b, z+c) | a<-[-1..1], b<-[-1..1], c<-[-1..1]]
    lookup :: Coord -> Bool
    lookup c = case M.lookup c input' of
      Just True -> True
      otherwise -> False

compute :: Input -> Int
compute input = M.size . M.filter id $ step 6 input

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
