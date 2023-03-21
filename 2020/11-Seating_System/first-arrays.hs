module Main (main) where
import Control.Monad (when)
import Data.Array qualified as A
import Data.List (foldl')
import Data.Maybe (catMaybes)
import System.Exit (die)

exampleExpectedOutput = 37

type Seating = A.Array (Int, Int) (Maybe Bool)

inputToArrayAssociations :: String -> [((Int, Int), Maybe Bool)]
inputToArrayAssociations = snd . foldl'  nextSeat  ((0, 0), [])
  where
    nextSeat :: ((Int, Int), [((Int, Int), Maybe Bool)]) -> Char -> ((Int, Int), [((Int, Int), Maybe Bool)])
    nextSeat ((x, y), acc) '\n' = ((0, y+1), acc)
    nextSeat ((x, y), acc) '.' = ((x+1, y), acc ++ [((x, y), Nothing)])
    nextSeat ((x, y), acc) 'L' = ((x+1, y), acc ++ [((x, y), Just False)])

parseInput :: String -> IO Seating
parseInput filename = do
  input <- readFile filename
  let ls = lines input
      height = (length ls) - 1
      width = (length (ls !! 0)) - 1
  return $ A.array  ((0,0), (width, height))  (inputToArrayAssociations input)

compute :: Seating -> Int
compute seating
  | seating == seating' = length . filter id . catMaybes $ A.elems seating
  | otherwise = compute seating'
  where
    (width, height) = snd $ A.bounds seating
    seating' :: Seating
    seating' = seating A.// (foldl' next [] (A.assocs seating))
    next :: [((Int, Int), Maybe Bool)] -> ((Int, Int), Maybe Bool) -> [((Int, Int), Maybe Bool)]
    next acc ((x, y), Just False) = ((x, y), Just $ around (x, y) == []) : acc
    next acc ((x, y), Just True) = ((x, y), Just $ length (around (x,y)) < 4) : acc
    next acc _ = acc
    around :: (Int, Int) -> [Bool]
    around (x, y) = filter id . catMaybes $ map lookup [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    lookup :: (Int, Int) -> Maybe Bool
    lookup (x, y)
      | x < 0 || y < 0 || x > width || y > height = Nothing
      | otherwise = seating A.! (x, y)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
