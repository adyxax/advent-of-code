module Main (main) where
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import System.Exit (die)

exampleExpectedOutput = 37

type Seating = M.Map (Int, Int) Bool

inputToList :: String -> [((Int, Int), Bool)]
inputToList = snd . foldl'  nextSeat  ((0, 0), [])
  where
    nextSeat :: ((Int, Int), [((Int, Int), Bool)]) -> Char -> ((Int, Int), [((Int, Int), Bool)])
    nextSeat ((x, y), acc) '\n' = ((0, y+1), acc)
    nextSeat ((x, y), acc) '.' = ((x+1, y), acc)
    nextSeat ((x, y), acc) 'L' = ((x+1, y), acc ++ [((x, y), False)])

parseInput :: String -> IO Seating
parseInput filename = do
  input <- readFile filename
  return $ M.fromList (inputToList input)

compute :: Seating -> Int
compute seating
  | seating == seating' = length $ M.filter id seating
  | otherwise = compute seating'
  where
    seating' = M.mapWithKey next seating
    next :: (Int, Int) -> Bool -> Bool
    next (x, y) False = around (x, y) == []
    next (x, y) True = length (around (x,y)) < 4
    around :: (Int, Int) -> [Bool]
    around (x, y) = filter id $ mapMaybe (\v -> M.lookup v seating) [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
