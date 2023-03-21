module Main (main) where
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
import System.Exit (die)

exampleExpectedOutput = 26

type Seating = M.Map (Int, Int) (Maybe Bool)

inputToList :: String -> [((Int, Int), Maybe Bool)]
inputToList = snd . foldl'  nextSeat  ((0, 0), [])
  where
    nextSeat :: ((Int, Int), [((Int, Int), Maybe Bool)]) -> Char -> ((Int, Int), [((Int, Int), Maybe Bool)])
    nextSeat ((x, y), acc) '\n' = ((0, y+1), acc)
    nextSeat ((x, y), acc) '.' = ((x+1, y), ((x, y), Nothing) : acc)
    nextSeat ((x, y), acc) 'L' = ((x+1, y), ((x, y), Just False) : acc)

parseInput :: String -> IO Seating
parseInput filename = do
  input <- readFile filename
  return $ M.fromList (inputToList input)

compute :: Seating -> Int
compute seating
  | seating == seating' = length . filter id . catMaybes $ map snd (M.toList seating)
  | otherwise = compute seating'
  where
    seating' :: Seating
    seating' = M.mapWithKey next seating
    next :: (Int, Int) -> Maybe Bool -> Maybe Bool
    next _ Nothing = Nothing
    next (x, y) (Just False) = Just $ around (x, y) == []
    next (x, y) (Just True) = Just $ length (around (x,y)) < 5
    around :: (Int, Int) -> [Bool]
    around (x, y) = filter id . catMaybes $ mapMaybe lookup [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
      where
        lookup :: (Int, Int) -> Maybe (Maybe Bool)
        lookup (dx, dy) = lookup' (1, dx, dy)
        lookup' :: (Int, Int, Int) -> Maybe (Maybe Bool)
        lookup' (i, dx, dy) = case M.lookup (x + i * dx, y + i * dy) seating of
          Just (Just a) -> Just $ Just a
          Just Nothing -> lookup' (i+1, dx, dy)
          Nothing -> Nothing

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
