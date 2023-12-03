-- requires cabal install --lib megaparsec parser-combinators vector
module Main (main) where
import Control.Monad (void, when)
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 20

type Input = M.Map (Int, Int) Bool -- True if there is an elf

type Parser = Parsec Void String

inputToList :: String -> [((Int, Int), Bool)]
inputToList = snd . L.foldl'  nextElf  ((0, 0), [])
  where
    nextElf :: ((Int, Int), [((Int, Int), Bool)]) -> Char -> ((Int, Int), [((Int, Int), Bool)])
    nextElf ((x, y), acc) '\n' = ((0, y+1), acc)
    nextElf ((x, y), acc) '.' = ((x+1, y), acc)
    nextElf ((x, y), acc) '#' = ((x+1, y), acc ++ [((x, y), True)])

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  return $ M.fromList (inputToList input)

type Proposals = M.Map (Int, Int) (Int, Int, Bool)

firstHalfStep :: Input -> Int -> Proposals
firstHalfStep input iter = M.filter (\(_, _, e) -> e) $ M.foldrWithKey eval M.empty input
  where
    eval :: (Int, Int) -> Bool -> Proposals -> Proposals
    eval (x, y) _ = eval' (x, y, 0)
    eval' :: (Int, Int, Int) -> Proposals -> Proposals
    eval' (x, y, 4) acc = acc
    eval' (x, y, r) acc | alone = acc
                        | otherwise = case (iter + r) `mod` 4 of
      0 -> if M.member (x-1, y-1) input || M.member (x, y-1) input || M.member (x+1, y-1) input
           then next
           else if M.member (x, y-1) acc
                then M.insert (x, y-1) (x, y, False) acc -- two or more elves attempted to move to the same spot
                else M.insert (x, y-1) (x, y, True) acc
      1 -> if M.member (x-1, y+1) input || M.member (x, y+1) input || M.member (x+1, y+1) input
           then next
           else if M.member (x, y+1) acc
                then M.insert (x, y+1) (x, y, False) acc -- two or more elves attempted to move to the same spot
                else M.insert (x, y+1) (x, y, True) acc
      2 -> if M.member (x-1, y-1) input || M.member (x-1, y) input || M.member (x-1, y+1) input
           then next
           else if M.member (x-1, y) acc
                then M.insert (x-1, y) (x, y, False) acc -- two or more elves attempted to move to the same spot
                else M.insert (x-1, y) (x, y, True) acc
      3 -> if M.member (x+1, y-1) input || M.member (x+1, y) input || M.member (x+1, y+1) input
           then next
           else if M.member (x+1, y) acc
                then M.insert (x+1, y) (x, y, False) acc -- two or more elves attempted to move to the same spot
                else M.insert (x+1, y) (x, y, True) acc
      where
        alone = M.notMember (x-1, y-1) input
             && M.notMember (x, y-1) input
             && M.notMember (x+1, y-1) input
             && M.notMember (x-1, y) input
             && M.notMember (x+1, y) input
             && M.notMember (x-1, y+1) input
             && M.notMember (x, y+1) input
             && M.notMember (x+1, y+1) input
        next = eval' (x, y, r+1) acc

secondHalfStep :: Input -> Proposals -> Input
secondHalfStep = M.foldrWithKey eval
  where
    eval :: (Int, Int) -> (Int, Int, Bool) -> Input -> Input
    eval (x', y') (x, y, _) acc = M.insert (x', y') True $ M.delete (x, y) acc

compute :: Input -> Int
compute = compute' 0
  where
    compute' :: Int -> Input -> Int
    compute' i input | next == input = i + 1
                     | otherwise = compute' (i+1) next
     where
       next = secondHalfStep input (firstHalfStep input i)
       keys = M.keys input
       xs = L.map fst keys
       ys = L.map snd keys
       xmax = maximum xs
       xmin = minimum xs
       ymax = maximum ys
       ymin = minimum ys

main :: IO ()
main = do
  example <- parseInput "example"
  putStr $ showMap example
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input

showMap :: Input -> String
showMap input = printMap' xmin ymin ""
  where
    printMap' :: Int -> Int -> String -> String
    printMap' x y acc | M.member (x, y) input = printMap' (x+1) y (acc ++ "#")
                      | y > ymax = acc
                      | x <= xmax = printMap' (x+1) y (acc ++ ".")
                      | otherwise = printMap' xmin (y+1) (acc ++ "\n")
    keys = M.keys input
    xs = L.map fst keys
    ys = L.map snd keys
    xmax = maximum xs
    xmin = minimum xs
    ymax = maximum ys
    ymin = minimum ys
