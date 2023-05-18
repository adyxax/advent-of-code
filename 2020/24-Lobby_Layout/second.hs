-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 2208

data Direction = E | W | NE | NW | SE | SW
type Directions = [Direction]
type Coordinates = (Int, Int, Int)
type Floor = M.Map Coordinates Bool
type Input = [Directions]

type Parser = Parsec Void String

parseDirection :: Parser Direction
parseDirection = (string "se" *> return SE)
  <|> (string "sw" *> return SW)
  <|> (string "ne" *> return NE)
  <|> (string "nw" *> return NW)
  <|> (char 'e' *> return E)
  <|> (char 'w' *> return W)

parseInput' :: Parser Input
parseInput' = some (some parseDirection <* optional (char '\n')) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

surroundings :: Coordinates -> [Coordinates]
surroundings (x, y, z) = [(x+1, y-1, z), (x-1, y+1, z), (x+1, y, z-1), (x-1, y, z+1), (x, y+1, z-1), (x, y-1, z+1)]

clean :: Floor -> Floor
clean = M.filter id

expand :: Floor -> Floor
expand floor = M.union floor $ M.foldrWithKey expand' M.empty floor
  where
    expand' :: Coordinates -> Bool -> Floor -> Floor
    expand' c True f = M.union f . M.fromList $ zip (surroundings c) (repeat False)
    expand' _ False f = f

proceed :: Int -> Floor -> Floor
proceed 0 f = f
proceed i f = proceed (i-1) $ expand next
  where
    next :: Floor
    next = M.union (M.foldrWithKey compute' M.empty f) f
    compute' :: Coordinates -> Bool -> Floor -> Floor
    compute' c s acc
      | s && (neighbors == 0 || neighbors > 2) = M.insert c False acc
      | not s && neighbors == 2 = M.insert c True acc
      | otherwise = acc
      where
        neighbors = length . filter id . catMaybes . map (\a -> M.lookup a f) $ surroundings c

compute :: Input -> Int
compute input = M.size . clean. proceed 100 . expand . clean $ L.foldl' compute' M.empty input
  where
    compute' :: Floor -> Directions -> Floor
    compute' floor directions = case M.lookup destination floor of
      Just f -> M.insert destination (not f) floor
      Nothing -> M.insert destination True floor
      where
        destination :: Coordinates
        destination = L.foldl' run (0, 0, 0) directions
    run :: Coordinates -> Direction -> Coordinates
    run (x, y, z) E = (x+1,y-1,z)
    run (x, y, z) W = (x-1,y+1,z)
    run (x, y, z) NE = (x+1,y,z-1)
    run (x, y, z) SW = (x-1,y,z+1)
    run (x, y, z) NW = (x,y+1,z-1)
    run (x, y, z) SE = (x,y-1,z+1)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
