-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 10

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

compute :: Input -> Int
compute input = M.size . M.filter id $ L.foldl' compute' M.empty input
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
