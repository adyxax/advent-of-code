-- very slow with runghc, use ghc -O3 -o first first.hs
-- requires cabal install --lib megaparsec parser-combinators unordered-containers
module Main (main) where
import Control.Monad (void, when)
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

import Debug.Trace

exampleExpectedOutput = 18

data Direction = N | S | E | W deriving Show
data Blizzard = Blizzard Int Int Direction deriving Show
data Input = Input { blizzards :: [Blizzard]
                   , height :: Int
                   , width :: Int
                   , xend :: Int
                   , xstart :: Int
                   } deriving Show

type Parser = Parsec Void String

findBlizzards :: (Int, [Blizzard]) -> String -> (Int, [Blizzard])
findBlizzards (y, acc) line = (y+1, snd $ L.foldl' findBlizzards' (0, acc) line)
  where
    findBlizzards' :: (Int, [Blizzard]) -> Char -> (Int, [Blizzard])
    findBlizzards' (x, acc) '#' = (x+1, acc)
    findBlizzards' (x, acc) '.' = (x+1, acc)
    findBlizzards' (x, acc) '^' = (x+1, Blizzard x y N : acc)
    findBlizzards' (x, acc) 'v' = (x+1, Blizzard x y S : acc)
    findBlizzards' (x, acc) '>' = (x+1, Blizzard x y E : acc)
    findBlizzards' (x, acc) '<' = (x+1, Blizzard x y W : acc)

parseMapLine :: Parser String
parseMapLine = some (char '.' <|> char '#' <|> char '>' <|> char '<' <|> char '^' <|> char 'v') <* eol

parseInput' :: Parser Input
parseInput' = do
  lines <- some parseMapLine <* eof
  let height = length lines
      start = head lines
      width = length start
      Just xend = L.elemIndex '.' $ last lines
      Just xstart = L.elemIndex '.' start
      blizzards = snd $ L.foldl' findBlizzards (0, []) lines
  return $ Input blizzards height width xend xstart

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

type Position = (Int, Int)

compute :: Input -> Int
compute (Input blizzards height width xend xstart) = compute' 0 $ S.singleton (xstart, 0)
  where
    boundaries :: S.Set Position
    boundaries = S.fromList (([(x, y)|x<-[0..width-1], y<-[0, height-1]] L.\\ [(xstart, 0), (xend, height-1)]) -- north and south walls
              ++ [(x, y)|x<-[0, width-1], y<-[1..height-2]] -- east and west walls
              ++ [(xstart, -1), (xend, height)]) -- to prevent escaping to the north and south
    compute' :: Int -> S.Set Position -> Int
    compute' i pos | (xend, height-1) `L.elem` pos = i
                   | otherwise = compute' (i+1) (step (i+1) pos)
    step :: Int -> S.Set Position -> S.Set Position
    step i = L.foldl' eval S.empty
      where
        eval :: S.Set Position -> Position -> S.Set Position
        eval acc pos = S.union acc ((candidates pos S.\\ boundaries) S.\\ blizzards' i)
          where
            candidates :: Position -> S.Set Position
            candidates (x, y) = S.fromList [(x-1, y), (x, y), (x+1, y), (x, y-1), (x, y+1)]
    blizzards' i = S.fromList $ map (evalBlizzards i) blizzards
    evalBlizzards :: Int -> Blizzard -> Position
    evalBlizzards i (Blizzard x y N) = (x, ((y - i - 1) `mod` (height-2)) + 1)
    evalBlizzards i (Blizzard x y S) = (x, ((y + i - 1) `mod` (height-2)) + 1)
    evalBlizzards i (Blizzard x y E) = (((x + i - 1) `mod` (width-2)) + 1, y)
    evalBlizzards i (Blizzard x y W) = (((x - i - 1) `mod` (width-2)) + 1, y)

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
