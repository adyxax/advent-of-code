-- requires cabal install --lib megaparsec parser-combinators MemoTrie
{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Char qualified as C
import Data.Either
import Data.Functor
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.MemoTrie
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput = 525152

data Tile = Broken | Operational | Unknown deriving (Eq, Generic, Ord)
instance HasTrie Tile where
  newtype (Tile :->: b) = TileTrie { unTileTrie :: Reg Tile :->: b }
  trie = trieGeneric TileTrie 
  untrie = untrieGeneric unTileTrie
  enumerate = enumerateGeneric unTileTrie
instance Show Tile where
  show Broken = "#"
  show Operational = "."
  show Unknown = "?"
data Row = Row [Tile] [Int] deriving Show
type Input = [Row]

type Parser = Parsec Void String

parseNumber :: Parser Int
parseNumber = read <$> some digitChar <* optional (char ',')

parseTile :: Parser Tile
parseTile = char '#' $> Broken
        <|> char '.' $> Operational
        <|> char '?' $> Unknown

parseRow :: Parser Row
parseRow = Row <$> some parseTile <* space
               <*> some parseNumber <* eol

parseInput' :: Parser Input
parseInput' = some parseRow <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> error $ errorBundlePretty bundle
    Right input' -> return input'

--type Memo = M.Map [Tile] Int
--
--permutations :: Memo -> [Int] -> [Tile] -> (Memo, Int)
--permutations m [] ts | Broken `L.notElem` ts = (m, 1)
--                     | otherwise = (m, 0)
--permutations m rr@(r:rs) ts | isJust cached = (m, fromJust cached)
--                            | (length rr - 1) + sum rr < length ts' = (m, 0)
--                            | Operational `L.elem` take r ts' = (m'', operational)
--                            | null ts'' = (m'', operational + 1)
--                            | head ts'' == Broken = (m'', operational)
--                            | otherwise = (m'''', operational + broken)
--  where
--    cached = M.lookup ts m
--    ts' = L.dropWhile (== Operational) ts
--    ts'' = drop r ts'
--    (m', operational) = permutations m rr ts'
--    m'' = if length ts == 5 then M.insert ts operational m' else m'
--    (m''', broken) = permutations m' rs $ tail ts''
--    m'''' = if length ts == 5 then M.insert ts broken m''' else m'''
--
-- permutations rs (Operational:xs) = map (Operational:) $ permutations rs xs
-- permutations [r] (Broken:xs) = map (\n -> Broken:Operational:n) $ permutations rs xs
-- permutations (r:rs) (Broken:xs) = map (replicate l' Broken ++) (if null xs || head xs == Operational then permutations rs xs' else [])
--   where
--     l = length (L.takeWhile (/= Operational) xs)
--     l' = min l (r-1)
--     xs' = drop l' xs
-- permutations rr@(r:rs) (Unknown:xs) = (if possible rs xs then map (Operational:) (permutations rr (Operational:xs)) else [])
--                                    ++ (if l' == l && possible rs xs' then map (replicate l' Broken ++) (permutations rs xs') else [])
--   where
--     l = length (L.takeWhile (/= Operational) xs)
--     l' = min l (r-1)
--     xs' = drop l' xs
-- permutations _ _ = []
permutations = memo2 permutations'
  where
    permutations' :: [Int] -> [Tile] -> Int
    permutations' [] ts | Broken `L.notElem` ts = 1
                        | otherwise = 0
    permutations' _ [] = 0
    permutations' rs (Operational:ts) = permutations rs ts
    permutations' (r:rs) (Broken:ts) = case splitAt (r-1) ts of
      (a, t:b) | length a == r-1, Operational `L.notElem` a, t /= Broken -> permutations rs b
      (a, [])  | length a == r-1, Operational `L.notElem` a -> permutations rs []
      _ -> 0
    permutations' rs (Unknown:ts) = permutations rs (Operational:ts) + permutations rs (Broken:ts)

compute :: Input -> Int
compute input = sum $ map compute' expand
  where
    compute' :: Row -> Int
    compute' (Row tiles record) = permutations record tiles
    expand = map expandOne input
    expandOne (Row tiles record) = Row (L.intercalate [Unknown] $ replicate 5 tiles) (concat $ replicate 5 record)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
