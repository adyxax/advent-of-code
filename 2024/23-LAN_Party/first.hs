-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import qualified Data.Set             as S
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 7

type Computer = String
type Link = (Computer, Computer)
type Input = [Link]

type Parser = Parsec Void String

parseLink :: Parser Link
parseLink = (,) <$> some letterChar <* char '-'
                <*> some letterChar

parseInput' :: Parser Input
parseInput' = some (parseLink <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Network = M.Map Computer [Computer]
type NetGroup = S.Set [Computer]

compute :: Input -> Int
compute links = length tgroups
  where
    tgroups :: [[Computer]]
    tgroups = L.nub $ L.filter (\(a:b:c:[]) -> and [isConnected a b, isConnected a c, isConnected b c]) $ L.sort $ map L.sort $ concatMap tgroup thosts
    tgroup :: Computer -> [[Computer]]
    tgroup c = map (\ns -> (c:ns)) $ tgroup' (network M.! c)
    tgroup' :: [Computer] -> [[Computer]]
    tgroup' (n:ns) = map (\c -> [n,c]) ns ++ tgroup' ns
    tgroup' _      = []
    isConnected :: Computer -> Computer -> Bool
    isConnected c1 c2 = L.elem c1 $ network M.! c2
    thosts :: [Computer]
    thosts = M.keys $ M.filterWithKey (\k _ -> head k == 't') network
    network = L.foldl' connect M.empty links
    connect :: Network -> Link -> Network
    connect net (c1, c2) = connectOne c1 c2 $ connectOne c2 c1 net
    connectOne :: Computer -> Computer -> Network -> Network
    connectOne c1 c2 net = case M.lookup c1 net of
      Just l  -> M.insert c1 (c2:l) net
      Nothing -> M.insert c1 [c2] net

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
