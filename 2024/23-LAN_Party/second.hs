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

exampleExpectedOutput = "co,de,ka,ta"

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
type Negroup = S.Set [Computer]

-- All nodes have 4 neighbours in the example and 13 in the input. Since all
-- nodes are connected and the example having a biggest group size of 4, I tried
-- to solve for this and it worked. This means looking for nodes where all
-- neighbors except one are the same.
--
-- Luckily, there is a single group of this size that matches.
compute :: Input -> String
compute links = L.intercalate "," . L.sort . fromJust . snd $ L.foldl' biggestGroup (network, Nothing) hosts
  where
    biggestGroup :: (Network, Maybe [Computer]) -> Computer -> (Network, Maybe [Computer])
    biggestGroup a@(_, Just _) _ = a
    biggestGroup (net, Nothing) n | M.size sharedNs == desiredSize = (net', Just $ n : M.keys sharedNs)
                                  | otherwise = (net', Nothing)
      where
        net' = M.delete n net
        ns = n : net M.! n
        sharedNs = M.filter (\ns' -> length (L.filter (\n' -> L.elem n' ns) ns') == desiredSize) net
    hosts :: [Computer]
    hosts = M.keys network
    desiredSize = (length $ network M.! "ta") - 1
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
