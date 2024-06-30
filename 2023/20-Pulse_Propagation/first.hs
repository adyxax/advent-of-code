-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Applicative.Permutations
import           Control.Monad                    (void, when)
import qualified Data.Char                        as C
import           Data.Either
import           Data.Functor
import qualified Data.Heap                        as H
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           Data.Void                        (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 32000000
example2ExpectedOutput = 11687500
data Pulse = Low | High deriving (Eq, Show)
data Module = Normal | FlipFlop Bool | Conjunction (M.Map String Pulse) | Broadcaster deriving (Eq, Show)
data Configuration = Configuration Module String [String] deriving (Eq, Show)
type Conf = (Module, [String])
type Input = M.Map String Conf

type Parser = Parsec Void String

parseModule :: Parser Module
parseModule = char '%' $> FlipFlop False
          <|> char '&' $> Conjunction M.empty
          <|> lookAhead (string "broadcaster") $> Broadcaster
          <|> lookAhead letterChar $> Normal

parseLabel :: Parser String
parseLabel = some letterChar

parseConfiguration :: Parser Configuration
parseConfiguration = Configuration <$> parseModule
                                   <*> parseLabel <* string " -> "
                                   <*> some (parseLabel <* optional (string ", "))

parseInput' :: Parser Input
parseInput' = M.fromList . map (\(Configuration m s l) -> (s, (m, l))) <$> some (parseConfiguration <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = let (x, y) = computeX 1000 (0, 0) $ initConjuctions input in x * y
  where
    computeX :: Int -> (Int, Int) -> Input -> (Int, Int)
    computeX 0 n _ = n
    computeX i n input = let (n', input') = compute' (1, 0) [("button", Low, "broadcaster")] input
                         in computeX (i-1) (scoreAdd n n') input'
    compute' :: (Int, Int) -> [(String, Pulse, String)] -> Input -> ((Int, Int), Input)
    compute' n [] input = (n, input)
    compute' n signals input | length stepAll == 0 = (n, input)
                             | otherwise = compute' (scoreAdd n $ score stepAll) stepAll alterAll
      where
        alterAll :: Input
        alterAll = L.foldl' alterOne input signals
        alterOne :: Input -> (String, Pulse, String) -> Input
        alterOne acc (prev, p, me) = alter p prev me acc (M.lookup me input)
        alter :: Pulse -> String -> String -> Input -> Maybe Conf -> Input
        alter _    _    _  input (Just (Normal, _))         = input
        alter High _    _  input (Just (FlipFlop _, _))     = input
        alter Low  _    me input (Just (FlipFlop False, l)) = M.insert me (FlipFlop True, l) input
        alter Low  _    me input (Just (FlipFlop True, l))  = M.insert me (FlipFlop False, l) input
        alter p    prev me input (Just (Conjunction m, l))  = M.insert me (Conjunction $ M.insert prev p m, l) input
        alter p    _    _  input (Just (Broadcaster, l))    = input
        alter _    _    _  input Nothing                    = input
        score :: [(String, Pulse, String)] -> (Int, Int)
        score = L.foldl' scoreOne (0, 0)
        scoreOne :: (Int, Int) -> (String, Pulse, String) -> (Int, Int)
        scoreOne (x, y) (_, Low, _)  = (x + 1, y)
        scoreOne (x, y) (_, High, _) = (x, y + 1)
        stepAll :: [(String, Pulse, String)]
        stepAll = L.foldl' stepOne [] signals
        stepOne :: [(String, Pulse, String)] -> (String, Pulse, String) -> [(String, Pulse, String)]
        stepOne acc (prev, p, s) = step p prev s acc (M.lookup s input)
        step :: Pulse -> String -> String -> [(String, Pulse, String)] -> Maybe Conf -> [(String, Pulse, String)]
        step _    _    _  acc (Just (Normal, _))         = acc
        step High _    _  acc (Just (FlipFlop _, _))     = acc
        step Low  _    me acc (Just (FlipFlop False, l)) = acc ++ map (set me High) l
        step Low  _    me acc (Just (FlipFlop True, l))  = acc ++ map (set me Low) l
        step p    prev me acc (Just (Conjunction m, l))  = let p2 = if length (M.filter (\x -> x == High) $ M.insert prev p m) == length m then Low else High
                                                           in acc ++ map (set me p2) l
        step p    _    me acc (Just (Broadcaster, l))    = acc ++ map (set me p) l
        step _    _    _  acc Nothing                    = acc
    initConjuctions :: Input -> Input
    initConjuctions input = let r = M.foldrWithKey initConf input input in r
    initConf :: String -> Conf -> Input -> Input
    initConf c (_, l) input = L.foldl' initOne input l
      where
        initOne :: Input -> String -> Input
        initOne input s = case M.lookup s input of
          Just (Conjunction m, l) -> M.insert s (Conjunction (M.insert c Low m), l) input
          _ -> input
    scoreAdd (x, y) (x', y') = (x + x', y + y')
    set :: String -> Pulse -> String -> (String, Pulse, String)
    set me p s = (me, p, s)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  example2 <- parseInput "example2"
  let example2Output = compute example2
  when  (example2Output /= example2ExpectedOutput)  (error $ "example2 failed: got " ++ show example2Output ++ " instead of " ++ show example2ExpectedOutput)
  input <- parseInput "input"
  print $ compute input
