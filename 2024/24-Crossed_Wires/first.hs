-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

exampleExpectedOutput = 4
example2ExpectedOutput = 2024

type In = (String, Bool)
data Op = And | Or | Xor deriving (Show, Eq)
type Gate = (String, Op, String, String)
data Input = Input [In] [Gate] deriving Show

type Parser = Parsec Void String

parseIn :: Parser In
parseIn = (,) <$> some alphaNumChar <* string ": "
              <*> (char '1' $> True <|> char '0' $> False)

parseOp :: Parser Op
parseOp = string "AND" $> And
      <|> string "OR" $> Or
      <|> string "XOR" $> Xor

parseGate :: Parser Gate
parseGate = (,,,) <$> some alphaNumChar <* space
                  <*> parseOp <* space
                  <*> some alphaNumChar <* string " -> "
                  <*> some alphaNumChar

parseInput' :: Parser Input
parseInput' = Input <$> some (parseIn <* eol) <* eol
                    <*> some (parseGate <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

exec And = ( && )
exec Or  = ( || )
exec Xor = ( /= )

type Gates = M.Map String Bool

compute :: Input -> Int
compute (Input ins gates) = sum [if b then 2^i else 0 | (b, i) <- zip outs [0..]]
  where
    outs = map (fullMap M.!) $ M.keys $ M.filterWithKey (\(z:_) _ -> z == 'z') gatesMap
    startMap = M.fromList ins
    gatesMap = M.fromList $ map (\(a, b, c, d) -> (d, (a, b, c))) gates
    fullMap = M.foldlWithKey compute' startMap $ gatesMap
    compute' :: Gates -> String -> (String, Op, String) -> Gates
    compute' gates c (a, op, b) = case M.lookup c gates of
      Just _ -> gates
      Nothing -> let (va, gates') = case M.lookup a gates of
                       Just g -> (g, gates)
                       Nothing -> let gates' = compute' gates a $ gatesMap M.! a
                                  in (gates' M.! a, gates')
                     (vb, gates'') = case M.lookup b gates' of
                       Just g -> (g, gates')
                       Nothing -> let gates'' = compute' gates b $ gatesMap M.! b
                                  in (gates'' M.! b, gates'')
                 in M.insert c (exec op va vb) gates''

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
