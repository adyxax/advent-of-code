-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Bits
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Debug.Trace

exampleExpectedOutput :: Int
exampleExpectedOutput = 2

-- Let's represent lights as binary numbers and buttons as XOR masks
type Label = String
type Device = (Label, [Label])
type Input = M.Map Label [Label]
type Parser = Parsec Void String

parseLabel :: Parser Label
parseLabel = some letterChar <* optional (char ' ')

parseDevice :: Parser Device
parseDevice = (,) <$> parseLabel <* string ": "
                  <*> some parseLabel <* eol

parseInput' :: Parser Input
parseInput' = M.fromList <$> some parseDevice <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Memo = M.Map (Bool, Bool, Label) Int

compute :: Input -> Int
compute input = (\(a, b) -> trace (show a) b) $ L.foldl' step (M.empty, 0) [(False, False, "svr")]
  where
    step :: (Memo, Int) -> (Bool, Bool, Label) -> (Memo, Int)
    step (m, a) (True, True, "out") = (m, a+1)
    step acc (_, _, "out") = acc
    step acc@(m, a) e@(d, f, l) = case M.lookup e m of
      Just v -> (m, a+v)
      Nothing -> let (m', a') = L.foldl' step (m, 0) $ zip3 (repeat $ if l == "dac" then True else d) (repeat $ if l == "fft" then True else f) (input M.! l)
                 in (M.insert e a' m', a + a')

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
