-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.Bits (clearBit, setBit)
import Data.List (foldl')
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 165

type Bits = [Maybe Bool]
data Op = Mask Bits| Mem Int Int
type Input = [Op]

type Parser = Parsec Void String

parseMask :: Parser Op
parseMask = do
  void $ string "mask = "
  bits <- some $ (char '1' *> (return $ Just True)) <|> (char '0' *> (return $ Just False)) <|> (char 'X' *> (return Nothing))
  return $ Mask bits

parseMem :: Parser Op
parseMem = do
  void $ string "mem["
  index <- some digitChar
  void $ string "] = "
  value <- some digitChar
  return $ Mem (read index) (read value)

parseOps :: Parser Input
parseOps = do
  ops <- some $ (parseMask <|> parseMem) <* (optional $ char '\n')
  void $ eof
  return $ ops

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseOps filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right ops -> return ops

computeValue :: Int -> Bits -> Int
computeValue value bits = fst $ foldl' evalBit (value, 35) bits
  where
    evalBit :: (Int, Int) -> Maybe Bool -> (Int, Int)
    evalBit (value, index) (Just True) = (setBit value index, index-1)
    evalBit (value, index) (Just False) = (clearBit value index, index-1)
    evalBit (value, index) Nothing = (value, index-1)

compute :: Input -> Int
compute = extract . foldl' compute init
  where
    extract :: (M.Map Int Int, Bits) -> Int
    extract = sum . M.elems . fst
    compute :: (M.Map Int Int, Bits) -> Op -> (M.Map Int Int, Bits)
    compute (acc, _) (Mask bits) = (acc, bits)
    compute (acc, bits) (Mem index value) = (M.insert index (computeValue value bits) acc, bits)
    init :: (M.Map Int Int, Bits)
    init = (M.empty, [])

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
