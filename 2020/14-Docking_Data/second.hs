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

exampleExpectedOutput = 208

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

computeOp :: Int -> Bits -> [Int]
computeOp index bits = fst $ foldl' evalBit ([index], 35) bits
  where
    evalBit :: ([Int], Int) -> Maybe Bool -> ([Int], Int)
    evalBit (acc, bit) (Just False) = (acc, bit-1)
    evalBit (acc, bit) (Just True) = (map (\x -> setBit x bit) acc, bit-1)
    evalBit (acc, bit) Nothing = ((map (\x -> setBit x bit) acc) ++ (map (\x -> clearBit x bit) acc), bit-1)

compute :: Input -> Int
compute = extract . foldl' compute init
  where
    extract :: (M.Map Int Int, Bits) -> Int
    extract = sum . M.elems . fst
    compute :: (M.Map Int Int, Bits) -> Op -> (M.Map Int Int, Bits)
    compute (acc, _) (Mask bits) = (acc, bits)
    compute (acc, bits) (Mem index value) = (newAcc, bits)
      where
        newAcc :: M.Map Int Int
        newAcc = foldl' eval acc (computeOp index bits)
        eval :: M.Map Int Int -> Int -> M.Map Int Int
        eval acc' index' = M.insert index' value acc'
    init :: (M.Map Int Int, Bits)
    init = (M.empty, [])

main :: IO ()
main = do
  example <- parseInput "example2"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
