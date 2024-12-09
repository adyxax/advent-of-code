-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 1928

type Input = [Int]

type Parser = Parsec Void String

parseBlockSize :: Parser Int
parseBlockSize = read . pure <$> digitChar

parseInput' :: Parser Input
parseInput' = some parseBlockSize <* eol <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute input = sum $ map (\(x, y) -> x * y) $ zip [0..] $ computeFile files' free []
  where
    files = [s | (i,s)<-zip [0..] input, even i]
    files' = zip [0..] files
    free = [s | (i,s)<-zip [0..] input, odd i]
    computeFile :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
    computeFile ((fId, fSize):fs) free acc = computeFillFree fs free (acc ++ L.replicate fSize fId)
    computeFillFree :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
    computeFillFree [] _ acc            = acc
    computeFillFree files (free:frees) acc | free == fSize = computeFile fs frees (acc ++ L.replicate fSize fId)
                                           | free > fSize  = computeFillFree fs (free-fSize:frees) (acc ++ L.replicate fSize fId)
                                           | free < fSize  = computeFile (fs++[(fId, fSize-free)]) frees (acc ++ L.replicate free fId)
      where
        (fId, fSize) = last files
        fs = init files

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
