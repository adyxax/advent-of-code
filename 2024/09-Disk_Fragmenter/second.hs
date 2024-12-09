-- requires cabal install --lib megaparsec parser-combinators heap vector
-- very slow with runghc, use ghc -O3 -o second second.hs and get the result in seconds
module Main (main) where

import           Control.Monad        (void, when)
import qualified Data.List            as L
import           Data.Maybe
import qualified Data.Vector          as V
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 2858

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

type Disk = V.Vector (Maybe Int) -- Maybe fileId
type File = (Int, Int, Int) -- fileId, fileIndex, fileSize
type Free = (Int, Int) -- index, size

compute :: Input -> Int
compute input = V.sum $ V.imap checksum defragmentedDisk
  where
    checksum _ Nothing  = 0
    checksum i (Just n) = i * n
    defragmentedDisk :: Disk
    (_, defragmentedDisk) = L.foldr defragment (frees, startingDisk) files
    startingDisk :: Disk
    startingDisk = V.replicate (sum input) Nothing
    (files, frees, _, _) = computeFileIndexAndSize input ([], [], 0, 0)
    computeFileIndexAndSize :: Input -> ([File], [Free], Int, Int) -> ([File], [Free], Int, Int)
    computeFileIndexAndSize [] acc = acc
    computeFileIndexAndSize (fileSize:[]) (files, frees, fileId, fileIndex) = (files ++ [(fileId, fileIndex, fileSize)], frees, 0, 0)
    computeFileIndexAndSize (fileSize:freeSize:fs) (files, frees, fileId, fileIndex) = computeFileIndexAndSize fs
                                                                                       ( files ++ [(fileId, fileIndex, fileSize)]
                                                                                       , frees ++ [(fileIndex + fileSize, freeSize)]
                                                                                       , fileId + 1
                                                                                       , fileIndex + fileSize + freeSize )
    defragment :: File -> ([Free], Disk) -> ([Free], Disk)
    defragment (fileId, fileIndex, fileSize) (frees, disk) = (frees', disk V.// [(i, Just fileId)|i<-[fileIndex'..fileIndex'+fileSize-1]])
      where
        (fileIndex', frees') = findHole frees []
        findHole :: [Free] -> [Free] -> (Int, [Free])
        findHole [] _ = (fileIndex, frees)
        findHole (f@(freeIndex, freeSize):fs) acc | freeIndex > fileIndex = (fileIndex, frees)
                                                  | freeSize == fileSize = (freeIndex, acc ++ fs)
                                                  | freeSize > fileSize = (freeIndex, acc ++ ((freeIndex + fileSize, freeSize - fileSize):fs))
                                                  | freeSize < fileSize = findHole fs (acc ++ [f])

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
