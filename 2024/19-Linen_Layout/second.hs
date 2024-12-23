-- requires cabal install --lib megaparsec parser-combinators heap vector
module Main (main) where

import           Control.Monad        (void, when)
import           Data.Functor
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

exampleExpectedOutput = 16

type Input = ([String], [String]) -- patterns, designs

type Parser = Parsec Void String

parseInput' :: Parser Input
parseInput' = (,) <$> some (some letterChar <* optional (string ", ")) <* eol <* eol
                  <*> some (some letterChar <* eol) <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle  -> error $ errorBundlePretty bundle
    Right input' -> return input'

type Memo = M.Map String Int

compute :: Input -> Int
compute (stripes, designs) = snd $ L.foldl' compute' (M.empty, 0) designs
  where
    compute' :: (Memo, Int) -> String -> (Memo, Int)
    compute' (memo, c) [] = (memo, c + 1)
    compute' (memo, c) design = case M.lookup design memo of
      Just c' -> (memo, c + c')
      Nothing -> let (memo', c') = L.foldl' compute' (memo, 0) $ catMaybes $ map (\s -> L.stripPrefix s design) stripes
                 in (M.insert design c' memo', c + c')

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (error $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
