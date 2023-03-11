-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Applicative.Permutations
import Control.Monad (void, when)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 2

data Passport = Passport { byr :: String
                         , iyr :: String
                         , eyr :: String
                         , hgt :: String
                         , hcl :: String
                         , ecl :: String
                         , pid :: String
                         } deriving (Show)

type Parser = Parsec Void String

parseString :: String -> Parser String
parseString key = do
  void $ string key
  void $ char ':'
  void (optional $ char '#')
  some alphaNumChar

spaces = char ' ' <|> char '\n'

parsePassport :: Parser (Maybe Passport)
parsePassport = do
  (byr, iyr, eyr, hgt, hcl, ecl, pid, cid) <- runPermutation $
    (,,,,,,,) <$> toPermutationWithDefault Nothing (Just <$> parseString "byr" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "iyr" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "eyr" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "hgt" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "hcl" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "ecl" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "pid" <* spaces)
              <*> toPermutationWithDefault Nothing (Just <$> parseString "cid" <* spaces)
  void (char '\n')
  return $ makePassport byr iyr eyr hgt hcl ecl pid cid

makePassport :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe Passport
makePassport (Just byr) ( Just iyr) (Just eyr) (Just hgt) (Just hcl) (Just ecl) (Just pid) _ = Just $ Passport { byr=byr, iyr=iyr, eyr=eyr, hgt=hgt, hcl=hcl, ecl=ecl, pid=pid }
makePassport _ _ _ _ _ _ _ _ = Nothing

parsePassports :: Parser [Passport]
parsePassports = do
  passports <- some (parsePassport)
  eof
  return $ catMaybes passports

parseInput :: String -> IO [Passport]
parseInput filename = do
  input <- readFile filename
  case runParser parsePassports filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right passports -> return passports

compute :: [Passport]-> Int
compute passports = length passports

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
