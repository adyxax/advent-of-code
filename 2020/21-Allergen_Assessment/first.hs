-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where
import Control.Monad (void, when)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 5

newtype Ingredient = Ingredient String deriving (Eq, Ord, Read, Show)
newtype Allergen = Allergen String deriving (Eq, Ord, Read, Show)
type Rule = ([Ingredient], [Allergen])
type Input = [Rule]

type Parser = Parsec Void String

parseRule :: Parser Rule
parseRule = do
  ingredients <- some ((Ingredient <$> some letterChar) <* char ' ')
  void $ string "(contains "
  allergens <- some ((Allergen <$> some letterChar) <* optional (string ", "))
  void $ string ")\n"
  return $ (ingredients, allergens)

parseInput' :: Parser Input
parseInput' = some parseRule <* eof

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

compute :: Input -> Int
compute rules = sum . map count $ filter isUnused ingredients
  where
    ingredients :: [Ingredient]
    ingredients = S.elems . S.fromList . concat $ map fst rules
    candidates :: [[Ingredient]]
    candidates = M.elems candidatesMap
    candidatesMap :: M.Map Allergen [Ingredient]
    candidatesMap = L.foldl' appendIngredientsForRule M.empty rules
    appendIngredientsForRule :: M.Map Allergen [Ingredient] -> Rule -> M.Map Allergen [Ingredient]
    appendIngredientsForRule m (is, as) = L.foldl' (appendIngredientsForAllergen is) m as
    appendIngredientsForAllergen :: [Ingredient] -> M.Map Allergen [Ingredient] -> Allergen -> M.Map Allergen [Ingredient]
    appendIngredientsForAllergen is m a = case M.lookup a m of
      Nothing -> M.insert a is m
      Just is' -> M.insert a (L.intersect is is') m
    isUnused :: Ingredient -> Bool
    isUnused i = (catMaybes $ map (L.elemIndex i) candidates) == []
    count :: Ingredient -> Int
    count i = sum $ map (length . L.elemIndices i) (map fst rules)

main :: IO ()
main = do
  example <- parseInput "example"
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
