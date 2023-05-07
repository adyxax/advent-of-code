-- requires cabal install --lib megaparsec parser-combinators
module Main (main) where

import Control.Monad (mapM_, void, when)
import Data.List (elemIndex, foldl', intercalate, intersect, transpose)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Map qualified as M
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Exit (die)

exampleExpectedOutput = 273

type TileID = Int
type Edge = [Bool]
type Image = [Edge]
data Tile = Tile { tileID :: TileID
                 , image :: Image
                 , edgesPermutations :: [Edge]
                 }
type Input = [Tile]

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some digitChar
  return $ read n

parseLine :: Parser Edge
parseLine = do
  elts <- some (char '#' <|> char '.')
  void $ char '\n'
  return $ map (== '#') elts

parseTile :: Parser Tile
parseTile = do
  void $ string "Tile "
  n <- parseInt
  void $ string ":\n"
  image <- some parseLine
  let edges = [head image, last image, map head image, map last image]
      inverted = map reverse edges
  return $ Tile n image (edges ++ inverted)

parseInput' :: Parser Input
parseInput' = do
  tiles <- some $ parseTile <* (optional $ char '\n')
  void eof
  return tiles

parseInput :: String -> IO Input
parseInput filename = do
  input <- readFile filename
  case runParser parseInput' filename input of
    Left bundle -> die $ errorBundlePretty bundle
    Right input' -> return input'

oneCornerTopLeftOriented :: [Tile] -> Tile
oneCornerTopLeftOriented tiles = topLeftOrientation $ (map fst . filter ((== 4) . snd) $ map matchingEdges tiles) !! 0
  where
    matchingEdges :: Tile -> (Tile, Int)
    matchingEdges tile = (tile, sum $ map (matches tile) tiles)
    matches :: Tile -> Tile -> Int
    matches Tile{tileID=a, image=_, edgesPermutations=e} Tile{tileID=b, image=_, edgesPermutations=f}
      | a == b = 0
      | otherwise = length $ intersect e f
    topLeftOrientation :: Tile -> Tile
    topLeftOrientation Tile{tileID=tID, image=img, edgesPermutations=e} = case (leftMatches, topMatches) of
      (False, False) -> Tile tID img e
      (False, True)  -> Tile tID (rotateRight img) e
      (True, False)  -> Tile tID (rotateLeft img) e
      (True, True)   -> Tile tID (rotateRight $ rotateRight img) e
      where
        leftMatches = or $ map (edgeMatch tID (map head img)) tiles
        topMatches = or $ map (edgeMatch tID (head img)) tiles
        edgeMatch :: TileID -> Edge -> Tile -> Bool
        edgeMatch i e tile
          | i == tileID tile = False
          | otherwise = isJust $ elemIndex e (edgesPermutations tile)

orientToMatchLeft :: Edge -> Image -> Image
orientToMatchLeft edge img = rotateLeft . orientToMatchTop (reverse edge) $ rotateRight img -- it took me a long time to find out I needed to reverse the edge!

orientToMatchTop :: Edge -> Image -> Image
orientToMatchTop edge img
  | edge == head img = img -- top is top
  | redg == head img = map reverse img -- top is reverse top
  | edge == head timg = timg -- top is left
  | redg == head timg = map reverse timg -- top is reverse left
  | edge == head rimg = rimg -- top is bottom
  | redg == head rimg = map reverse rimg -- top is reverse bottom
  | edge == head rtimg = rtimg -- top is right
  | redg == head rtimg = map reverse rtimg -- top is reverse right
  where
    redg = reverse edge
    timg = transpose img
    rimg = reverse img
    rtimg = reverse timg

buildTileGrid :: [[Tile]] -> M.Map TileID Tile -> [[Tile]]
buildTileGrid tiles tilesMap
  | M.size tilesMap == 0 = tiles
  | isJust nextTileToTheRight = buildTileGrid ((init tiles) ++ [(last tiles) ++ [fromJust nextTileToTheRight]]) (M.delete (tileID $ fromJust nextTileToTheRight) tilesMap)
  | otherwise = buildTileGrid (tiles ++ [[nextTileBellow]]) (M.delete (tileID nextTileBellow) tilesMap)
  where
    lastPlacedTile :: Tile
    lastPlacedTile = last $ last tiles
    firstOnLastLine :: Tile
    firstOnLastLine = head $ last tiles
    nextTileToTheRight :: Maybe Tile
    nextTileToTheRight = case filter (edgeMatch rightEdge) (M.elems tilesMap) of
      [] -> Nothing
      [a] -> Just (orientedOnTheLeft a)
      where
        orientedOnTheLeft :: Tile -> Tile
        orientedOnTheLeft Tile{tileID=tid, image=img, edgesPermutations=e} = Tile{tileID=tid, image=orientToMatchLeft rightEdge img, edgesPermutations=e}
        rightEdge :: Edge
        rightEdge = map last $ image lastPlacedTile
    nextTileBellow :: Tile
    nextTileBellow = orientedOnTop . head $ filter (edgeMatch bottomEdge) (M.elems tilesMap)
      where
        orientedOnTop :: Tile -> Tile
        orientedOnTop Tile{tileID=tid, image=img, edgesPermutations=e} = Tile{tileID=tid, image=orientToMatchTop bottomEdge img, edgesPermutations=e}
        bottomEdge :: Edge
        bottomEdge = last . image $ firstOnLastLine
    edgeMatch :: Edge -> Tile -> Bool
    edgeMatch e tile = isJust $ elemIndex e (edgesPermutations tile)

cropImage :: Image -> Image
cropImage = tail . init . transpose . tail . init . transpose

assembleLines :: [Image] -> Image
assembleLines images
  | length (images !! 0) == 0 = []
  | otherwise = (concat $ map head images) : (assembleLines $ map tail images)

rotateLeft :: Image -> Image
rotateLeft = reverse . transpose

rotateRight :: Image -> Image
rotateRight = transpose . reverse

monster :: Image
monster = [[ False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, True,  False ]
          ,[ True,  False, False, False, False, True,  True,  False, False, False, False, True,  True,  False, False, False, False, True,  True,  True ]
          ,[ False, True,  False, False, True,  False, False, True,  False, False, True,  False, False, True,  False, False, True,  False, False, False ]]

countMonsters :: Bool -> Image -> Int
countMonsters goDown i = --(if length z1 < length monster
                         -- then 0
                         -- else if length (z2 !! 0) < length (monster !! 0)
                         --      then 0
                         --      else if match then 1 else 0
                         (if match then 1 else 0) + nexts
  where
    z1 :: [(Edge, Edge)]
    z1 = zip i monster
    z2 :: [[(Bool, Bool)]]
    z2 = map (\(e, e') -> zip e e') $ z1
    match :: Bool
    match = and $ map (and . map matchOne) z2
    matchOne :: (Bool, Bool) -> Bool
    matchOne (_, False) = True
    matchOne (i, True) = i
    nexts :: Int
    nexts = (if goDown && length i > 3 then (countMonsters True $ tail i) else 0) + (if length (i !! 0) > 20 then countMonsters False $ map tail i else 0)

compute :: Input -> Int
compute tiles = spots - (monsters * 15)
  where
    topLeftTile :: Tile
    topLeftTile = oneCornerTopLeftOriented tiles
    tilesMap :: M.Map TileID Tile
    tilesMap = M.delete (tileID topLeftTile) (M.fromList $ zip (map tileID tiles) tiles)
    imagesGrid :: [[Image]]
    imagesGrid = map (map image) $ buildTileGrid [[topLeftTile]] tilesMap
    croppedImages :: [[Image]]
    croppedImages = map (map cropImage) imagesGrid
    assembledImage :: Image
    assembledImage = concat $ map assembleLines croppedImages
    permutations :: [Image]
    permutations = let r1 = rotateRight assembledImage
                       r2 = rotateRight r1
                       r3 = rotateRight r2
                       rotations = [assembledImage, r1, r2, r3]
                       inverted = map reverse rotations
                   in rotations ++ inverted
    monsters :: Int
    monsters = sum $ map (countMonsters True) permutations
    spots :: Int
    spots = length . filter id $ concat assembledImage

main :: IO ()
main = do
  example <- parseInput "example"
  --mapM_ (printImage . image) $ corners example
  --let topLeftTile = oneCornerTopLeftOriented example
  --    tilesMap = M.delete (tileID topLeftTile) (M.fromList $ zip (map tileID example) example)
  --    imagesGrid = map (map image) $ buildTileGrid [[topLeftTile]] tilesMap
  --    croppedImages = map (map cropImage) imagesGrid
  --    assembledImage = concat $ map assembleLines croppedImages
  --    permutations = let r1 = rotateRight assembledImage
  --                       r2 = rotateRight r2
  --                       r3 = rotateRight r3
  --                       rotations = [assembledImage, r1, r2, r3]
  --                       inverted = map reverse rotations
  --                   in rotations ++ inverted
  --printImage assembledImage
  let exampleOutput = compute example
  when  (exampleOutput /= exampleExpectedOutput)  (die $ "example failed: got " ++ show exampleOutput ++ " instead of " ++ show exampleExpectedOutput)
  input <- parseInput "input"
  print $ compute input
 --where
 -- printImage img = mapM_ print $ map (map (\x -> if x then '#' else '.')) img
