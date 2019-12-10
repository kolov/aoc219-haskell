module Day8 where

import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Traversable as S
import           Debug.Trace

type Layer a = [[a]]

type Image a = [Layer a]

data Color
  = Black
  | White
  | Transparent

mkColor :: Int -> Color
mkColor 2 = Transparent
mkColor 1 = Black
mkColor 0 = White

instance Semigroup Color where
  (<>) Transparent other = other
  (<>) other _           = other

instance Show Color where
  show Transparent = " "
  show Black       = "X"
  show White       = "-"

readRow :: [Int] -> Int -> ([Int], [Int])
readRow pixels w = foldl (\(result, h:t) i -> (result ++ [h], t)) ([], pixels) [1 .. w]

readLayer :: [Int] -> Int -> Int -> ([[Int]], [Int])
readLayer pixels w h =
  foldl
    (\(rows, restData) i ->
       let (nextRow, nextData) = readRow restData w
        in (rows ++ [nextRow], nextData))
    ([], pixels)
    [1 .. h]

readImage :: [Int] -> Int -> Int -> Image Int
readImage pixels w h = readLayers [] pixels
  where
    readLayers curr restData =
      case restData of
        [] -> curr
        _ ->
          let (nextLayer, nextData) = readLayer restData w h
           in readLayers (curr ++ [nextLayer]) nextData

answer1 :: [Int] -> Int -> Int -> Int
answer1 pixels w h =
  let flattened = map join $ readImage pixels w h
      zeros = (map length . map (filter (== 0))) flattened
      ixLowest0 = (fst . head . sortBy (\(_, l1) (_, l2) -> compare l1 l2)) (zip [0 ..] zeros)
      layer = head . drop ixLowest0 $ flattened
   in (length (filter (== 1) layer)) * (length (filter (== 2) layer))

mapImage :: (a -> b) -> Image a -> Image b
mapImage f img = map (\layer -> (map (\row -> (map f row)) layer)) img

mergeRow :: Semigroup a => [a] -> [a] -> [a]
mergeRow a b = zipWith (<>) a b

mergeLayer :: Semigroup a => Layer a -> Layer a -> Layer a
mergeLayer a b = zipWith mergeRow a b

mergeImage :: Semigroup a => Image a -> Layer a
mergeImage img = foldl (\acc l -> mergeLayer acc l) (head img) (tail img)

answer2 :: [Int] -> Int -> Int -> IO [()]
answer2 pixels w h =
  let image = readImage pixels w h
      colored = mapImage mkColor image
      merged = mergeImage colored
      ios = map (\row -> putStrLn (foldl (\c x -> c ++ show x) "" row)) merged
   in S.sequence ios

solution :: IO ()
solution = do
  imageData <- (\s -> map (\c -> (ord c) - (ord '0')) s) <$> (readFile "input/input-day-8.txt")
  putStrLn $ "answer1 " ++ show (answer1 imageData 25 6)
  answer2 imageData 25 6
  putStrLn "Finished"
