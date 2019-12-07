module Day6
  ( solution
  ) where

import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map

allParents :: Ord a => [a] -> Map.Map a a -> a -> [a]
allParents current m k =
  case Map.lookup k m of
    Nothing     -> current
    Just parent -> allParents (current ++ [parent]) m parent

numParents :: Ord a => Int -> Map.Map a a -> a -> Int
numParents current m k =
  case Map.lookup k m of
    Nothing     -> current
    Just parent -> numParents (1 + current) m parent

flatten :: [Maybe a] -> [a]
flatten ls = [x | Just x <- ls]

numTransfers :: Ord a => Map.Map a a -> a -> a -> Int
numTransfers orbits a1 a2 =
  let p1 = allParents [] orbits a1
      p2 = allParents [] orbits a2
      common = intersect p1 p2
      distances = map (\x -> (fmap (+) (elemIndex x p1)) <*> (elemIndex x p2)) common
      maxLen = max (length p1) (length p2)
   in (head . sort . flatten) distances

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-6.txt"
  orbits <- return $ Map.fromList (map ((\[a, b] -> (b, a)) . (splitOn ")")) (lines contents))
  answer1 <- return $ sum (map (numParents 0 orbits) (Map.keys orbits))
  putStrLn $ "answer1 " ++ (show answer1)
  putStrLn $ "answer2 " ++ (show (numTransfers orbits "YOU" "SAN"))
