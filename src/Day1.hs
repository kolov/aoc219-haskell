module Day1
  ( solution
  ) where

import           Data.Monoid

fuelSimple :: (Integral a) => a -> a
fuelSimple mass = (mass `div` 3) - 2

fuelCompound :: (Integral a) => a -> a
fuelCompound mass =
  let f = fuelSimple mass
   in if (f <= 0)
        then 0
        else f + (fuelCompound f)

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-1.txt"
  modules <- return (map (\x -> read (x) :: Int) (lines contents))
  putStrLn $ "Answer1 " ++ show (sum (map fuelSimple modules))
  putStrLn $ "Answer2 " ++ show (sum (map fuelCompound modules))
