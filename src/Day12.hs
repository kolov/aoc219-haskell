{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Day12 where

import           Data.List
import           Data.List        (transpose)
import           Data.List.Split
import qualified Data.Map         as Map
import           Data.Map.Strict  (Map, empty, insert, member, (!))
import qualified Data.Set         as Set
import           Debug.Trace
import           System.IO.Unsafe

data Pt =
  Pt Int
     Int
     Int
  deriving (Show)

instance Semigroup Pt where
  (<>) (Pt x1 y1 z1) (Pt x2 y2 z2) = Pt (x1 + x2) (y1 + y2) (z1 + z2)

instance Monoid Pt where
  mempty = Pt 0 0 0

data Moon = Moon
  { position :: Pt
  , velocity :: Pt
  , forces   :: Pt
  } deriving (Show)

point0 = Pt 0 0 0

sign :: Int -> Int
sign a =
  if (a > 0)
    then 1
    else if (a == 0)
           then 0
           else -1

addForce :: Moon -> Moon -> Moon
addForce this@(Moon (Pt x1 y1 z1) _ _) (Moon (Pt x2 y2 z2) _ _) =
  this {forces = (forces this) <> Pt (sign $ x2 - x1) (sign $ y2 - y1) (sign $ z2 - z1)}

applyOtherForces :: Moon -> [Moon] -> Moon
applyOtherForces this all = foldl (\t other -> addForce t other) (this {forces = point0}) all

applyAllForces :: [Moon] -> [Moon]
applyAllForces ms = map (\m -> applyOtherForces m ms) ms

step :: [Moon] -> [Moon]
step ms =
  let withForces = applyAllForces ms
      withVelocities = map (\m -> m {velocity = (velocity m) <> (forces m)}) withForces
   in map (\m -> m {position = (position m) <> (velocity m)}) withVelocities

steps :: Int -> [Moon] -> [Moon]
steps n ms = foldl (\ms _ -> step ms) ms [1 .. n]

sumC :: Pt -> Integer
sumC (Pt x y z) = toInteger (abs(x) + abs(y) + abs(z))

energy :: [Moon] -> Integer
energy ms = sum $ map (\m -> (sumC (position m)) * (sumC (velocity m))) ms

parseMoonPositions :: String -> [Moon]
parseMoonPositions str =
  let parseLine :: String -> Pt
      parseLine =
        (\[x, y, z] -> Pt x y z) . (map (read :: String -> Int)) . (splitOn ",") . (filter (flip elem "0123456789,-"))
   in map ((\pos -> Moon pos point0 point0) . parseLine) (lines str)

solution :: IO ()
solution = do
  input <- readFile "input/input-day-12.txt"
  let moons = parseMoonPositions input
  putStrLn $ "answer1: " ++ show (energy (steps 1000 moons))
--  putStrLn $ show (step moons)
