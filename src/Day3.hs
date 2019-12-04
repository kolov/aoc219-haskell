module Day3
  ( solution
  ) where

import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import qualified Data.Set        as Set

data Point = Point
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show)

initialPoint = Point 0 0

data Path = Path
  { pos     :: Point
  , visited :: Map.Map Point Int
  , time    :: Int
  } deriving (Show)

initialPath = Path initialPoint (Map.fromList []) 0

moveBy :: Path -> Point -> Path
moveBy (Path pos visited time) (Point offsetX offsetY) =
  let newPos = Point {x = x pos + offsetX, y = y pos + offsetY}
      maybePos = Map.lookup newPos visited
      newVisited =
        case maybePos of
          Nothing -> Map.insert newPos (time + 1) visited
          Just _  -> visited
   in Path newPos newVisited (time + 1)

wirePoints :: [String] -> Path
wirePoints commands =
  foldl
    (\path command ->
       let len = read (drop 1 command) :: Int
           step =
             case (take 1 command) of
               "L" -> Point (-1) 0
               "R" -> Point 1 0
               "U" -> Point 0 1
               "D" -> Point 0 (-1)
        in foldl (\p _ -> (moveBy p step)) path [1 .. len])
    initialPath
    commands

distance :: Point -> Int
distance (Point x y) = abs x + abs y

points :: Path -> Set.Set Point
points path = (Set.fromList (Map.keys (visited path)))

timeToPoint :: Point -> Path -> Int
timeToPoint pt path =
  case Map.lookup pt (visited path) of
    Just t  -> t
    Nothing -> 0

answer1 :: [Point] -> Int
answer1 crossings =
  let distances = map distance crossings
   in head (sort distances)

answer2 :: [Path] -> [Point] -> Int
answer2 paths crossings =
  let times = map (\point -> foldl (\t path -> t + (timeToPoint point path)) 0 paths) crossings
   in head (sort times)

solution :: IO ()
solution = do
  content <- readFile "input/input-day-3.txt"
  wires <- return (map (\x -> splitOn "," x) (lines content))
  paths <- return $ map wirePoints wires
  crossings <-
    return $ (Set.toList (foldl (\set path -> Set.intersection set (points path)) (points (head paths)) (tail paths)))
  putStrLn $ "answer1 " ++ (show (answer1 crossings))
  putStrLn $ "answer2 " ++ (show (answer2 paths crossings))
