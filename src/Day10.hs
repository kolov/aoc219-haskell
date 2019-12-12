module Day10 where

import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import           Data.Monoid
import           Data.Ratio
import qualified Data.Vector     as V
import           Debug.Trace

type Row = [Bool]

type AMap = [Row]

type Coordinates = (Int, Int)

at :: Int -> Int -> AMap -> Bool
at x y m = (m !! y) !! x

rowToString :: Row -> String
rowToString r =
  map
    (\c ->
       if c
         then '#'
         else '.')
    r

--  get a row with asteroids at y-distance distToRow from observer
--  obstructed by obstructingRow at distance distToObstructing
--  and return only visible asteroids
shadowed :: Int -> Int -> Row -> Int -> Row -> Row
shadowed x distToObstructing obstructingRow distToRow row =
  let len = length row
      r = distToObstructing % distToRow
      vResult =
        foldl
          (\vRow (ix, occupied) ->
             if occupied
               then let dx = ((ix - x) * distToRow) % distToObstructing
                        destX = x + (numerator dx)
                     in if ((denominator dx) == 1 && destX < len && destX >= 0)
                          then (vRow V.// [(destX, False)])
                          else vRow
               else vRow)
          (V.fromList row)
          (zip [0 ..] obstructingRow)
   in (V.toList vResult)

--   trace
--        ("x=" ++
--         show x ++
--         "row " ++
--         (rowToString row) ++
--         " obstructed by " ++
--         (rowToString obstructingRow) ++
--         "from " ++ (show distToObstructing) ++ "/" ++ show distToRow ++ " -> " ++ (rowToString (V.toList vResult)))
shadowedByRows :: Int -> [Row] -> Row -> Row
shadowedByRows x obstructingRows row =
  let distToRow = length obstructingRows + 1
   in foldl
        (\result (obstructing, distObstruct) -> shadowed x distObstruct obstructing distToRow result)
        row
        (zip obstructingRows [1 ..])

seesUp :: Int -> Int -> AMap -> [Coordinates]
seesUp x y m = seesThrough x y (-1) (reverse (take y m))

seesDown :: Int -> Int -> AMap -> [Coordinates]
seesDown x y m = seesThrough x y 1 (drop (y + 1) m)

seesThrough :: Int -> Int -> Int -> [Row] -> [Coordinates]
seesThrough x y dy rows =
  let (_, total) =
        foldl
          (\(obstructingRows, found) examinedRow ->
             let shadowedRow = shadowedByRows x obstructingRows examinedRow
                 xPositions = map snd (filter fst (zip shadowedRow [0 ..]))
                 positions = map (\x -> (x, y + dy + dy * (length obstructingRows))) xPositions
              in (obstructingRows ++ [examinedRow], found ++ positions))
          ([], [])
          rows
   in total

seesOnRow :: Int -> Int -> AMap -> [Coordinates]
seesOnRow x y m =
  let indexed = zip (head (drop y m)) [0 ..]
      onLeft =
        case filter fst (reverse (take x indexed)) of
          h:t -> [(snd h, y)]
          _   -> []
      onRight =
        case filter fst (drop (x + 1) indexed) of
          h:t -> [(snd h, y)]
          _   -> []
   in onLeft ++ onRight

sees :: Int -> Int -> AMap -> [Coordinates]
sees x y m = (seesOnRow x y m) ++ (seesUp x y m) ++ (seesDown x y m)

asteroids :: AMap -> [(Int, Int)]
asteroids m = [(x, y) | x <- [0 .. (length (head m) - 1)], y <- [0 .. (length m) - 1], at x y m]

mostVisible :: AMap -> (Coordinates, Int, [Coordinates])
mostVisible m =
  head $
  sortBy (\(_, _, c1) (_, _, c2) -> compare (length c2) (length c1)) $
  map
    (\(x, y) ->
       let visible = sees x y m
        in ((x, y), length visible, visible))
    (asteroids m)

shootingOrder :: AMap -> [Coordinates]
shootingOrder m =
  let ((x, y), _, asteroids) = mostVisible m
   in sortBy (\(x1, y1) (x2, y2) -> compare (angle (x1 - x) (y1 - y)) (angle (x2 - x) (y2 - y))) asteroids

mapFromLines :: String -> AMap
mapFromLines contents = map (map (== '#')) (lines contents)

example1 =
  ".#..#\n\
            \.....\n\
            \#####\n\
            \....#\n\
            \...##"

example2 =
  "......#.#.\n\
            \#..#.#....\n\
            \..#######.\n\
            \.#.#.###..\n\
            \.#..#.....\n\
            \..#....#.#\n\
            \#..#....#.\n\
            \.##.#..###\n\
            \##...#..#.\n\
            \.#....####"

example3 =
  ".#..##.###...#######\n\
            \##.############..##.\n\
            \.#.######.########.#\n\
            \.###.#######.####.#.\n\
            \#####.##.#.##.###.##\n\
            \..#####..#.#########\n\
            \####################\n\
            \#.####....###.#.#.##\n\
            \##.#################\n\
            \#####.##.###..####..\n\
            \..######..##.#######\n\
            \####.##.####...##..#\n\
            \.#####..#.######.###\n\
            \##...#.##########...\n\
            \#.##########.#######\n\
            \.####.#.###.###.#.##\n\
            \....##.##.###..#####\n\
            \.#.#.###########.###\n\
            \#.#.#.#####.####.###\n\
            \###.##.####.##.#..##"

example0 =
  "###\n\
            \.#.\n\
            \###"

m0 = mapFromLines example0

sign :: Integer -> Integer
sign 0 = 0
sign x =
  if x > 0
    then 1
    else -1

angle :: Int -> Int -> Double
angle idx idy =
  case (sign dx, sign dy) of
    (0, -1)  -> 0
    (1, -1)  -> (atan . fromRational) (-dy % dx)
    (1, 0)   -> pi / 2
    (1, 1)   -> (atan . fromRational) (dy % dx) + pi / 2
    (0, 1)   -> pi
    (-1, -1) -> (atan . fromRational) (dy % dx) + pi
    (-1, 0)  -> pi * 3 / 4
    (-1, 1)  -> (atan . fromRational) (-dy % dx) + pi * 3 / 4
  where
    dx = fromIntegral idx
    dy = fromIntegral idy

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-10.txt"
--  putStrLn $ "answer example1: " ++ show (mostVisible (mapFromLines example1))
--  putStrLn $ "answer example2: " ++ show (mostVisible (mapFromLines example2))
--  putStrLn $ "answer example3: " ++ show (mostVisible (mapFromLines example3))
  putStrLn $ "answer day1 " ++ show (mostVisible (mapFromLines contents))
  putStrLn $ "answer day2 " ++ show (drop 199 (shootingOrder (mapFromLines contents)))
--  putStrLn contents
