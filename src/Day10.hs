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

seesUp :: Int -> Int -> AMap -> Int
seesUp x y m = seesThrough x (reverse (take y m))

seesDown :: Int -> Int -> AMap -> Int
seesDown x y m = seesThrough x (drop (y + 1) m)

seesThrough :: Int -> [Row] -> Int
seesThrough x rows =
  let (_, total) =
        foldl
          (\(obstructingRows, count) examinedRow ->
             let shadowedRow = shadowedByRows x obstructingRows examinedRow
              in (obstructingRows ++ [examinedRow], count + (length (filter id shadowedRow))))
          ([], 0)
          rows
   in total

seesOnRow :: Int -> Int -> AMap -> Int
seesOnRow x y m =
  let hasAsteroids r = (fromEnum . (> 0) . length . filter id) r
      row = head (drop y m)
   in hasAsteroids (take x row) + hasAsteroids (drop (x + 1) row)

sees :: Int -> Int -> AMap -> Int
sees x y m = (seesOnRow x y m) + (seesUp x y m) + (seesDown x y m)

asteroids :: AMap -> [(Int, Int)]
asteroids m = [(x, y) | x <- [0 .. (length (head m) - 1)], y <- [0 .. (length m) - 1], at x y m]

mostVisible :: AMap -> [(Int, Int, Int)]
mostVisible m = sortBy (\(_, _, c1) (_, _, c2) -> compare c1 c2) $ map (\(x, y) -> (x, y, sees x y m)) (asteroids m)

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

a = [[True, True, True], [False, True, False], [True, True, True]]

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-10.txt"
  putStrLn $ "answer example1: " ++ show (mostVisible (mapFromLines example1))
  putStrLn $ "answer example2: " ++ show (mostVisible (mapFromLines example2))
  putStrLn $ "answer example3: " ++ show (mostVisible (mapFromLines example3))
  putStrLn $ "answer day1 " ++ show (mostVisible (mapFromLines contents))
  putStrLn contents
