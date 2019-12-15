module Day11 where

import           Data.List
import           Data.List.Split
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Day9
import           Debug.Trace
import           System.IO.Unsafe

data Coordinates = Coordinates
  { x :: Integer
  , y :: Integer
  } deriving (Eq, Ord, Show)

type Color = Integer

type Surface = Map.Map Coordinates Color

data Direction
  = Up
  | LLeft
  | Down
  | RRight
  deriving (Show)

toLeft :: Direction -> Direction
toLeft d =
  case d of
    Up     -> LLeft
    LLeft  -> Down
    Down   -> RRight
    RRight -> Up

toRight :: Direction -> Direction
toRight d = (toLeft . toLeft . toLeft) d

data Robot = Robot
  { position  :: Coordinates
  , direction :: Direction
  , visited   :: Set.Set Coordinates
  , surface   :: Surface
  } deriving (Show)

newRobot :: Integer -> Robot
newRobot initial =
  Robot
    { position = Coordinates {x = 0, y = 0}
    , direction = Up
    , visited = Set.empty
    , surface = Map.fromList [((Coordinates 0 0), initial)]
    }

getColorAt :: Coordinates -> Map.Map Coordinates Color -> Color
getColorAt coords hull =
  case Map.lookup coords hull of
    Just color -> color
    Nothing    -> 0

getRobotInput :: Robot -> Color
getRobotInput robot = getColorAt (position robot) (surface robot)

moveOneStep :: Robot -> Robot
moveOneStep robot =
  let currP = position robot
      nextPos =
        case direction robot of
          Up     -> currP {y = (y currP) + 1}
          RRight -> currP {x = x currP + 1}
          Down   -> currP {y = (y currP) - 1}
          LLeft  -> currP {x = x currP - 1}
   in robot {position = nextPos}

runRobot1 :: Robot -> Integer -> Integer -> Robot
runRobot1 robot@Robot {position = p, direction = d, visited = v, surface = s} paint move =
  let painted = robot {surface = Map.insert p paint s, visited = Set.insert p v}
      rotated =
        painted
          { direction =
              case move of
                0 -> toLeft d
                1 -> toRight d
          }
      moved = moveOneStep rotated
   in moved

runInstructionsOnRobot :: Robot -> [Integer] -> Robot
runInstructionsOnRobot robot [] = robot
runInstructionsOnRobot robot instructions = runRobot1 robot (head instructions) (head (tail instructions))

runProgramOnRobot :: Robot -> Program -> (Robot, Program)
runProgramOnRobot robot program =
  let nextProgram = execute program
   in case ((status nextProgram), (output nextProgram)) of
        (Finished, instructions) -> (runInstructionsOnRobot robot instructions, nextProgram)
        (Waiting, instructions) ->
          let nextRobot = runInstructionsOnRobot robot instructions
           in runProgramOnRobot nextRobot nextProgram {input = [getRobotInput nextRobot], output = []}

runRobot :: Robot -> Memory -> Robot
runRobot robot memory = fst (runProgramOnRobot robot (newProgram memory))

answer1 :: Memory -> Int
answer1 memory = length (visited (runRobot (newRobot 0) memory))

answer2 :: Memory -> IO ()
answer2 memory =
  let hull = (surface (runRobot (newRobot 1) memory))
      cs = (Map.keys hull)
      minx = minimum $ map x cs
      maxx = maximum $ map x cs
      miny = minimum $ map y cs
      maxy = maximum $ map y cs
      getColorChar x y = case getColorAt (Coordinates x y) hull of
                         0 -> '#'
                         1 -> '.'
      getRow y = (\x -> getColorChar x y) <$> [x | x <- [minx .. maxx]]
      rows = map (\y -> putStrLn (show (getRow y))) [y | y <- [maxy,(maxy - 1) .. miny]]
   in const () <$> sequence rows

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-11.txt"
  memory <- return $ (readCode . (map (\x -> read (x) :: Integer)) . (splitOn ",")) contents
  putStrLn $ "answer1 " ++ show (answer1 memory)
  putStrLn $ "answer2 "
  answer2 memory
