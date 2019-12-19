module Day13 where

import           Data.List
import           Data.List.Split
import qualified Data.Map         as M
import qualified Data.Set         as Set
import           Day9
import           Debug.Trace
import           System.IO.Unsafe

data Coordinates = Coordinates
  { x :: Integer
  , y :: Integer
  } deriving (Eq, Ord, Show)

type Screen = M.Map Coordinates Tile

data Game = Game
  { screen    :: Screen
  , ballPos   :: Coordinates
  , paddlePos :: Coordinates
  , prg       :: Program
  , score     :: Integer
  , numBlocks :: Integer
  } deriving (Show)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq, Show)

tileFromCode :: Integer -> Tile
tileFromCode 0 = Empty
tileFromCode 1 = Wall
tileFromCode 2 = Block
tileFromCode 3 = Paddle
tileFromCode 4 = Ball

updateBlocks :: Game -> Coordinates -> Tile -> Game
updateBlocks game coords newTile =
  let dBlocks =
        case (M.lookup coords (screen game), newTile) of
          (Just Block, Block) -> 0
          (Just Block, _)     -> (-1)
          (_, Block)          -> 1
          _                   -> 0
   in game {numBlocks = (numBlocks game) + dBlocks, screen = M.insert coords newTile (screen game)}

setTile :: Game -> Coordinates -> Tile -> Game
setTile game coords Paddle = (updateBlocks game coords Paddle) {paddlePos = coords}
setTile game coords Ball = (updateBlocks game coords Ball) {ballPos = coords}
setTile game coords tile = updateBlocks game coords tile

addCodetoGame :: Game -> Integer -> Integer -> Integer -> Game
addCodetoGame game (-1) 0 s = game {score = s}
addCodetoGame game x y code = setTile game (Coordinates x y) (tileFromCode code)

applyInstructions :: Game -> Game
applyInstructions game =
  case (output (prg game)) of
    x:y:code:t -> applyInstructions (addCodetoGame game x y code) {prg = (prg game) {output = t}}
    _ -> game

mkGame :: Memory -> Game
mkGame memory =
  Game
    { screen = M.empty
    , ballPos = Coordinates 0 0
    , paddlePos = Coordinates 0 0
    , prg = newProgram memory
    , score = 0
    , numBlocks = 0
    }

gameInput :: Game -> Integer
gameInput game = signum ((x . ballPos) game - (x . paddlePos) game)

playGame :: Game -> Game
playGame game =
  let nextProgram =
        executeUntil
          (\prg -> (status prg) == Finished || (status prg) == Waiting || (length (output prg)) > 2)
          (prg game)
      nextGame = applyInstructions game {prg = nextProgram}
   in case ((numBlocks nextGame), (status nextProgram)) of
        (_, Finished) -> nextGame
        (_, Waiting) -> playGame $ nextGame {prg = (nextProgram {input = [gameInput nextGame]})}
        (0, Running) -> nextGame
        (_, Running) -> playGame $ nextGame

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-13.txt"
  memory <- return $ (readCode . (map (\x -> read (x) :: Integer)) . (splitOn ",")) contents
  putStrLn $ "answer1 " ++ show (numBlocks (playGame (mkGame memory)))
  let game2 = mkGame (M.insert 0 2 memory)
  putStrLn $ "answer2 " ++ show (score (playGame game2))
