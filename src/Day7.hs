module Day7
  ( solution
  ) where

import           Data.Array
import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import           Debug.Trace

type P = Array Int Int

data Mode
  = Position
  | Immediate

data Instruction = Instruction
  { code  :: Int
  , mode1 :: Mode
  , mode2 :: Mode
  }

data Status
  = Running
  | Finished
  | Waiting
  deriving (Eq, Show)

data ProgramEnv = ProgramEnv
  { input   :: [Int]
  , output  :: [Int]
  , program :: P
  , pointer :: Int
  , status  :: Status
  }

instance Show ProgramEnv where
  show (ProgramEnv input output program pointer status) =
    (show input) ++ "," ++ show output ++ "," ++ show pointer ++ ",=" ++ show status

mkMode :: Int -> Mode
mkMode 0 = Position
mkMode 1 = Immediate

mkInstruction :: Int -> Instruction
mkInstruction n = Instruction (n `mod` 100) (mkMode ((n `div` 100) `mod` 10)) (mkMode ((n `div` 1000) `mod` 10))

mkArray :: [Int] -> P
mkArray l =
  let lim = (length l) - 1
      arr = (array (0, lim) (zip [0 .. lim] l))
   in arr

getVal :: P -> Int -> Mode -> Int
getVal prg pos Position  = prg ! (prg ! pos)
getVal prg pos Immediate = prg ! pos

executeOp :: ProgramEnv -> ProgramEnv
executeOp (ProgramEnv i out prg ptr fi) =
  case (code inst) of
    1 -> nextEnv (update 3 (val1 + val2)) (ptr + 4)
    2 -> nextEnv (update 3 (val1 * val2)) (ptr + 4)
    3 ->
      case i of
        h:t -> ProgramEnv t out (update 1 h) (ptr + 2) Running
        []  -> ProgramEnv i out prg ptr Running
    4 -> ProgramEnv i (out ++ [val1]) prg (ptr + 2) Running
    5 ->
      updatePtr
        (if val1 /= 0
           then val2
           else (ptr + 3))
    6 ->
      updatePtr
        (if val1 == 0
           then val2
           else (ptr + 3))
    7 ->
      nextEnv
        (update
           3
           (if val1 < val2
              then 1
              else 0))
        (ptr + 4)
    8 ->
      nextEnv
        (update
           3
           (if val1 == val2
              then 1
              else 0))
        (ptr + 4)
    99 -> ProgramEnv i out prg (1 `div` 0) Finished
    a -> ProgramEnv i [0, 0, a, ptr] prg ptr Finished -- debug info in case of error
  where
    inst = (mkInstruction (prg ! ptr))
    val1 = getVal prg (ptr + 1) (mode1 inst)
    val2 = getVal prg (ptr + 2) (mode2 inst)
    update offset val = prg // [(prg ! (ptr + offset), val)]
    nextEnv nextProgram nextPtr = ProgramEnv i out nextProgram nextPtr Running
    updatePtr nextPtr = ProgramEnv i out prg nextPtr Running

executeUntil :: ProgramEnv -> (ProgramEnv -> Bool) -> ProgramEnv
executeUntil env cond =
  case (cond env) of
    True  -> env
    False -> executeUntil (executeOp env) cond

isFinished :: ProgramEnv -> Bool
isFinished env = (status env) == Finished

hasOutput :: ProgramEnv -> Bool
hasOutput env = (length . output) env > 0

signalFromPhases :: [Int] -> P -> Int
signalFromPhases phases program =
  foldl
    (\input phase ->
       let next = executeUntil (ProgramEnv [phase, input] [] program 0 Running) hasOutput
        in head (output next))
    0
    phases

runOneCycle :: [ProgramEnv] -> Int -> ([ProgramEnv], Maybe Int)
runOneCycle envs inVal =
  foldl
    (\(result, nextInput) env ->
       case nextInput of
         Nothing -> (result ++ [env], Nothing)
         Just somInput ->
           let executed = executeUntil (addInput env somInput) (\e -> (hasOutput e) || (isFinished e))
               (nextProgram, output) = readOutput executed
            in (result ++ [nextProgram], output))
    ([], Just inVal)
    envs

-- runs a list of amplifiers until the last is finished
runFullCycle :: [ProgramEnv] -> Int -> Int
runFullCycle envs inVal =
  let (nextEnvs, maybeOutput) = runOneCycle envs inVal
   in case maybeOutput of
        Nothing -> inVal
        Just n  -> (runFullCycle nextEnvs n)

amplifiers :: [Int] -> P -> [ProgramEnv]
amplifiers phases program = map (\phase -> (ProgramEnv [phase] [] program 0 Running)) phases

addInput :: ProgramEnv -> Int -> ProgramEnv
addInput env inVal = env {input = (input env) ++ [inVal]}

readOutput :: ProgramEnv -> (ProgramEnv, Maybe Int)
readOutput env =
  case (output env) of
    []  -> (env, Nothing)
    h:t -> (env {output = t}, Just h)

allPhases :: Int -> Int -> [[Int]]
allPhases from to =
  Data.List.nub
    [ [a, b, c, d, e]
    | a <- [from .. to]
    , b <- [from .. to]
    , c <- [from .. to]
    , d <- [from .. to]
    , e <- [from .. to]
    , a /= b && a /= c && a /= d && a /= e && b /= c && b /= d && b /= e && c /= d && c /= e && d /= e
    ]

maxSignal :: [[Int]] -> P -> Int
maxSignal allPhases program = (head . reverse . sort) (map (\phase -> signalFromPhases phase program) allPhases)

maxSignalWithFeedback :: [[Int]] -> P -> Int
maxSignalWithFeedback allPhases program =
  (head . reverse . sort) (map (\phase -> runFullCycle (amplifiers phase program) 0) allPhases)

solution :: IO ()
solution = do
  content <- readFile "input/input-day-7.txt"
  program <- return $ (mkArray . (map (\x -> read (x) :: Int)) . (splitOn ",")) content
  putStrLn $ "answer1 " ++ (show (maxSignal (allPhases 0 4) program))
  putStrLn $ "answer2 " ++ (show (maxSignalWithFeedback (allPhases 5 9) program))
