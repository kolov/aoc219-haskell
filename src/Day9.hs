module Day9 where

import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import           Data.Monoid
import           Debug.Trace

type P = Map.Map Integer Integer

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Show)

data Status
  = Running
  | Finished
  | Waiting
  deriving (Eq, Show)

data Instruction = Instruction
  { code  :: Int
  , mode1 :: Mode
  , mode2 :: Mode
  , mode3 :: Mode
  } deriving (Show)

data ProgramEnv = ProgramEnv
  { input   :: [Integer]
  , output  :: [Integer]
  , program :: P
  , pointer :: Integer
  , status  :: Status
  , base    :: Integer
  }

instance Show ProgramEnv where
  show (ProgramEnv input output program pointer status base) =
    (show input) ++ "," ++ show output ++ "," ++ show pointer ++ "," ++ show status ++ "," ++ show base

mkMode :: Int -> Mode
mkMode 0 = Position
mkMode 1 = Immediate
mkMode 2 = Relative

mkInstruction :: Int -> Instruction
mkInstruction n =
  Instruction
    (n `mod` 100)
    (mkMode ((n `div` 100) `mod` 10))
    (mkMode ((n `div` 1000) `mod` 10))
    (mkMode ((n `div` 10000) `mod` 10))

mkArray :: [Integer] -> P
mkArray l =
  let lim = (length l) - 1
      arr = Map.fromList (zip (map toInteger [0 .. lim]) l)
   in arr

readMem :: P -> Integer -> Integer
readMem prg pos =
  case Map.lookup pos prg of
    Just v  -> v
    Nothing -> 0

getVal :: P -> Integer -> Integer -> Mode -> Integer
getVal prg _ pos Position    = readMem prg (readMem prg pos)
getVal prg _ pos Immediate   = readMem prg pos
getVal prg base pos Relative = readMem prg ((readMem prg pos) + base)

putVal :: P -> Integer -> Integer -> Integer -> Mode -> P
putVal prg base pos val Position = Map.insert (readMem prg pos) val prg
putVal prg base pos val Relative = Map.insert ((readMem prg pos) + base) val prg

--putVal prg base pos val Relative = Map.insert pos val prg
executeOp :: ProgramEnv -> ProgramEnv
executeOp (ProgramEnv i out prg ptr fi base) =
  case (code inst) of
    1 -> nextEnv (update3 (val1 + val2)) (ptr + 4)
    2 -> nextEnv (update3 (val1 * val2)) (ptr + 4)
    3 -> ProgramEnv (drop 1 i) out (update1 (head i)) (ptr + 2) Running base
    4 -> ProgramEnv i (out ++ [val1]) prg (ptr + 2) Running base
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
        (update3
           (if val1 < val2
              then 1
              else 0))
        (ptr + 4)
    8 ->
      nextEnv
        (update3
           (if val1 == val2
              then 1
              else 0))
        (ptr + 4)
    9 -> ProgramEnv i out prg (ptr + 2) Running (base + val1)
    99 -> ProgramEnv i out prg ptr Finished base
    a -> ProgramEnv i [0, 0, ptr] prg ptr Finished base -- debug info in case of error
  where
    inst = (mkInstruction (fromIntegral (readMem prg ptr)))
    val1 = getVal prg base (ptr + 1) (mode1 inst)
    val2 = getVal prg base (ptr + 2) (mode2 inst)
    update3 val = putVal prg base (ptr + 3) val (mode3 inst)
    update1 val = putVal prg base (ptr + 1) val (mode1 inst)
    nextEnv nextProgram nextPtr = ProgramEnv i out nextProgram nextPtr Running base
    updatePtr nextPtr = ProgramEnv i out prg nextPtr Running base

execute :: ProgramEnv -> ProgramEnv
execute e =
  let opcode = (readMem (program e) (pointer e))
      next = (executeOp e)
      n = trace ("executed " ++ show opcode ++ " -> " ++ show next) next
   in case status n of
        Finished -> next
        Waiting  -> next
        Running  -> execute next

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-9.txt"
  program <- return $ (mkArray . (map (\x -> read (x) :: Integer)) . (splitOn ",")) contents
--      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
  env <- return $ ProgramEnv [1] [] program 0 Running 0
  putStrLn $ "answer1 " ++ (show (output (execute env)))