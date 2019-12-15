module Day9 where

import           Data.List
import           Data.List.Split
import qualified Data.Map        as Map
import           Data.Monoid
import           Debug.Trace

type Address = Integer

type Value = Integer

type Memory = Map.Map Address Value

readMem :: Memory -> Address -> Value
readMem prg pos =
  case Map.lookup pos prg of
    Just v  -> v
    Nothing -> 0

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

data Program = Program
  { input   :: [Integer]
  , output  :: [Integer]
  , program :: Memory
  , pointer :: Integer
  , status  :: Status
  , base    :: Integer
  }
  
newProgram :: Memory -> Program
newProgram memory = Program [] [] memory 0 Running 0

instance Show Program where
  show (Program input output program pointer status base) =
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

data OpCode
  = Add Value Value
  | Multiply

readCode :: [Integer] -> Memory
readCode l =
  let lim = (length l) - 1
      arr = Map.fromList (zip (map toInteger [0 .. lim]) l)
   in arr

getVal :: Memory -> Integer -> Integer -> Mode -> Integer
getVal prg _ pos Position    = readMem prg (readMem prg pos)
getVal prg _ pos Immediate   = readMem prg pos
getVal prg base pos Relative = readMem prg ((readMem prg pos) + base)

putVal :: Memory -> Integer -> Integer -> Integer -> Mode -> Memory
putVal prg base pos val Position = Map.insert (readMem prg pos) val prg
putVal prg base pos val Relative = Map.insert ((readMem prg pos) + base) val prg

executeOp :: Program -> Program
executeOp program@(Program i out prg ptr fi base) =
  case (code inst) of
    1 -> nextEnv (update3 (val1 + val2)) (ptr + 4)
    2 -> nextEnv (update3 (val1 * val2)) (ptr + 4)
    3 ->
      case i of
        []  -> program { status = Waiting}
        h:t -> Program t out (update1 h) (ptr + 2) Running base
    4 -> Program i (out ++ [val1]) prg (ptr + 2) Running base
    5 ->
      program
        { pointer =
            if val1 /= 0
              then val2
              else (ptr + 3)
        }
    6 ->
      program
        { pointer =
            if val1 == 0
              then val2
              else (ptr + 3)
        }
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
    9 -> Program i out prg (ptr + 2) Running (base + val1)
    99 -> program {status = Finished}
    a -> Program i [0, 0, ptr] prg ptr Finished base -- debug info in case of error
  where
    inst = (mkInstruction (fromIntegral (readMem prg ptr)))
    val1 = getVal prg base (ptr + 1) (mode1 inst)
    val2 = getVal prg base (ptr + 2) (mode2 inst)
    update3 val = putVal prg base (ptr + 3) val (mode3 inst)
    update1 val = putVal prg base (ptr + 1) val (mode1 inst)
    nextEnv nextProgram nextPtr = Program i out nextProgram nextPtr Running base

execute :: Program -> Program
execute e =
  let opcode = (readMem (program e) (pointer e))
      next = (executeOp e)
   in case status next of
        Finished -> next
        Waiting  -> next
        Running  -> execute next

--      n = trace ("executed " ++ show opcode ++ " -> " ++ show next) next
solution :: IO ()
solution = do
  contents <- readFile "input/input-day-9.txt"
  program <- return $ (readCode . (map (\x -> read (x) :: Integer)) . (splitOn ",")) contents
  putStrLn $ "answer1 " ++ (show (output (execute (Program [1] [] program 0 Running 0))))
  putStrLn $ "answer2 " ++ (show (output (execute (Program [2] [] program 0 Running 0))))
