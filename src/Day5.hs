module Day5
  ( solution
  ) where

import           Data.Array
import           Data.List
import           Data.List.Split
import           Data.Monoid

type P = Array Int Int

data Mode
  = Position
  | Immediate

data Instruction = Instruction
  { code  :: Int
  , mode1 :: Mode
  , mode2 :: Mode
  }

data ProgramEnv = ProgramEnv
  { input    :: [Int]
  , output   :: [Int]
  , program  :: P
  , pointer  :: Int
  , finished :: Bool
  } deriving (Show)

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
    3 -> ProgramEnv (drop 1 i) out (update 1 (head i)) (ptr + 2) False
    4 -> ProgramEnv i (out ++ [val1]) prg (ptr + 2) False
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
    99 -> ProgramEnv i out prg ptr True
    a -> ProgramEnv i [0, 0, a, ptr] prg ptr True -- debug info in case of error
  where
    inst = (mkInstruction (prg ! ptr))
    val1 = getVal prg (ptr + 1) (mode1 inst)
    val2 = getVal prg (ptr + 2) (mode2 inst)
    update offset val = prg // [(prg ! (ptr + offset), val)]
    nextEnv nextProgram nextPtr = ProgramEnv i out nextProgram nextPtr False
    updatePtr nextPtr = ProgramEnv i out prg nextPtr False

execute :: ProgramEnv -> ProgramEnv
execute e =
  let next = (executeOp e)
   in case finished next of
        True  -> next
        False -> execute next

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-5.txt"
  program <- return $ (mkArray . (map (\x -> read (x) :: Int)) . (splitOn ",")) contents
  env <- return $ ProgramEnv [5] [] program 0 False
  putStrLn $ "answer1 " ++ (show (output (execute env)))
