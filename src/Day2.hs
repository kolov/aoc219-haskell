module Day2
  ( solution
  ) where

import           Data.Array
import           Data.List
import           Data.List.Split
import           Data.Monoid

type P = Array Int Int

mkArray :: [Int] -> P
mkArray l =
  let lim = (length l) - 1
      arr = (array (0, lim) (zip [0 .. lim] l))
   in arr

patch :: P -> Int -> Int -> P
patch program verb noun = program//[(1, verb), (2, noun)]

executeAOp :: (Int -> Int -> Int) -> Int -> P -> P
executeAOp op pos program =
  let v1 = program ! (program ! (pos + 1))
      v2 = program ! (program ! (pos + 2))
      resultPos = program ! (pos + 3)
   in program // [(resultPos, (op v1 v2))]

executeOp :: Int -> P -> Int -> (Bool, P)
executeOp 1 program pos  = (False, executeAOp (+) pos program)
executeOp 2 program pos  = (False, executeAOp (*) pos program)
executeOp 99 program pos = (True, program)

execute :: Int -> P -> P
execute pos program =
  let (finished, next) = executeOp (program ! pos) program pos
   in if finished
        then next
        else execute (pos + 4) next

patchOutcome :: P -> Int -> Int -> Int
patchOutcome program verb noun =
  let patched = patch program verb noun
      result = execute 0 patched
   in result ! 0

answer1 :: P -> Int
answer1 p = patchOutcome p 12 2

answer2 :: P -> Int
answer2 p =
  let outcomes = map (\i -> (i, patchOutcome p (i `div` 100) (i `mod` 100))) [0 .. 10000]
      filtered = filter (\(ix, outcome) -> outcome == 19690720) outcomes
   in head (map fst filtered)

solution :: IO ()
solution = do
  contents <- readFile "input/input-day-2.txt"
  l <- return ((map (\x -> read (x) :: Int) (splitOn "," (intercalate "," (lines contents)))))
  program <- return (mkArray l)
  putStrLn $ "answer1 " ++ (show (answer1 program))
  putStrLn $ "answer2 " ++ (show (answer2 program))
