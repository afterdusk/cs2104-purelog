module Main where

import System.Environment
import qualified Data.Map.Strict as M
import Parser
import AST
import Engine

import Debug.Trace 

data State = Idle | Search Rel Tree Int

showTruth :: Int -> IO ()
showTruth nSols = trace (show nSols) (if nSols == 0 then putStrLn "false" else putStrLn "true")

printSolution :: Rel -> Subs -> IO ()
-- printSolution takes query rel and subs, extracting variables and printing
-- their substitutions
printSolution query table = traverse printVar (variables query) >> return ()
  where
    printVar v@(Var x) = putStrLn $ x ++ " = " ++ show (resolve table v)

interpret :: Program -> Rel -> [Subs]
interpret prog rel = searchAll prog (initTree rel)

{- allow a goal to be typed -}
run_prog :: Program -> IO ()
run_prog program = do
  putStrLn "type a goal: "
  -- retrieve string from IO monad
  command <- getLine
  -- call parseRel on string
  case parseRel command of
      --  pattern match on result
      Right rel -> 
        do
          -- call interpret on program and rel
          let answers = interpret program rel
          -- bind to traverse function, called on partially applied printSolution
          -- over the answers
          (traverse (printSolution rel) answers
          --  bind to showTruths
            >> showTruth (length answers))
      -- print error and... try again?
      Left err  -> print err >> run_prog program

{- reading prolog file as argument -}
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile (head args)
      case parseProgram source of
        -- Right program -> putStrLn "Loaded successfully" >> run_prog program
        Right program -> putStrLn (show program) >> run_prog program
        Left err -> print err
    _ -> putStrLn "Usage: purelog <filename>"
  return ()
