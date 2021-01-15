{-# OPTIONS_GHC -Wall #-}
{-
This function contains the main logic.
-}

module Main where

-- User modules
import Interface
import Execution
import Emulator

-- Library modules
import System.Exit
import System.Environment
import Control.Monad
import Control.Monad.State


-- Usage
usage :: String -> String
usage prog_name = "usage: " ++ prog_name ++ " PROG_NAME"

main :: IO ()
main = do
  -- Read information from the environment
  args <- getArgs
  prog_name <- getProgName

  -- Validate arguments
  when ((length args) == 0) $ do
    putStrLn $ usage prog_name
    exitFailure

  -- Read in the input program
  let fname = args !! 0
  program_contents <- readFile fname

  -- Execute the program
  let instructions = readProgram program_contents
      program = initializeProgramState instructions

  runStateT emulate program >> return ()

