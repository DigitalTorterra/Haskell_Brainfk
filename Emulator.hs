{-# OPTIONS_GHC -Wall #-}
{-
This file contains the emulation loop,
and some monadic wrappers for ProgramState manipulation
-}

module Emulator where

-- User modules
import Interface
import Execution

-- Library modules
import Data.Char
import Data.Maybe
import Control.Monad.Loops
import Control.Monad.State

-- Main emulation loop
emulate :: StateT ProgramState IO ()
emulate = do
  whileM_ running (do
                      curr_cmd <- getStateInstructionM
                      case curr_cmd of PointerDecr -> moveDataPointerM (-1)
                                       PointerIncr -> moveDataPointerM 1
                                       ByteDecr -> changeDataM (-1)
                                       ByteIncr -> changeDataM 1
                                       ByteOutput -> outputCharacterM
                                       ByteInput -> writeCharacterM
                                       JumpForward -> jumpProgramM
                                       JumpBackward -> jumpProgramM
                      incrementProgramCounterM
               )

liftStateFunction :: (a -> a) -> StateT a IO ()
liftStateFunction f = do
  my_state <- get
  put $ f my_state
  return ()

printCurrentState :: StateT ProgramState IO ()
printCurrentState = do
  my_state <- get
  let curr_state = fromJust . getStateInstruction $ my_state
      curr_char = fromJust . readMemory . snd $ my_state
  liftIO . putStrLn $ "Instruction: " ++ (show curr_state) ++ ", Memory: " ++ (show (ord curr_char))

running :: StateT ProgramState IO Bool
running = do
  my_state <- get
  return . not . isFinished $ my_state

getStateInstructionM :: StateT ProgramState IO Command
getStateInstructionM = do
  my_state <- get
  return . fromJust . getStateInstruction $ my_state

incrementProgramCounterM :: StateT ProgramState IO ()
incrementProgramCounterM = liftStateFunction incrementProgramCounter

moveDataPointerM :: Int -> StateT ProgramState IO ()
moveDataPointerM x = liftStateFunction $ moveDataPointer x

changeDataM :: Int -> StateT ProgramState IO ()
changeDataM x = liftStateFunction $ changeData x

outputCharacterM :: StateT ProgramState IO ()
outputCharacterM = do
  my_state <- get
  liftIO $ outputCharacter my_state
  return ()

writeCharacterM :: StateT ProgramState IO ()
writeCharacterM = do
  my_state <- get
  c <- liftIO getChar
  put $ writeCharacter c my_state
  return ()

jumpProgramM :: StateT ProgramState IO ()
jumpProgramM = do
  my_state <- get
  curr_instr <- getStateInstructionM
  if ((isNull my_state) && curr_instr == JumpForward) || ((not (isNull my_state)) && curr_instr == JumpBackward)
    then put $ jumpProgram my_state
  else put my_state
  return ()
