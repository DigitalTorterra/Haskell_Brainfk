{-# OPTIONS_GHC -Wall #-}
{-
This file contains the functions for executing
the Brainf**k program.

Brainf**k programs have a few different streams to manage:
* STDIN - programs can read from STDIN with `,`
* STDOUT - programs can write to STDOUT with `.`
* Memory - programs can arbitrarily read and write to a
(theoretically) infinite tape of characters.
* Instruction List - programs can step through their own
contents (forward 1 each instruction by default, or otherwise
with brackets)
-}

module Execution where

-- User modules
import Interface

-- Library modules
import Data.Char
import Data.Maybe

-- Helper Functions
safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0 = Nothing
  | otherwise = Just (xs !! n)

updateListElem :: Int -> (a -> a) -> [a] -> [a]
updateListElem n f xs = front ++ val ++ back
  where front = take n xs
        back = drop (n+1) xs
        val = maybeToList . (fmap f) . (safeIndex n) $ xs

updateList :: Int -> a -> [a] -> [a]
updateList idx val xs = updateListElem idx (\_ -> val) xs

updateChar :: Int -> Char -> Char
updateChar n c = chr $ (n + (ord c)) `mod` 256

-- Encapsulation of Program State
data InstructionTape = InstructionTape [Command] Int
data MemoryTape = MemoryTape [Char] Int
type ProgramState = (InstructionTape, MemoryTape)

initializeProgramState :: [Command] -> ProgramState
initializeProgramState instrs = (ins_tape, mem_tape)
  where ins_tape = InstructionTape instrs 0
        mem_tape = MemoryTape (repeat '\0') 0

isFinished :: ProgramState -> Bool
isFinished (InstructionTape commands ptr, _) = ptr >= (length commands)

readInstruction :: InstructionTape -> Maybe Command
readInstruction (InstructionTape xs ptr)
  | ptr < 0 || ptr >= (length xs) = Nothing
  | otherwise = Just $ xs !! ptr

readMemory :: MemoryTape -> Maybe Char
readMemory (MemoryTape xs ptr)
  | ptr < 0 = Nothing
  | otherwise = Just $ xs !! ptr

getStateInstruction :: ProgramState -> Maybe Command
getStateInstruction = readInstruction . fst

updateInstructionPointer :: Int -> InstructionTape -> InstructionTape
updateInstructionPointer amt (InstructionTape xs pc) = InstructionTape xs (pc+amt)

getMemoryPointer :: ProgramState -> Int
getMemoryPointer (_, MemoryTape _ ptr) = ptr

isNull :: ProgramState -> Bool
isNull (_, mem)
  | c == Just '\0' = True
  | otherwise = False
  where c = readMemory mem

-- Operations on Program State
incrementProgramCounter :: ProgramState -> ProgramState
incrementProgramCounter (instrs, mems) = (updateInstructionPointer 1 instrs, mems)

moveDataPointer :: Int -> ProgramState -> ProgramState
moveDataPointer amt (instrs, MemoryTape mems dp) = (instrs, MemoryTape mems (max 0 (dp+amt)))

changeData :: Int -> ProgramState -> ProgramState
changeData amt (instrs, MemoryTape mems dp) = (instrs, MemoryTape mems' dp)
  where mems' = updateListElem dp (updateChar amt) mems

jumpProgram :: ProgramState -> ProgramState
jumpProgram (instrs, mem) = (instrs', mem)
  where curr_symbol = readInstruction instrs
        f :: Int -> Command -> InstructionTape -> InstructionTape
        f levels target cmds
          | new_symbol == Nothing || (new_symbol == Just target && levels == 0) = cmds'
          | new_symbol == Just target_inverse = f (levels+1) target cmds'
          | new_symbol == Just target = f (levels-1) target cmds'
          | otherwise = f levels target cmds'
          where dir = case target of JumpBackward -> 1
                                     JumpForward -> (-1)
                                     _ -> 1
                target_inverse = invertCommand target
                cmds' = updateInstructionPointer dir cmds
                new_symbol = readInstruction cmds'
        instrs' = case curr_symbol of Just x -> f 0 (invertCommand x) instrs
                                      Nothing -> instrs

outputCharacter :: ProgramState -> IO ()
outputCharacter (_, mem) = case (readMemory mem) of Nothing -> return ()
                                                    Just c -> putChar c

writeCharacter :: Char -> ProgramState -> ProgramState
writeCharacter c (instrs, MemoryTape mem ptr) = (instrs, MemoryTape (updateList ptr c mem) ptr)

