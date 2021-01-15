{-# OPTIONS_GHC -Wall #-}
{-
This file contains the data interface used to represent a
P''/Brainf**k file.
-}

module Interface where

import Data.Maybe

data Command = PointerIncr | PointerDecr
             | ByteIncr    | ByteDecr
             | ByteOutput  | ByteInput
             | JumpForward | JumpBackward
             deriving (Show, Eq)

invertCommand :: Command -> Command
invertCommand PointerDecr = PointerIncr
invertCommand PointerIncr = PointerDecr
invertCommand ByteDecr = ByteIncr
invertCommand ByteIncr = ByteDecr
invertCommand ByteInput = ByteOutput
invertCommand ByteOutput = ByteInput
invertCommand JumpBackward = JumpForward
invertCommand JumpForward = JumpBackward


readCommand :: Char -> Maybe Command
readCommand x = case x of '>' -> Just PointerIncr
                          '<' -> Just PointerDecr
                          '+' -> Just ByteIncr
                          '-' -> Just ByteDecr
                          '.' -> Just ByteOutput
                          ',' -> Just ByteInput
                          '[' -> Just JumpForward
                          ']' -> Just JumpBackward
                          _   -> Nothing

readProgram :: String -> [Command]
readProgram = mapMaybe readCommand
